/*
 * Universal Media Extractor (v6)
 * ------------------------------------------------------------------
 * A single-file browser-console utility that discovers and downloads
 * every image / video resource on the current web page.
 *
 * HOW TO USE
 *   1. Open the target page in Chrome / Edge / Firefox.
 *   2. Open DevTools (F12) -> Console.
 *   3. Paste the entire contents of this file and press Enter.
 *   4. Use the on-page control panel to scan and download.
 *
 * This is the consolidated successor to the iterative scripts developed
 * in the source conversation. It reunifies every good idea that earlier
 * revisions gained and then lost:
 *
 *   - Smart, ranked filename inference (the "scoring engine") so files
 *     keep meaningful names instead of Asset_0001.jpg.
 *   - Deep discovery: <img>/<video>/<source>, srcset (highest res),
 *     lazy-load data-* attributes, CSS background-image, shadow DOM,
 *     and a brute-force regex sweep of the raw HTML/JSON for hidden URLs.
 *   - Bing Image Search fast-path via the .iusc `m` metadata JSON.
 *   - Zero-tab, CORS-aware downloader (fetch -> canvas re-encode fallback)
 *     that never spawns popup tabs.
 *   - .crdownload mitigation: delayed object-URL revocation, bounded
 *     concurrency, and batch cool-downs.
 *   - A live progress panel plus a manual "rescue gallery" for resources
 *     that cannot be fetched silently (hard CORS), and an optional URL
 *     manifest export for external download managers.
 *
 * No dependencies. Works as a plain paste-in script or a bookmarklet.
 */

(function () {
  "use strict";

  if (window.__universalMediaExtractor && window.__universalMediaExtractor.open) {
    window.__universalMediaExtractor.open();
    return;
  }

  // ================================================================
  // Configuration
  // ================================================================
  const CONFIG = {
    scrollDelay: 1200,       // ms between auto-scroll steps
    stableThreshold: 3,      // consecutive stable heights => done scrolling
    maxScrollSteps: 60,      // hard cap so we never loop forever
    concurrency: 4,          // simultaneous downloads (bounded to avoid IO thrash)
    downloadDelay: 300,      // ms between individual download starts
    batchSize: 20,           // cool down after this many downloads
    batchDelay: 1500,        // ms cool-down between batches
    revokeDelay: 90000,      // ms before revoking a blob URL (prevents .crdownload)
    minImageSize: 64,        // ignore rendered images smaller than this (icons/pixels)
    maxTitleLength: 120,
    debug: true,
  };

  const SPAM_KEYWORDS = [
    "analytics", "tracker", "pixel", "doubleclick", "googleads",
    "google-analytics", "facebook.com/tr", "bat.bing", "/ads/", "adservice",
    "beacon", "sb.scorecardresearch",
  ];

  const BLACKLIST_TITLES = [
    "image", "photo", "img", "thumbnail", "thumb", "untitled", "click",
    "download", "view image", "loading", "logo", "icon", "avatar", "banner",
    "spacer", "blank",
  ];

  const IMAGE_EXT = /\.(jpe?g|png|gif|webp|avif|svg|bmp|ico)(?:[?#]|$)/i;
  const VIDEO_EXT = /\.(mp4|webm|mov|m4v|ogv|m3u8|mpd|ts)(?:[?#]|$)/i;
  const MIME_EXT = {
    "image/jpeg": ".jpg", "image/png": ".png", "image/gif": ".gif",
    "image/webp": ".webp", "image/avif": ".avif", "image/svg+xml": ".svg",
    "image/bmp": ".bmp", "image/x-icon": ".ico",
    "video/mp4": ".mp4", "video/webm": ".webm", "video/quicktime": ".mov",
    "video/ogg": ".ogv",
  };

  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const log = (...a) => CONFIG.debug && console.log(...a);

  // ================================================================
  // String / URL helpers
  // ================================================================
  function sanitize(text) {
    if (!text) return "";
    return String(text)
      .replace(/[\n\r\t]+/g, " ")
      .replace(/[\\/*?:"<>|]/g, "")
      .replace(/\s+/g, " ")
      .trim();
  }

  function normalizeFilename(text) {
    const name = sanitize(text).replace(/\s+/g, "_");
    return name ? name.substring(0, CONFIG.maxTitleLength) : "";
  }

  function isGarbageTitle(text) {
    if (!text) return true;
    const t = text.toLowerCase().trim();
    if (t.length < 3) return true;
    if (t.length > CONFIG.maxTitleLength * 2) return true;
    if (BLACKLIST_TITLES.includes(t)) return true;
    if (/^\d+$/.test(t)) return true;
    if (/^[a-f0-9]{16,}$/i.test(t)) return true; // hash-like
    return false;
  }

  function absolutize(url) {
    try {
      if (url.startsWith("//")) url = location.protocol + url;
      return new URL(url, location.href).href;
    } catch {
      return "";
    }
  }

  // Strip resize params and prefer the largest variant of a URL.
  function cleanImageUrl(url) {
    if (!url) return "";
    try {
      const u = new URL(url, location.href);
      ["w", "h", "width", "height", "crop", "scale", "resize", "quality", "q"]
        .forEach((p) => u.searchParams.delete(p));
      return u.toString().replace(/\/(small|thumb|thumbs|th|mini|square)\//gi, "/large/");
    } catch {
      return url;
    }
  }

  function extractFilenameTitle(url) {
    try {
      const file = new URL(url, location.href).pathname.split("/").pop();
      if (!file) return "";
      return sanitize(
        decodeURIComponent(file).replace(/\.[a-z0-9]+$/i, "").replace(/[-_]+/g, " ")
      );
    } catch {
      return "";
    }
  }

  // Pick the highest-resolution candidate from a srcset string.
  function bestFromSrcset(srcset) {
    if (!srcset) return "";
    let best = "";
    let bestWidth = -1;
    srcset.split(",").forEach((part) => {
      const [u, d] = part.trim().split(/\s+/);
      if (!u) return;
      const w = d && d.endsWith("w") ? parseInt(d) : d && d.endsWith("x") ? parseFloat(d) * 1000 : 0;
      if (w >= bestWidth) {
        bestWidth = w;
        best = u;
      }
    });
    return best;
  }

  function classifyType(url) {
    if (VIDEO_EXT.test(url)) return "video";
    if (IMAGE_EXT.test(url)) return "image";
    return "image"; // default assumption for extension-less CDN URLs
  }

  function isSpam(url) {
    return SPAM_KEYWORDS.some((k) => url.toLowerCase().includes(k));
  }

  // ================================================================
  // Title scoring engine
  // ================================================================
  function createScoringEngine() {
    const candidates = [];
    return {
      add(text, score, source) {
        const cleaned = sanitize(text);
        if (cleaned && !isGarbageTitle(cleaned)) {
          candidates.push({ text: cleaned, score, source });
        }
      },
      best() {
        if (!candidates.length) return "";
        candidates.sort((a, b) =>
          b.score !== a.score ? b.score - a.score : b.text.length - a.text.length
        );
        return candidates[0].text;
      },
    };
  }

  // ================================================================
  // Shadow-DOM-aware querySelectorAll
  // ================================================================
  function deepQueryAll(selector, root = document) {
    const out = [];
    const walk = (node) => {
      out.push(...node.querySelectorAll(selector));
      node.querySelectorAll("*").forEach((el) => {
        if (el.shadowRoot) walk(el.shadowRoot);
      });
    };
    walk(root);
    return out;
  }

  // ================================================================
  // Auto-scroll to trigger lazy loading
  // ================================================================
  async function autoScroll(onProgress) {
    let lastHeight = document.body.scrollHeight;
    let stable = 0;
    let steps = 0;
    while (stable < CONFIG.stableThreshold && steps < CONFIG.maxScrollSteps) {
      window.scrollTo(0, document.body.scrollHeight);
      await sleep(CONFIG.scrollDelay);
      steps++;
      const h = document.body.scrollHeight;
      if (h === lastHeight) stable++;
      else {
        stable = 0;
        lastHeight = h;
      }
      onProgress && onProgress(steps);
    }
    window.scrollTo(0, 0);
  }

  // ================================================================
  // Discovery
  // ================================================================
  function parseBingCard(card, addEntry) {
    const iusc = card.querySelector(".iusc");
    let meta = {};
    if (iusc) {
      try {
        meta = JSON.parse(iusc.getAttribute("m") || "{}");
      } catch {}
    }
    const rawUrl = meta.murl || meta.imgurl || card.querySelector("img")?.src;
    if (!rawUrl) return;

    const engine = createScoringEngine();
    engine.add(meta.t, 100, "bing.t");
    engine.add(meta.desc, 96, "bing.desc");
    card.querySelectorAll("a[title]").forEach((a) =>
      engine.add(a.getAttribute("title"), 95, "a.title")
    );
    const img = card.querySelector("img");
    if (img) {
      engine.add(img.alt, 90, "img.alt");
      engine.add(img.title, 88, "img.title");
    }
    engine.add(extractFilenameTitle(rawUrl), 70, "url");
    engine.add(meta.purl && extractFilenameTitle(meta.purl), 40, "purl");

    addEntry({
      url: cleanImageUrl(rawUrl),
      title: engine.best(),
      type: "image",
      thumbnail: card.querySelector("img")?.src || "",
      source: meta.sitename || "",
    });
  }

  function parseGenericMedia(el, addEntry) {
    const tag = el.tagName.toLowerCase();
    let raw =
      el.currentSrc ||
      el.src ||
      bestFromSrcset(el.getAttribute("srcset")) ||
      el.getAttribute("data-src") ||
      el.getAttribute("data-original") ||
      el.getAttribute("data-lazy-src") ||
      el.getAttribute("data-url") ||
      "";
    if (!raw || raw.startsWith("data:") || raw.startsWith("blob:")) return;
    if (isSpam(raw)) return;

    // Skip tiny rendered images (icons / tracking pixels).
    if (tag === "img" && el.naturalWidth && el.naturalWidth < CONFIG.minImageSize) return;

    const type = tag === "video" || tag === "source" ? classifyType(raw) : "image";
    const engine = createScoringEngine();
    engine.add(el.alt, 92, "alt");
    engine.add(el.title, 90, "title");
    engine.add(el.getAttribute("aria-label"), 88, "aria-label");
    Object.entries(el.dataset || {}).forEach(([k, v]) => engine.add(v, 80, `data.${k}`));

    const link = el.closest("a");
    if (link) {
      engine.add(link.title, 86, "a.title");
      engine.add(link.getAttribute("aria-label"), 84, "a.aria");
      engine.add(link.innerText, 68, "a.text");
    }
    const figure = el.closest("figure");
    if (figure) engine.add(figure.querySelector("figcaption")?.innerText, 94, "figcaption");

    const container = el.closest("article, li, .card, .item, [class*=item], [class*=card]");
    if (container) {
      container.querySelectorAll("h1,h2,h3,h4").forEach((h) => engine.add(h.innerText, 78, "heading"));
    }
    engine.add(extractFilenameTitle(raw), 70, "url");

    addEntry({
      url: cleanImageUrl(raw),
      title: engine.best(),
      type,
      thumbnail: el.currentSrc || el.src || raw,
      source: "",
    });
  }

  function collectResources() {
    const seen = new Set();
    const entries = [];
    const addEntry = (entry) => {
      if (!entry.url) return;
      const abs = absolutize(entry.url);
      if (!abs || seen.has(abs)) return;
      seen.add(abs);
      entry.url = abs;
      entry.title = normalizeFilename(entry.title);
      entries.push(entry);
    };

    // Fast-path: Bing Image Search cards.
    if (location.hostname.includes("bing.com")) {
      deepQueryAll(".imgpt, .iuscp, .dgControl").forEach((c) => parseBingCard(c, addEntry));
    }

    // Standard + lazy DOM media (shadow-DOM aware).
    deepQueryAll("img, video, source, [data-src], [data-original], [data-lazy-src]").forEach(
      (el) => parseGenericMedia(el, addEntry)
    );

    // CSS background-image on any element.
    deepQueryAll("*").forEach((el) => {
      const bg = el.style && el.style.backgroundImage;
      if (bg) {
        const m = bg.match(/url\(['"]?(.*?)['"]?\)/);
        if (m && m[1] && !m[1].startsWith("data:")) {
          addEntry({ url: cleanImageUrl(m[1]), title: extractFilenameTitle(m[1]), type: classifyType(m[1]) });
        }
      }
    });

    // Brute-force regex sweep of raw HTML for URLs hidden in JS/JSON.
    const html = document.documentElement.innerHTML;
    const rx = /(?:https?:)?\/\/[^\s"'<>()\[\]{}]+?\.(?:jpe?g|png|gif|webp|avif|svg|mp4|webm|mov|m3u8|ts)(?:\?[^\s"'<>()\[\]{}]*)?/gi;
    (html.match(rx) || []).forEach((u) => {
      u = u.replace(/[\\'",;]+$/, "");
      if (isSpam(u)) return;
      addEntry({ url: cleanImageUrl(u), title: extractFilenameTitle(u), type: classifyType(u) });
    });

    return entries;
  }

  // ================================================================
  // Zero-tab, CORS-aware, .crdownload-safe downloader
  // ================================================================
  function triggerBlobDownload(blob, filename) {
    if (!blob || blob.size === 0) return false;
    const blobUrl = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = blobUrl;
    a.download = filename;
    a.style.display = "none";
    document.body.appendChild(a);
    requestAnimationFrame(() => a.click());
    setTimeout(() => {
      if (document.body.contains(a)) document.body.removeChild(a);
      URL.revokeObjectURL(blobUrl);
    }, CONFIG.revokeDelay);
    return true;
  }

  function drawToCanvas(url) {
    return new Promise((resolve) => {
      const img = new Image();
      img.crossOrigin = "Anonymous";
      img.onload = () => {
        try {
          const canvas = document.createElement("canvas");
          canvas.width = img.naturalWidth;
          canvas.height = img.naturalHeight;
          canvas.getContext("2d").drawImage(img, 0, 0);
          canvas.toBlob((b) => resolve(b), "image/png");
        } catch {
          resolve(null);
        }
      };
      img.onerror = () => resolve(null);
      img.src = url;
    });
  }

  function pickExtension(url, blob) {
    if (blob && MIME_EXT[blob.type]) return MIME_EXT[blob.type];
    const m = url.match(/\.([a-z0-9]{2,4})(?:[?#]|$)/i);
    if (m) return "." + m[1].toLowerCase();
    return classifyType(url) === "video" ? ".mp4" : ".jpg";
  }

  // Returns "ok" | "rescue" (needs manual save).
  async function downloadEntry(entry, index) {
    const base = entry.title || (entry.type === "video" ? "video" : "image");
    const stem = `${base}_${String(index).padStart(4, "0")}`;

    // Strategy A: fetch as blob (preserves original bytes / format).
    try {
      const resp = await fetch(entry.url, { mode: "cors", credentials: "omit" });
      if (resp.ok) {
        const blob = await resp.blob();
        if (triggerBlobDownload(blob, stem + pickExtension(entry.url, blob))) return "ok";
      }
    } catch {}

    // Strategy B: canvas re-encode (works when server allows anonymous <img> but not fetch).
    if (entry.type === "image") {
      const blob = await drawToCanvas(entry.url);
      if (blob && triggerBlobDownload(blob, stem + ".png")) return "ok";
    }

    // Strategy C: give up silently (never open a popup tab).
    return "rescue";
  }

  // ================================================================
  // URL manifest export (for external download managers)
  // ================================================================
  function exportManifest(entries) {
    const text = entries
      .map((e) => `${e.url}\t${e.title || ""}\t${e.type}`)
      .join("\n");
    const blob = new Blob([`# url\ttitle\ttype\n${text}`], { type: "text/plain" });
    triggerBlobDownload(blob, "media_manifest.txt");
  }

  // ================================================================
  // UI control panel
  // ================================================================
  const ui = (() => {
    const panel = document.createElement("div");
    panel.style.cssText =
      "position:fixed;top:16px;right:16px;width:340px;max-height:88vh;z-index:2147483647;" +
      "background:#1e1f22;color:#e6e6e6;font:13px/1.5 -apple-system,Segoe UI,Roboto,sans-serif;" +
      "border-radius:12px;box-shadow:0 10px 40px rgba(0,0,0,.5);overflow:hidden;display:flex;flex-direction:column;";

    panel.innerHTML = `
      <div style="display:flex;align-items:center;justify-content:space-between;padding:12px 14px;background:#2b2d31;">
        <strong style="font-size:14px;">Universal Media Extractor</strong>
        <span data-close style="cursor:pointer;font-size:18px;line-height:1;opacity:.7;">&times;</span>
      </div>
      <div style="padding:14px;display:flex;flex-direction:column;gap:10px;overflow-y:auto;">
        <label style="display:flex;align-items:center;gap:8px;cursor:pointer;">
          <input type="checkbox" data-autoscroll checked> Auto-scroll first (loads lazy media)
        </label>
        <button data-scan style="padding:9px;border:0;border-radius:8px;background:#5865f2;color:#fff;font-weight:600;cursor:pointer;">Scan page</button>
        <div data-summary style="font-size:12px;opacity:.85;min-height:18px;"></div>
        <div style="display:flex;gap:8px;">
          <button data-download disabled style="flex:1;padding:9px;border:0;border-radius:8px;background:#248046;color:#fff;font-weight:600;cursor:pointer;">Download all</button>
          <button data-manifest disabled style="padding:9px;border:0;border-radius:8px;background:#4e5058;color:#fff;cursor:pointer;">Export URLs</button>
        </div>
        <div data-progress style="height:8px;border-radius:4px;background:#3a3c42;overflow:hidden;display:none;">
          <div data-bar style="height:100%;width:0;background:#248046;transition:width .2s;"></div>
        </div>
        <div data-status style="font-size:12px;opacity:.8;"></div>
        <div data-gallery style="display:none;flex-direction:column;gap:8px;"></div>
      </div>`;

    document.body.appendChild(panel);

    const $ = (sel) => panel.querySelector(sel);
    const state = { entries: [] };

    const setSummary = (t) => ($("[data-summary]").textContent = t);
    const setStatus = (t) => ($("[data-status]").textContent = t);
    const setProgress = (done, total) => {
      $("[data-progress]").style.display = "block";
      $("[data-bar]").style.width = total ? `${(done / total) * 100}%` : "0";
    };

    $("[data-close]").onclick = () => panel.remove();

    $("[data-scan]").onclick = async () => {
      $("[data-scan]").disabled = true;
      $("[data-gallery]").style.display = "none";
      $("[data-gallery]").innerHTML = "";
      setStatus("");
      if ($("[data-autoscroll]").checked) {
        setSummary("Scrolling to load lazy content...");
        await autoScroll((s) => setSummary(`Scrolling... (step ${s})`));
      }
      setSummary("Scanning DOM & source...");
      state.entries = collectResources();
      const imgs = state.entries.filter((e) => e.type === "image").length;
      const vids = state.entries.filter((e) => e.type === "video").length;
      setSummary(`Found ${state.entries.length} resources — ${imgs} images, ${vids} videos.`);
      $("[data-scan]").disabled = false;
      $("[data-download]").disabled = state.entries.length === 0;
      $("[data-manifest]").disabled = state.entries.length === 0;
      log("Media entries:", state.entries);
      if (console.table) console.table(state.entries.map((e) => ({ title: e.title, type: e.type, url: e.url.slice(0, 90) })));
    };

    $("[data-manifest]").onclick = () => exportManifest(state.entries);

    $("[data-download]").onclick = async () => {
      const total = state.entries.length;
      if (!total) return;
      $("[data-download]").disabled = true;
      $("[data-scan]").disabled = true;
      const rescue = [];
      let done = 0;
      let ok = 0;

      // Bounded-concurrency worker pool.
      let cursor = 0;
      const worker = async () => {
        while (cursor < total) {
          const i = cursor++;
          const entry = state.entries[i];
          const result = await downloadEntry(entry, i + 1);
          if (result === "ok") ok++;
          else rescue.push(entry);
          done++;
          setProgress(done, total);
          setStatus(`Downloaded ${ok} / ${total} (${rescue.length} need manual save)`);
          if (done % CONFIG.batchSize === 0) await sleep(CONFIG.batchDelay);
          else await sleep(CONFIG.downloadDelay);
        }
      };
      await Promise.all(
        Array.from({ length: Math.min(CONFIG.concurrency, total) }, worker)
      );

      setStatus(`Done. ${ok} downloaded, ${rescue.length} blocked by CORS.`);
      $("[data-scan]").disabled = false;
      $("[data-download]").disabled = false;
      if (rescue.length) renderGallery(rescue);
    };

    function renderGallery(items) {
      const g = $("[data-gallery]");
      g.style.display = "flex";
      const header = document.createElement("div");
      header.style.cssText = "font-weight:600;font-size:12px;";
      header.textContent = `Manual rescue (${items.length}) — right-click to save:`;
      g.appendChild(header);
      items.forEach((entry) => {
        const row = document.createElement("a");
        row.href = entry.url;
        row.target = "_blank";
        row.rel = "noopener";
        row.title = entry.url;
        row.style.cssText =
          "display:flex;align-items:center;gap:8px;text-decoration:none;color:#9dbcff;background:#2b2d31;padding:6px;border-radius:6px;";
        const thumb = document.createElement("img");
        thumb.src = entry.thumbnail || entry.url;
        thumb.style.cssText = "width:44px;height:44px;object-fit:cover;border-radius:4px;background:#111;flex:0 0 auto;";
        const label = document.createElement("span");
        label.style.cssText = "font-size:11px;word-break:break-all;overflow:hidden;max-height:44px;";
        label.textContent = entry.title || entry.url.split("/").pop();
        row.appendChild(thumb);
        row.appendChild(label);
        g.appendChild(row);
      });
    }

    return { open: () => (panel.style.display = "flex") };
  })();

  window.__universalMediaExtractor = ui;
  console.log("%c🖼️ Universal Media Extractor ready — use the panel (top-right).", "color:#5865f2;font-weight:bold;");
})();
