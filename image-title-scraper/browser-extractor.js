/**
 * Image Title Scraper — Browser Extractor v5
 *
 * Purpose:
 *   Discover media on a page (Bing / Google Images / generic sites), score
 *   candidate native titles, then export a JSON manifest for the companion
 *   Python downloader (recommended) or download in-browser.
 *
 * How to use:
 *   1. Open the target page (Bing/Google image results list works best).
 *   2. Open DevTools → Console.
 *   3. Paste this entire file and press Enter.
 *   4. Use the panel: Scan → Export JSON → run: python download.py manifest.json
 *
 * v5 capability upgrades:
 *   - On-page control panel (scan / export / download / rescue gallery)
 *   - Deep discovery: srcset (highest res), lazy data-* attrs, CSS
 *     background-image, open Shadow DOM, HTML/JSON URL regex sweep
 *   - Google Images + Bing `.iusc` metadata fast-paths
 *   - Stronger garbage/hash title filters + min rendered size filter
 *   - Resize-param stripping via URLSearchParams; prefer /large/ variants
 *   - Canvas re-encode fallback when fetch CORS fails
 *   - Bounded-concurrency downloads + delayed blob revoke (.crdownload safe)
 *   - Idempotent: re-running reopens the existing panel
 */
(function () {
  "use strict";

  if (window.__imageTitleScraper && window.__imageTitleScraper.open) {
    window.__imageTitleScraper.open();
    return;
  }

  console.log("🚀 Image Title Scraper v5 — browser extractor");

  // =========================================================================
  // CONFIG
  // =========================================================================
  const CONFIG = {
    scrollDelay: 1200,
    stableThreshold: 3,
    maxScrollSteps: 60,
    downloadDelay: 300,
    batchPauseEvery: 20,
    batchPauseMs: 1500,
    concurrency: 4,
    revokeDelayMs: 90000,
    maxTitleLength: 120,
    minTitleLength: 3,
    minImageSize: 64,
    debug: true,
  };

  const SPAM_URL_KEYWORDS = [
    "analytics",
    "tracker",
    "pixel",
    "doubleclick",
    "googleads",
    "google-analytics",
    "facebook.com/tr",
    "bat.bing",
    "/ads/",
    "adservice",
    "adgeek",
    "beacon",
    "sb.scorecardresearch",
  ];

  const BLACKLIST_TITLES = new Set([
    "image",
    "photo",
    "img",
    "thumbnail",
    "thumb",
    "untitled",
    "click",
    "download",
    "view image",
    "loading",
    "link",
    "logo",
    "icon",
    "avatar",
    "banner",
    "spacer",
    "blank",
    "...",
  ]);

  const IMAGE_EXT = /\.(jpe?g|png|gif|webp|avif|svg|bmp|ico)(?:[?#]|$)/i;
  const VIDEO_EXT = /\.(mp4|webm|mov|m4v|ogv|m3u8|mpd|ts)(?:[?#]|$)/i;

  // =========================================================================
  // UTILS
  // =========================================================================
  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const log = (...args) => {
    if (CONFIG.debug) console.log(...args);
  };

  function sanitize(text) {
    if (!text) return "";
    return String(text)
      .replace(/[\n\r\t]+/g, " ")
      .replace(/[\\/*?:"<>|]/g, "")
      .replace(/\s+/g, " ")
      .trim();
  }

  function normalizeFilename(text) {
    return sanitize(text)
      .replace(/\s+/g, "_")
      .substring(0, CONFIG.maxTitleLength);
  }

  function isGarbage(text) {
    if (!text) return true;
    const t = text.toLowerCase().trim();
    if (t.length < CONFIG.minTitleLength) return true;
    if (t.length > CONFIG.maxTitleLength * 2) return true;
    if (BLACKLIST_TITLES.has(t)) return true;
    if (/^\d+$/.test(t)) return true;
    if (/^[a-f0-9]{16,}$/i.test(t)) return true;
    if (/^(OIP|ODF|thid)\b/i.test(t)) return true;
    return false;
  }

  function createScoringEngine() {
    const candidates = [];

    function add(text, score, source) {
      const cleaned = sanitize(text);
      if (isGarbage(cleaned)) return;
      candidates.push({ text: cleaned, score, source });
    }

    function best() {
      candidates.sort((a, b) => {
        if (b.score !== a.score) return b.score - a.score;
        return b.text.length - a.text.length;
      });
      if (CONFIG.debug && candidates.length) {
        log("🧠 title candidates:", candidates.slice(0, 5));
      }
      return candidates[0]?.text || "";
    }

    return { add, best, candidates };
  }

  function absolutize(url) {
    try {
      if (!url) return "";
      if (url.startsWith("//")) url = location.protocol + url;
      return new URL(url, location.href).href;
    } catch {
      return "";
    }
  }

  function cleanImageUrl(url) {
    if (!url) return "";
    try {
      const u = new URL(url, location.href);
      ["w", "h", "width", "height", "crop", "scale", "resize", "quality", "q"].forEach(
        (p) => u.searchParams.delete(p)
      );
      let cleaned = u.toString();
      cleaned = cleaned.replace(
        /\/(small|thumb|thumbs|th|mini|square)\//gi,
        "/large/"
      );
      return cleaned;
    } catch {
      return url;
    }
  }

  function extractFilename(url) {
    try {
      const pathname = new URL(url, location.href).pathname;
      let file = pathname.split("/").pop() || "";
      file = decodeURIComponent(file);
      file = file.replace(/\.[a-z0-9]+$/i, "");
      file = file.replace(/[-_]+/g, " ");
      if (/^(OIP|ODF|thid)\b/i.test(file) || file.length > 40) return "";
      return sanitize(file);
    } catch {
      return "";
    }
  }

  function bestFromSrcset(srcset) {
    if (!srcset) return "";
    let best = "";
    let bestWidth = -1;
    srcset.split(",").forEach((part) => {
      const [u, d] = part.trim().split(/\s+/);
      if (!u) return;
      const w =
        d && d.endsWith("w")
          ? parseInt(d, 10)
          : d && d.endsWith("x")
            ? parseFloat(d) * 1000
            : 0;
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
    return "image";
  }

  function isSpamUrl(url) {
    const u = String(url).toLowerCase();
    return SPAM_URL_KEYWORDS.some((k) => u.includes(k));
  }

  function extFromMime(mime, fallback = ".jpg") {
    const map = {
      "image/jpeg": ".jpg",
      "image/jpg": ".jpg",
      "image/png": ".png",
      "image/gif": ".gif",
      "image/webp": ".webp",
      "image/avif": ".avif",
      "image/svg+xml": ".svg",
      "image/bmp": ".bmp",
      "video/mp4": ".mp4",
      "video/webm": ".webm",
      "video/quicktime": ".mov",
    };
    return map[mime] || fallback;
  }

  function pickExtension(url, blob, mediaType) {
    if (blob && blob.type) {
      const fromMime = extFromMime(blob.type, "");
      if (fromMime) return fromMime;
    }
    const m = String(url).match(/\.([a-z0-9]{2,4})(?:[?#]|$)/i);
    if (m) {
      const ext = "." + m[1].toLowerCase();
      return ext === ".jpeg" ? ".jpg" : ext;
    }
    return mediaType === "video" ? ".mp4" : ".jpg";
  }

  // Shadow-DOM-aware querySelectorAll
  function deepQueryAll(selector, root = document) {
    const out = [];
    const walk = (node) => {
      try {
        out.push(...node.querySelectorAll(selector));
      } catch {
        /* invalid selector in some roots */
      }
      node.querySelectorAll("*").forEach((el) => {
        if (el.shadowRoot) walk(el.shadowRoot);
      });
    };
    walk(root);
    return out;
  }

  // =========================================================================
  // AUTO SCROLL (lazy-load wake-up)
  // =========================================================================
  async function deepScroll(onProgress) {
    console.log("🔄 Deep-scrolling to wake lazy-loaded media...");
    let lastHeight = document.body.scrollHeight;
    let stableCount = 0;
    let steps = 0;

    while (stableCount < CONFIG.stableThreshold && steps < CONFIG.maxScrollSteps) {
      window.scrollTo({ top: document.body.scrollHeight, behavior: "smooth" });
      await sleep(CONFIG.scrollDelay);
      steps += 1;
      const newHeight = document.body.scrollHeight;
      if (newHeight === lastHeight) {
        stableCount += 1;
      } else {
        stableCount = 0;
        lastHeight = newHeight;
      }
      if (onProgress) onProgress(steps);
    }
    window.scrollTo(0, 0);
    console.log("✅ Page expand complete");
  }

  // =========================================================================
  // SITE DETECTORS
  // =========================================================================
  function detectSite() {
    const host = location.hostname.toLowerCase();
    if (/bing\.com/i.test(host)) return "bing";
    if (/google\./i.test(host) && /\/(search|imghp)/i.test(location.pathname + location.search + location.href)) {
      return "google";
    }
    if (/google\./i.test(host) && /tbm=isch/i.test(location.search)) return "google";
    return "generic";
  }

  // =========================================================================
  // BING CARD PARSER (.iusc metadata is the native title source)
  // =========================================================================
  function parseBingCard(card) {
    try {
      const iusc = card.matches?.(".iusc")
        ? card
        : card.querySelector(".iusc") || card;
      let meta = {};
      const raw = iusc.getAttribute?.("m");
      if (raw) {
        try {
          meta = JSON.parse(raw);
        } catch {
          meta = {};
        }
      }

      const img = card.querySelector("img");
      const imageUrl =
        meta.murl || meta.imgurl || img?.currentSrc || img?.src || "";
      if (!imageUrl || imageUrl.startsWith("data:") || isSpamUrl(imageUrl)) {
        return null;
      }

      const engine = createScoringEngine();
      engine.add(meta.t, 100, "bing.meta.t");
      engine.add(meta.title, 98, "bing.meta.title");
      engine.add(meta.desc, 85, "bing.meta.desc");

      card.querySelectorAll("a[title]").forEach((a) => {
        engine.add(a.getAttribute("title"), 95, "a.title");
      });

      if (img) {
        engine.add(img.alt, 90, "img.alt");
        engine.add(img.title, 88, "img.title");
        engine.add(img.getAttribute("aria-label"), 87, "img.aria-label");
        Object.entries(img.dataset || {}).forEach(([k, v]) => {
          if (/title|caption|alt|name|desc/i.test(k)) {
            engine.add(v, 82, `img.dataset.${k}`);
          }
        });
      }

      const parentLink = img?.closest("a") || card.closest("a");
      if (parentLink) {
        engine.add(parentLink.getAttribute("title"), 93, "parent.a.title");
        engine.add(parentLink.innerText, 70, "parent.a.text");
      }

      engine.add(extractFilename(imageUrl), 60, "url.filename");
      if (meta.purl) engine.add(extractFilename(meta.purl), 40, "bing.purl");

      const best = engine.best();
      return {
        url: cleanImageUrl(imageUrl),
        suggestedName: normalizeFilename(best) || "bing_image",
        thumbnail: img?.src || imageUrl,
        type: "image",
        source: "bing",
        siteName: meta.sitename || "",
        titleSource: engine.candidates[0]?.source || "none",
      };
    } catch (err) {
      log("Bing card parse error:", err);
      return null;
    }
  }

  // =========================================================================
  // GOOGLE IMAGES PARSER
  // =========================================================================
  function parseGoogleCard(node) {
    try {
      // Google often embeds JSON-ish blobs with "ou" (original url) + "pt"/"s" titles
      const html = node.outerHTML || "";
      let imageUrl = "";
      let title = "";

      const ou = html.match(/"ou":"(https?:[^"]+)"/);
      if (ou) imageUrl = ou[1].replace(/\\u003d/g, "=").replace(/\\u0026/g, "&");

      const pt = html.match(/"pt":"([^"]+)"/);
      const s = html.match(/"s":"([^"]+)"/);
      title = (pt && pt[1]) || (s && s[1]) || "";

      const img = node.querySelector?.("img") || (node.tagName === "IMG" ? node : null);
      if (!imageUrl && img) {
        imageUrl =
          img.currentSrc ||
          img.src ||
          bestFromSrcset(img.getAttribute("srcset")) ||
          img.getAttribute("data-src") ||
          "";
      }
      if (!imageUrl || imageUrl.startsWith("data:") || isSpamUrl(imageUrl)) {
        return null;
      }

      const engine = createScoringEngine();
      engine.add(title, 100, "google.meta.pt");
      if (img) {
        engine.add(img.alt, 92, "img.alt");
        engine.add(img.title, 90, "img.title");
        engine.add(img.getAttribute("aria-label"), 88, "img.aria-label");
      }
      const parentLink = img?.closest("a") || node.closest?.("a");
      if (parentLink) {
        engine.add(parentLink.getAttribute("title"), 94, "parent.a.title");
        engine.add(parentLink.getAttribute("aria-label"), 89, "parent.a.aria");
      }
      engine.add(extractFilename(imageUrl), 55, "url.filename");

      const best = engine.best();
      return {
        url: cleanImageUrl(imageUrl),
        suggestedName: normalizeFilename(best) || "google_image",
        thumbnail: img?.src || imageUrl,
        type: "image",
        source: "google",
        siteName: "",
        titleSource: engine.candidates[0]?.source || "none",
      };
    } catch (err) {
      log("Google card parse error:", err);
      return null;
    }
  }

  // =========================================================================
  // GENERIC MEDIA PARSER (multi-fallback title mining)
  // =========================================================================
  function resolveMediaUrl(el) {
    return (
      el.currentSrc ||
      el.src ||
      bestFromSrcset(el.getAttribute("srcset")) ||
      el.getAttribute("data-src") ||
      el.getAttribute("data-original") ||
      el.getAttribute("data-lazy-src") ||
      el.getAttribute("data-url") ||
      el.getAttribute("data-image") ||
      el.getAttribute("data-full") ||
      el.getAttribute("data-large") ||
      ""
    );
  }

  function parseGenericMedia(el) {
    try {
      const tag = el.tagName.toLowerCase();
      const src = resolveMediaUrl(el);
      if (!src || src.startsWith("data:") || src.startsWith("blob:") || isSpamUrl(src)) {
        return null;
      }

      if (
        tag === "img" &&
        el.naturalWidth &&
        el.naturalWidth > 0 &&
        el.naturalWidth < CONFIG.minImageSize
      ) {
        return null;
      }

      const type =
        tag === "video" || tag === "source" ? classifyType(src) : "image";

      const engine = createScoringEngine();

      engine.add(el.alt, 92, "img.alt");
      engine.add(el.title, 90, "img.title");
      engine.add(el.getAttribute("aria-label"), 88, "img.aria-label");
      engine.add(el.getAttribute("data-title"), 94, "img.data-title");
      engine.add(
        el.getAttribute("data-original-title"),
        93,
        "img.data-original-title"
      );
      Object.entries(el.dataset || {}).forEach(([k, v]) => {
        if (/title|caption|alt|name|desc/i.test(k)) {
          engine.add(v, 86, `dataset.${k}`);
        }
      });

      const parentLink = el.closest("a");
      if (parentLink) {
        engine.add(parentLink.getAttribute("title"), 95, "parent.a.title");
        engine.add(parentLink.getAttribute("aria-label"), 89, "parent.a.aria");
        const linkText = sanitize(parentLink.innerText);
        if (linkText.length > CONFIG.minTitleLength) {
          engine.add(linkText, 72, "parent.a.text");
        }
      }

      const figure = el.closest("figure");
      if (figure) {
        const caption = figure.querySelector("figcaption");
        if (caption) engine.add(caption.innerText, 96, "figcaption");
      }

      const container = el.closest(
        'li, article, .imgpt, .dg_u, .infopt, .item, .card, [class*="item"], [class*="card"], [class*="result"]'
      );
      if (container) {
        const infopt = container.querySelector(".infopt a");
        if (infopt) {
          engine.add(infopt.getAttribute("title"), 94, ".infopt a.title");
          engine.add(infopt.innerText, 78, ".infopt a.text");
        }
        container.querySelectorAll("a[title]").forEach((a) => {
          engine.add(a.getAttribute("title"), 91, "container.a.title");
        });
        container
          .querySelectorAll("h1, h2, h3, h4, [class*='title']")
          .forEach((h) => {
            engine.add(h.innerText || h.getAttribute("title"), 80, "container.heading");
          });
      }

      engine.add(extractFilename(src), 55, "url.filename");

      const best = engine.best();
      return {
        url: cleanImageUrl(src),
        suggestedName:
          normalizeFilename(best) || (type === "video" ? "video" : "image"),
        thumbnail: el.currentSrc || el.src || src,
        type,
        source: "generic",
        siteName: "",
        titleSource: engine.candidates[0]?.source || "none",
      };
    } catch (err) {
      log("Generic parse error:", err);
      return null;
    }
  }

  // =========================================================================
  // COLLECT
  // =========================================================================
  function collectResources() {
    const seen = new Set();
    const mediaEntries = [];
    const site = detectSite();

    const push = (parsed) => {
      if (!parsed || !parsed.url) return;
      const abs = absolutize(parsed.url);
      if (!abs || seen.has(abs) || isSpamUrl(abs)) return;
      seen.add(abs);
      parsed.url = abs;
      if (!parsed.suggestedName) {
        parsed.suggestedName =
          parsed.type === "video" ? "video" : "image";
      }
      mediaEntries.push(parsed);
    };

    if (site === "bing") {
      console.log("🟦 Bing Images mode");
      const cards = deepQueryAll(".iusc, .imgpt, .iuscp, .dgControl");
      const nodes = cards.length ? cards : deepQueryAll("[m]");
      console.log(`🔎 Scanning ${nodes.length} Bing cards`);
      for (const card of nodes) push(parseBingCard(card));
    } else if (site === "google") {
      console.log("🟥 Google Images mode");
      // Result tiles + any script/JSON-bearing containers
      const tiles = deepQueryAll(
        "div[data-id], div[jsname], a[href*='/imgres'], img[data-src], img[src]"
      );
      console.log(`🔎 Scanning ${tiles.length} Google nodes`);
      for (const node of tiles) push(parseGoogleCard(node));
    }

    console.log("🌐 Generic / deep discovery pass");
    const mediaEls = deepQueryAll(
      "img, video, source, [data-src], [data-original], [data-lazy-src], [data-url], [data-image]"
    );
    console.log(`🔎 Scanning ${mediaEls.length} media / lazy nodes`);
    for (const el of mediaEls) push(parseGenericMedia(el));

    // CSS background-image
    deepQueryAll("*").forEach((el) => {
      const bg = el.style && el.style.backgroundImage;
      if (!bg || bg === "none") return;
      const m = bg.match(/url\(['"]?(.*?)['"]?\)/i);
      if (!m || !m[1] || m[1].startsWith("data:")) return;
      const url = cleanImageUrl(m[1]);
      if (!url || isSpamUrl(url)) return;
      const title = extractFilename(url);
      push({
        url,
        suggestedName: normalizeFilename(title) || "bg_image",
        thumbnail: url,
        type: classifyType(url),
        source: "css-bg",
        siteName: "",
        titleSource: title ? "url.filename" : "none",
      });
    });

    // Brute-force regex sweep of raw HTML for hidden media URLs
    const html = document.documentElement.innerHTML;
    const rx =
      /(?:https?:)?\/\/[^\s"'<>()\[\]{}]+?\.(?:jpe?g|png|gif|webp|avif|svg|mp4|webm|mov|m3u8|ts)(?:\?[^\s"'<>()\[\]{}]*)?/gi;
    (html.match(rx) || []).forEach((u) => {
      u = u.replace(/[\\'",;]+$/, "");
      if (isSpamUrl(u)) return;
      const title = extractFilename(u);
      push({
        url: cleanImageUrl(u),
        suggestedName: normalizeFilename(title) || "media",
        thumbnail: u,
        type: classifyType(u),
        source: "html-sweep",
        siteName: "",
        titleSource: title ? "url.filename" : "none",
      });
    });

    return mediaEntries;
  }

  // =========================================================================
  // MANIFEST
  // =========================================================================
  function buildManifest(mediaEntries) {
    return {
      version: 5,
      generatedAt: new Date().toISOString(),
      pageUrl: location.href,
      pageTitle: document.title,
      count: mediaEntries.length,
      items: mediaEntries.map((e, i) => ({
        index: i + 1,
        url: e.url,
        title: e.suggestedName,
        type: e.type,
        titleSource: e.titleSource,
        source: e.source,
        siteName: e.siteName || "",
        thumbnail: e.thumbnail || "",
      })),
    };
  }

  function downloadTextFile(text, filename, mime) {
    const blob = new Blob([text], { type: mime || "text/plain" });
    const blobUrl = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = blobUrl;
    a.download = filename;
    a.style.display = "none";
    document.body.appendChild(a);
    a.click();
    setTimeout(() => {
      a.remove();
      URL.revokeObjectURL(blobUrl);
    }, 2000);
  }

  async function exportManifest(mediaEntries) {
    const manifest = buildManifest(mediaEntries);
    const jsonText = JSON.stringify(manifest, null, 2);
    window.__IMAGE_TITLE_MANIFEST__ = manifest;

    let copied = false;
    try {
      await navigator.clipboard.writeText(jsonText);
      copied = true;
      console.log("📋 Manifest copied to clipboard");
    } catch {
      /* fall through to file download */
    }

    downloadTextFile(
      jsonText,
      `image-title-manifest_${Date.now()}.json`,
      "application/json"
    );
    console.log("💾 Manifest JSON file download triggered");
    console.log(
      "➡️ Next: save as manifest.json, then run:\n" +
        "   python download.py manifest.json"
    );
    console.log("📦 Also available as window.__IMAGE_TITLE_MANIFEST__");
    return { copied, manifest };
  }

  // =========================================================================
  // IN-BROWSER DOWNLOAD
  // =========================================================================
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
      if (document.body.contains(a)) a.remove();
      URL.revokeObjectURL(blobUrl);
    }, CONFIG.revokeDelayMs);
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

  async function forceDownload(entry, index) {
    const base =
      `${String(index).padStart(3, "0")}_` +
      (entry.suggestedName || "image").substring(0, 80);

    try {
      const resp = await fetch(entry.url, { mode: "cors", credentials: "omit" });
      if (resp.ok) {
        const blob = await resp.blob();
        const ext = pickExtension(entry.url, blob, entry.type);
        if (triggerBlobDownload(blob, `${base}${ext}`)) {
          console.log(`✅ [blob] ${base}${ext}`);
          return "ok";
        }
      }
    } catch (err) {
      log(`fetch failed for ${entry.url}:`, err.message || err);
    }

    if (entry.type === "image") {
      const blob = await drawToCanvas(entry.url);
      if (blob && triggerBlobDownload(blob, `${base}.png`)) {
        console.log(`✅ [canvas] ${base}.png`);
        return "ok";
      }
    }

    console.warn(`⚠️ [cors] cannot download: ${entry.url}`);
    return "rescue";
  }

  async function downloadAll(mediaEntries, onProgress) {
    const total = mediaEntries.length;
    const failed = [];
    let ok = 0;
    let done = 0;
    let cursor = 0;

    const worker = async () => {
      while (cursor < total) {
        const i = cursor++;
        const result = await forceDownload(mediaEntries[i], i + 1);
        if (result === "ok") ok += 1;
        else failed.push(mediaEntries[i]);
        done += 1;
        if (onProgress) onProgress(done, total, ok, failed.length);
        if (done % CONFIG.batchPauseEvery === 0) await sleep(CONFIG.batchPauseMs);
        else await sleep(CONFIG.downloadDelay);
      }
    };

    await Promise.all(
      Array.from({ length: Math.min(CONFIG.concurrency, total) }, () => worker())
    );

    console.log(`🎉 Browser download done. ok=${ok} failed=${failed.length}`);
    return { ok, failed };
  }

  function renderRescueGallery(failed, container) {
    container.innerHTML = "";
    if (!failed.length) {
      container.style.display = "none";
      return;
    }
    container.style.display = "flex";
    container.style.flexDirection = "column";
    container.style.gap = "8px";

    const header = document.createElement("div");
    header.style.cssText = "font-weight:600;font-size:12px;";
    header.textContent = `CORS-blocked (${failed.length}) — open manually or use Python downloader`;
    container.appendChild(header);

    failed.forEach((entry) => {
      const row = document.createElement("a");
      row.href = entry.url;
      row.target = "_blank";
      row.rel = "noopener";
      row.title = entry.url;
      row.style.cssText =
        "display:flex;align-items:center;gap:8px;text-decoration:none;color:#1a5fb4;background:#f1f3f5;padding:6px;border-radius:6px;";
      const thumb =
        entry.type === "video"
          ? document.createElement("video")
          : document.createElement("img");
      thumb.src = entry.thumbnail || entry.url;
      thumb.style.cssText =
        "width:44px;height:44px;object-fit:cover;border-radius:4px;background:#ddd;flex:0 0 auto;";
      const label = document.createElement("span");
      label.style.cssText =
        "font-size:11px;word-break:break-all;overflow:hidden;max-height:44px;color:#222;";
      label.textContent = entry.suggestedName || entry.url.split("/").pop();
      row.append(thumb, label);
      container.appendChild(row);
    });
  }

  // =========================================================================
  // UI CONTROL PANEL
  // =========================================================================
  const ui = (() => {
    const panel = document.createElement("div");
    panel.id = "image-title-scraper-panel";
    panel.style.cssText =
      "position:fixed;top:16px;right:16px;width:360px;max-height:88vh;z-index:2147483647;" +
      "background:#f8f9fa;color:#1e1e1e;font:13px/1.5 Georgia, 'Times New Roman', serif;" +
      "border:1px solid #c5c9ce;border-radius:10px;box-shadow:0 8px 28px rgba(0,0,0,.18);" +
      "overflow:hidden;display:flex;flex-direction:column;";

    panel.innerHTML = `
      <div style="display:flex;align-items:center;justify-content:space-between;padding:12px 14px;background:#e9ecef;border-bottom:1px solid #ced4da;">
        <strong style="font-size:14px;font-family:system-ui,sans-serif;">Image Title Scraper v5</strong>
        <span data-close style="cursor:pointer;font-size:18px;line-height:1;opacity:.7;font-family:system-ui,sans-serif;">&times;</span>
      </div>
      <div style="padding:14px;display:flex;flex-direction:column;gap:10px;overflow-y:auto;font-family:system-ui,Segoe UI,sans-serif;">
        <label style="display:flex;align-items:center;gap:8px;cursor:pointer;font-size:12px;">
          <input type="checkbox" data-autoscroll checked> Auto-scroll first (lazy media)
        </label>
        <button data-scan style="padding:9px;border:0;border-radius:8px;background:#0b57d0;color:#fff;font-weight:600;cursor:pointer;">Scan page</button>
        <div data-summary style="font-size:12px;opacity:.9;min-height:18px;"></div>
        <div style="display:flex;gap:8px;">
          <button data-export disabled style="flex:1;padding:9px;border:0;border-radius:8px;background:#0f7b4c;color:#fff;font-weight:600;cursor:pointer;">Export JSON</button>
          <button data-download disabled style="flex:1;padding:9px;border:0;border-radius:8px;background:#5f6368;color:#fff;font-weight:600;cursor:pointer;">Download</button>
        </div>
        <div data-progress style="height:8px;border-radius:4px;background:#dee2e6;overflow:hidden;display:none;">
          <div data-bar style="height:100%;width:0;background:#0f7b4c;transition:width .2s;"></div>
        </div>
        <div data-status style="font-size:12px;opacity:.85;"></div>
        <div data-gallery style="display:none;"></div>
        <div style="font-size:11px;opacity:.7;line-height:1.4;">
          Tip: Export JSON → <code>python download.py manifest.json</code> avoids CORS / .crdownload issues.
        </div>
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
    const setBusy = (busy) => {
      $("[data-scan]").disabled = busy;
      $("[data-export]").disabled = busy || state.entries.length === 0;
      $("[data-download]").disabled = busy || state.entries.length === 0;
    };

    $("[data-close]").onclick = () => {
      panel.style.display = "none";
    };

    $("[data-scan]").onclick = async () => {
      setBusy(true);
      $("[data-gallery]").style.display = "none";
      $("[data-gallery]").innerHTML = "";
      $("[data-progress]").style.display = "none";
      setStatus("");
      try {
        if ($("[data-autoscroll]").checked) {
          setSummary("Scrolling to load lazy content...");
          await deepScroll((s) => setSummary(`Scrolling… step ${s}`));
        }
        setSummary("Scanning DOM, shadow roots & HTML…");
        state.entries = collectResources();
        const imgs = state.entries.filter((e) => e.type === "image").length;
        const vids = state.entries.filter((e) => e.type === "video").length;
        setSummary(
          `Found ${state.entries.length} items — ${imgs} images, ${vids} videos (${detectSite()}).`
        );
        console.table(
          state.entries.map((x) => ({
            title: x.suggestedName,
            via: x.titleSource,
            type: x.type,
            source: x.source,
            url: x.url.substring(0, 72),
          }))
        );
        console.log(`✅ Collected ${state.entries.length} media items`);
        if (!state.entries.length) {
          setStatus("No media found. Try the results list page (not the detail viewer).");
        }
      } catch (err) {
        console.error(err);
        setStatus(`Scan failed: ${err.message || err}`);
      } finally {
        setBusy(false);
      }
    };

    $("[data-export]").onclick = async () => {
      if (!state.entries.length) return;
      setBusy(true);
      try {
        const { copied } = await exportManifest(state.entries);
        setStatus(
          copied
            ? "Manifest copied + JSON file downloaded. Run download.py next."
            : "Manifest JSON file downloaded. Run download.py next."
        );
      } finally {
        setBusy(false);
      }
    };

    $("[data-download]").onclick = async () => {
      if (!state.entries.length) return;
      console.warn(
        "🔔 Allow multiple automatic downloads if Chrome blocks them."
      );
      console.warn(
        '🔔 Turn OFF “Ask where to save each file” in Chrome download settings.'
      );
      setBusy(true);
      try {
        const { ok, failed } = await downloadAll(
          state.entries,
          (done, total, okCount, failCount) => {
            setProgress(done, total);
            setStatus(
              `Downloaded ${okCount} / ${total} (${failCount} need manual / Python)`
            );
          }
        );
        setStatus(`Done. ${ok} downloaded, ${failed.length} blocked by CORS.`);
        renderRescueGallery(failed, $("[data-gallery]"));
      } finally {
        setBusy(false);
      }
    };

    return {
      open() {
        panel.style.display = "flex";
      },
      panel,
      getEntries: () => state.entries,
    };
  })();

  window.__imageTitleScraper = ui;
  console.log(
    "%c🖼️ Image Title Scraper v5 ready — use the panel (top-right).",
    "color:#0b57d0;font-weight:bold;"
  );
})();
