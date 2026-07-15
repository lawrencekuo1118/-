/**
 * Image Title Scraper — Browser Extractor v5
 *
 * Purpose:
 *   Scroll a page (Bing Images or generic sites), discover media, score
 *   candidate native titles, then either download in-browser or export a
 *   JSON manifest for the companion Python downloader (recommended).
 *
 * How to use:
 *   1. Open the target page (Bing Images results list works best).
 *   2. Open DevTools → Console.
 *   3. Paste this entire file and press Enter.
 *   4. When prompted, prefer "Export JSON" then run: python download.py manifest.json
 *
 * Upgrades in v5:
 *   - Multi-source title scoring with length / blacklist filters
 *   - Bing `.iusc` metadata (`m` JSON: murl + t) as highest-confidence source
 *   - Parent <a title>, card containers, figcaption, data-*, img alt/title fallbacks
 *   - srcset / picture source prioritization for higher-resolution media URLs
 *   - Optional CSS background-image discovery for card-based layouts
 *   - URL canonicalization + tracker query stripping to improve dedupe accuracy
 *   - Manifest export to avoid Chrome .crdownload / CORS download failures
 *   - Batched download delays + auto-download permission reminder
 */
(async function () {
  "use strict";

  console.clear();
  console.log("🚀 Image Title Scraper v5 — browser extractor");

  // =========================================================================
  // CONFIG
  // =========================================================================
  const CONFIG = {
    scrollDelay: 1500,
    stableThreshold: 3,
    maxScrollRounds: 40,
    downloadDelay: 1200,
    batchPauseEvery: 10,
    batchPauseMs: 3000,
    revokeDelayMs: 15000,
    maxTitleLength: 120,
    minTitleLength: 3,
    minImageArea: 1024, // reject tiny trackers/icons when dimensions are available
    includeBackgroundImages: true,
    includeSrcsetCandidates: true,
    dedupeByCanonicalUrl: true,
    debug: true,
    mode: "export", // "export" | "download" | "both"
  };

  const SPAM_URL_KEYWORDS = [
    "analytics",
    "tracker",
    "pixel",
    "doubleclick",
    "googleads",
    "facebook.com/tr",
    "bat.bing",
    "/ads/",
    "adgeek",
  ];

  const BLACKLIST_TITLES = new Set([
    "image",
    "photo",
    "thumbnail",
    "untitled",
    "click",
    "download",
    "view image",
    "loading",
    "link",
    "logo",
    "icon",
    "...",
  ]);

  const TRACKING_QUERY_KEYS = new Set([
    "utm_source",
    "utm_medium",
    "utm_campaign",
    "utm_term",
    "utm_content",
    "gclid",
    "fbclid",
    "msclkid",
    "mc_eid",
    "mc_cid",
  ]);

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
      .replace(/[\u200B-\u200D\uFEFF]/g, "")
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
    if (t.length > CONFIG.maxTitleLength) return true;
    if (BLACKLIST_TITLES.has(t)) return true;
    if (/^\d+$/.test(t)) return true;
    if (/^[\W_]+$/u.test(t)) return true;
    return false;
  }

  function createScoringEngine() {
    const byText = new Map();
    const candidates = [];

    function add(text, score, source) {
      const cleaned = sanitize(text);
      if (isGarbage(cleaned)) return;
      const key = cleaned.toLowerCase();
      const existing = byText.get(key);
      if (!existing || score > existing.score) {
        const next = { text: cleaned, score, source };
        byText.set(key, next);
      }
    }

    function best() {
      candidates.length = 0;
      candidates.push(...byText.values());
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

  function parseSrcset(srcsetText) {
    if (!srcsetText) return [];
    return String(srcsetText)
      .split(",")
      .map((part) => part.trim())
      .filter(Boolean)
      .map((part) => {
        const [rawUrl, descriptor] = part.split(/\s+/, 2);
        let weight = 1;
        if (descriptor && /\d+w$/i.test(descriptor)) {
          weight = Number(descriptor.replace(/\D+/g, "")) || 1;
        } else if (descriptor && /\d+(\.\d+)?x$/i.test(descriptor)) {
          weight = Number(descriptor.replace(/[^\d.]+/g, "")) || 1;
        }
        return { url: rawUrl, weight };
      })
      .sort((a, b) => b.weight - a.weight)
      .map((x) => x.url);
  }

  function collectNodeTitleSignals(node, engine, prefix = "node") {
    if (!node) return;
    const attrs = [
      "title",
      "aria-label",
      "data-title",
      "data-original-title",
      "data-caption",
      "data-name",
      "data-alt",
      "data-description",
    ];
    attrs.forEach((attr, idx) => {
      engine.add(node.getAttribute?.(attr), 90 - idx, `${prefix}.${attr}`);
    });
    if (node.dataset) {
      Object.entries(node.dataset).forEach(([k, v]) => {
        if (/title|caption|alt|name|desc/i.test(k)) {
          engine.add(v, 86, `${prefix}.dataset.${k}`);
        }
      });
    }
  }

  function cleanImageUrl(url) {
    if (!url) return "";
    try {
      const u = new URL(url, location.href);
      ["w", "h", "width", "height", "sz", "quality", "q"].forEach((key) => {
        u.searchParams.delete(key);
      });
      for (const key of [...u.searchParams.keys()]) {
        if (TRACKING_QUERY_KEYS.has(key.toLowerCase())) {
          u.searchParams.delete(key);
        }
      }
      u.pathname = u.pathname.replace(/\/small\//gi, "/large/");
      u.pathname = u.pathname.replace(/\/thumb\//gi, "/large/");
      u.hash = "";
      return u.toString();
    } catch {
      return String(url).trim();
    }
  }

  function canonicalMediaUrl(url) {
    return cleanImageUrl(url);
  }

  function extractFilename(url) {
    try {
      const pathname = new URL(url, location.href).pathname;
      let file = pathname.split("/").pop() || "";
      file = decodeURIComponent(file);
      file = file.replace(/\.[a-z0-9]+$/i, "");
      file = file.replace(/[-_]+/g, " ");
      // Bing/CDN noise like OIP.xxxxx
      if (/^(OIP|ODF|thid)\b/i.test(file) || file.length > 40) return "";
      return sanitize(file);
    } catch {
      return "";
    }
  }

  function isSpamUrl(url) {
    const u = String(url).toLowerCase();
    return SPAM_URL_KEYWORDS.some((k) => u.includes(k));
  }

  function hasUsefulArea(el) {
    const w =
      Number(el?.naturalWidth) ||
      Number(el?.videoWidth) ||
      Number(el?.width) ||
      Number(el?.getAttribute?.("width")) ||
      0;
    const h =
      Number(el?.naturalHeight) ||
      Number(el?.videoHeight) ||
      Number(el?.height) ||
      Number(el?.getAttribute?.("height")) ||
      0;
    if (!w || !h) return true; // unknown dimensions: do not over-filter
    return w * h >= CONFIG.minImageArea;
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
      "video/mp4": ".mp4",
      "video/webm": ".webm",
    };
    return map[mime] || fallback;
  }

  // =========================================================================
  // AUTO SCROLL (lazy-load wake-up)
  // =========================================================================
  async function deepScroll() {
    console.log("🔄 Deep-scrolling to wake lazy-loaded media...");
    let lastHeight = document.body.scrollHeight;
    let stableCount = 0;
    let rounds = 0;

    while (stableCount < CONFIG.stableThreshold && rounds < CONFIG.maxScrollRounds) {
      window.scrollTo({ top: document.body.scrollHeight, behavior: "smooth" });
      await sleep(CONFIG.scrollDelay);
      const newHeight = document.body.scrollHeight;
      if (newHeight === lastHeight) {
        stableCount += 1;
      } else {
        stableCount = 0;
        lastHeight = newHeight;
      }
      rounds += 1;
    }
    window.scrollTo(0, 0);
    console.log("✅ Page expand complete");
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
      const srcsetCandidates = CONFIG.includeSrcsetCandidates
        ? parseSrcset(img?.srcset || "").concat(
            parseSrcset(card.querySelector("source")?.srcset || "")
          )
        : [];
      const imageUrl =
        srcsetCandidates[0] ||
        meta.murl ||
        meta.imgurl ||
        img?.currentSrc ||
        img?.src ||
        "";
      if (!imageUrl || imageUrl.startsWith("data:") || isSpamUrl(imageUrl)) {
        return null;
      }

      const engine = createScoringEngine();
      // Highest confidence: Bing native metadata title
      engine.add(meta.t, 100, "bing.meta.t");
      engine.add(meta.title, 98, "bing.meta.title");
      engine.add(meta.desc, 85, "bing.meta.desc");

      card.querySelectorAll("a[title]").forEach((a) => {
        engine.add(a.getAttribute("title"), 95, "a.title");
      });

      if (img) {
        if (!hasUsefulArea(img)) return null;
        engine.add(img.alt, 90, "img.alt");
        engine.add(img.title, 88, "img.title");
        engine.add(img.getAttribute("aria-label"), 87, "img.aria-label");
        Object.entries(img.dataset || {}).forEach(([k, v]) => {
          engine.add(v, 82, `img.dataset.${k}`);
        });
      }

      // Parent link wrapping the image
      const parentLink = img?.closest("a") || card.closest("a");
      if (parentLink) {
        engine.add(parentLink.getAttribute("title"), 93, "parent.a.title");
        engine.add(parentLink.innerText, 70, "parent.a.text");
      }

      engine.add(extractFilename(imageUrl), 60, "url.filename");
      engine.add(document.title, 40, "document.title");

      const best = engine.best();
      return {
        url: canonicalMediaUrl(imageUrl),
        suggestedName: normalizeFilename(best) || "bing_image",
        thumbnail: img?.currentSrc || img?.src || imageUrl,
        type: "image",
        source: "bing",
        titleSource: engine.candidates[0]?.source || "none",
      };
    } catch (err) {
      log("Bing card parse error:", err);
      return null;
    }
  }

  // =========================================================================
  // GENERIC IMAGE PARSER (multi-fallback title mining)
  // =========================================================================
  function parseGenericImage(img) {
    try {
      const srcsetCandidates = CONFIG.includeSrcsetCandidates
        ? parseSrcset(img.srcset || "").concat(
            parseSrcset(img.closest("picture")?.querySelector("source")?.srcset || "")
          )
        : [];
      const src = srcsetCandidates[0] || img.currentSrc || img.src;
      if (!src || src.startsWith("data:") || isSpamUrl(src)) return null;
      if (!hasUsefulArea(img)) return null;

      const engine = createScoringEngine();

      // 1) Image self attributes (often SEO-native)
      engine.add(img.alt, 92, "img.alt");
      engine.add(img.title, 90, "img.title");
      engine.add(img.getAttribute("aria-label"), 88, "img.aria-label");
      engine.add(img.getAttribute("data-title"), 94, "img.data-title");
      engine.add(
        img.getAttribute("data-original-title"),
        93,
        "img.data-original-title"
      );
      collectNodeTitleSignals(img, engine, "img");

      // 2) Direct parent <a> (common search-engine / gallery wrap)
      const parentLink = img.closest("a");
      if (parentLink) {
        engine.add(parentLink.getAttribute("title"), 95, "parent.a.title");
        engine.add(parentLink.getAttribute("aria-label"), 89, "parent.a.aria");
        const linkText = sanitize(parentLink.innerText);
        if (linkText.length > CONFIG.minTitleLength) {
          engine.add(linkText, 72, "parent.a.text");
        }
      }

      // 3) Card / figure container siblings
      const figure = img.closest("figure");
      if (figure) {
        const caption = figure.querySelector("figcaption");
        if (caption) engine.add(caption.innerText, 96, "figcaption");
      }

      const container = img.closest(
        'li, article, .imgpt, .dg_u, .infopt, .item, .card, [class*="item"], [class*="card"], [class*="result"]'
      );
      if (container) {
        // Prefer historical .infopt a path, then any a[title]
        const infopt = container.querySelector(".infopt a");
        if (infopt) {
          engine.add(infopt.getAttribute("title"), 94, ".infopt a.title");
          engine.add(infopt.innerText, 78, ".infopt a.text");
        }
        container.querySelectorAll("a[title]").forEach((a) => {
          engine.add(a.getAttribute("title"), 91, "container.a.title");
        });
        collectNodeTitleSignals(container, engine, "container");
        container.querySelectorAll("h1, h2, h3, h4, [class*='title']").forEach((h) => {
          engine.add(h.innerText || h.getAttribute("title"), 80, "container.heading");
        });
      }

      // 4) URL filename last resort
      engine.add(extractFilename(src), 55, "url.filename");
      engine.add(document.title, 40, "document.title");

      const best = engine.best();
      return {
        url: canonicalMediaUrl(src),
        suggestedName: normalizeFilename(best) || "image",
        thumbnail: img.currentSrc || img.src || src,
        type: "image",
        source: "generic",
        titleSource: engine.candidates[0]?.source || "none",
      };
    } catch (err) {
      log("Generic parse error:", err);
      return null;
    }
  }

  function parseBackgroundImageNode(node) {
    try {
      const computed = window.getComputedStyle(node);
      const bg = computed.backgroundImage || "";
      const matches = [...bg.matchAll(/url\((['"]?)(.*?)\1\)/gi)].map((m) => m[2]);
      if (!matches.length) return null;
      const bestUrl = matches
        .map((u) => canonicalMediaUrl(u))
        .find((u) => u && !u.startsWith("data:") && !isSpamUrl(u));
      if (!bestUrl) return null;
      const engine = createScoringEngine();
      collectNodeTitleSignals(node, engine, "bg.node");
      engine.add(node.innerText, 84, "bg.node.text");
      engine.add(node.closest("a")?.getAttribute("title"), 92, "bg.parent.a.title");
      engine.add(node.closest("figure")?.querySelector("figcaption")?.innerText, 95, "bg.figcaption");
      engine.add(extractFilename(bestUrl), 58, "bg.url.filename");
      engine.add(document.title, 40, "document.title");
      const best = engine.best();
      return {
        url: bestUrl,
        suggestedName: normalizeFilename(best) || "background_image",
        thumbnail: bestUrl,
        type: "image",
        source: "generic",
        titleSource: engine.candidates[0]?.source || "none",
      };
    } catch (err) {
      log("Background parse error:", err);
      return null;
    }
  }

  function parseGenericVideo(node) {
    try {
      const videoEl = node.tagName?.toLowerCase() === "video" ? node : node.closest("video");
      const url =
        videoEl?.currentSrc ||
        videoEl?.src ||
        node.src ||
        [...(videoEl?.querySelectorAll("source") || [])]
          .map((s) => s.src)
          .find(Boolean) ||
        "";
      if (!url || url.startsWith("blob:") || isSpamUrl(url)) return null;
      const engine = createScoringEngine();
      collectNodeTitleSignals(videoEl, engine, "video");
      engine.add(videoEl?.closest("a")?.getAttribute("title"), 93, "video.parent.a.title");
      engine.add(videoEl?.closest("figure")?.querySelector("figcaption")?.innerText, 96, "video.figcaption");
      engine.add(extractFilename(url), 58, "video.url.filename");
      engine.add(document.title, 40, "document.title");
      const best = engine.best();
      return {
        url: canonicalMediaUrl(url),
        suggestedName: normalizeFilename(best) || "video",
        thumbnail: videoEl?.poster || url,
        type: "video",
        source: /bing\.com/i.test(location.hostname) ? "bing" : "generic",
        titleSource: engine.candidates[0]?.source || "video",
      };
    } catch (err) {
      log("Video parse error:", err);
      return null;
    }
  }

  // =========================================================================
  // COLLECT
  // =========================================================================
  await deepScroll();

  const seen = new Set();
  const mediaEntries = [];
  const isBing = /bing\.com/i.test(location.hostname);

  function addEntry(parsed) {
    if (!parsed?.url) return;
    const key = CONFIG.dedupeByCanonicalUrl ? canonicalMediaUrl(parsed.url) : parsed.url;
    if (!key || seen.has(key)) return;
    seen.add(key);
    mediaEntries.push({ ...parsed, url: key });
  }

  if (isBing) {
    console.log("🟦 Bing Images mode");
    const cards = [
      ...document.querySelectorAll(".iusc, .imgpt, .iuscp"),
    ];
    // Prefer unique .iusc nodes (metadata carriers)
    const nodes = cards.length
      ? cards
      : [...document.querySelectorAll("[m]")];
    console.log(`🔎 Scanning ${nodes.length} Bing cards`);
    for (const card of nodes) {
      const parsed = parseBingCard(card);
      addEntry(parsed);
    }
  } else {
    console.log("🌐 Generic site mode");
    const images = [...document.querySelectorAll("img")];
    console.log(`🔎 Scanning ${images.length} images`);
    for (const img of images) {
      const parsed = parseGenericImage(img);
      addEntry(parsed);
    }

    if (CONFIG.includeBackgroundImages) {
      const bgNodes = [
        ...document.querySelectorAll("[style*='background'], [style*='background-image']"),
      ];
      log(`🎨 Scanning ${bgNodes.length} background-image nodes`);
      for (const node of bgNodes) {
        const parsed = parseBackgroundImageNode(node);
        addEntry(parsed);
      }
    }
  }

  // Videos (generic + Bing pages containing videos)
  document.querySelectorAll("video, video source").forEach((node) => {
    const parsed = parseGenericVideo(node);
    addEntry(parsed);
  });

  console.table(
    mediaEntries.map((x) => ({
      title: x.suggestedName,
      via: x.titleSource,
      type: x.type,
      url: x.url.substring(0, 72),
    }))
  );
  console.log(`✅ Collected ${mediaEntries.length} media items`);

  if (mediaEntries.length === 0) {
    alert("⚠️ No media found. Try the Bing results list page (not the viewer).");
    return;
  }

  // =========================================================================
  // EXPORT MANIFEST (recommended — avoids .crdownload / CORS issues)
  // =========================================================================
  const manifest = {
    schemaVersion: "image-title-scraper.manifest.v2",
    generatedAt: new Date().toISOString(),
    pageUrl: location.href,
    pageTitle: document.title,
    count: mediaEntries.length,
    stats: mediaEntries.reduce(
      (acc, item) => {
        if (item.type === "video") acc.videos += 1;
        else acc.images += 1;
        return acc;
      },
      { images: 0, videos: 0 }
    ),
    items: mediaEntries.map((e, i) => ({
      index: i + 1,
      url: e.url,
      title: e.suggestedName,
      type: e.type,
      titleSource: e.titleSource,
      source: e.source,
    })),
  };

  const jsonText = JSON.stringify(manifest, null, 2);

  async function copyManifest() {
    try {
      await navigator.clipboard.writeText(jsonText);
      console.log("📋 Manifest copied to clipboard");
      console.log(
        "💡 Tip: in console run copy(JSON.stringify(window.__IMAGE_TITLE_MANIFEST__, null, 2)) if needed."
      );
      return true;
    } catch {
      // Fallback download of JSON file
      const blob = new Blob([jsonText], { type: "application/json" });
      const blobUrl = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = blobUrl;
      a.download = `image-title-manifest_${Date.now()}.json`;
      document.body.appendChild(a);
      a.click();
      setTimeout(() => {
        a.remove();
        URL.revokeObjectURL(blobUrl);
      }, 2000);
      console.log("💾 Manifest JSON file download triggered");
      return false;
    }
  }

  // =========================================================================
  // OPTIONAL IN-BROWSER DOWNLOAD
  // =========================================================================
  async function forceDownload(entry, index) {
    const base =
      `${String(index).padStart(3, "0")}_` +
      (entry.suggestedName || "image").substring(0, 80);

    try {
      const resp = await fetch(entry.url, { mode: "cors", credentials: "omit" });
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      const blob = await resp.blob();
      const ext = extFromMime(blob.type, entry.type === "video" ? ".mp4" : ".jpg");
      const blobUrl = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = blobUrl;
      a.download = `${base}${ext}`;
      a.style.display = "none";
      document.body.appendChild(a);
      a.click();
      setTimeout(() => {
        a.remove();
        URL.revokeObjectURL(blobUrl);
      }, CONFIG.revokeDelayMs);
      console.log(`✅ [blob] ${base}${ext}`);
      return true;
    } catch (err) {
      console.warn(`⚠️ [cors] cannot rename via blob: ${entry.url}`, err.message || err);
      return false;
    }
  }

  // Decide mode via prompt (default: export for Python)
  const choice = (window.prompt(
    `Found ${mediaEntries.length} items.\n` +
      `Type: export | download | both\n` +
      `(export → Python download.py avoids .crdownload)`,
    CONFIG.mode
  ) || "export")
    .trim()
    .toLowerCase();

  if (choice === "export" || choice === "both") {
    await copyManifest();
    console.log(
      "➡️ Next: save clipboard JSON as manifest.json, then run:\n" +
        "   python download.py manifest.json"
    );
    // Also expose for manual copy
    window.__IMAGE_TITLE_MANIFEST__ = manifest;
    console.log("📦 Also available as window.__IMAGE_TITLE_MANIFEST__");
  }

  if (choice === "download" || choice === "both") {
    console.warn(
      "🔔 Allow multiple automatic downloads in the address-bar prompt if Chrome blocks them."
    );
    console.warn(
      "🔔 Turn OFF “Ask where to save each file” in Chrome download settings."
    );

    let ok = 0;
    let fail = 0;
    const failed = [];

    for (let i = 0; i < mediaEntries.length; i++) {
      const success = await forceDownload(mediaEntries[i], i + 1);
      if (success) ok += 1;
      else {
        fail += 1;
        failed.push(mediaEntries[i]);
      }
      const delay =
        i > 0 && (i + 1) % CONFIG.batchPauseEvery === 0
          ? CONFIG.batchPauseMs
          : CONFIG.downloadDelay;
      await sleep(delay);
    }

    console.log(`🎉 Browser download done. ok=${ok} failed=${fail}`);

    // Rescue gallery for CORS failures
    if (failed.length) {
      const overlay = document.createElement("div");
      overlay.style.cssText =
        "position:fixed;inset:5%;background:#f8f9fa;border:3px solid #333;z-index:999999;overflow:auto;padding:20px;font-family:sans-serif;";
      const close = document.createElement("button");
      close.textContent = "Close gallery";
      close.onclick = () => overlay.remove();
      const h = document.createElement("h2");
      h.textContent = `CORS-blocked items (${failed.length}) — open manually or use Python downloader`;
      const grid = document.createElement("div");
      grid.style.cssText =
        "display:grid;grid-template-columns:repeat(auto-fill,minmax(220px,1fr));gap:16px;margin-top:16px;";
      failed.forEach((entry) => {
        const box = document.createElement("div");
        box.style.cssText =
          "background:#fff;padding:10px;border:1px solid #ddd;border-radius:8px;text-align:center;";
        const media =
          entry.type === "video"
            ? document.createElement("video")
            : document.createElement("img");
        media.src = entry.thumbnail || entry.url;
        media.style.cssText =
          "max-width:100%;height:140px;object-fit:contain;background:#eee;";
        const label = document.createElement("p");
        label.textContent = entry.suggestedName || "(no title)";
        label.style.cssText = "font-size:12px;word-break:break-all;";
        const link = document.createElement("a");
        link.href = entry.url;
        link.target = "_blank";
        link.rel = "noopener";
        link.textContent = "Open original";
        box.append(media, label, link);
        grid.appendChild(box);
      });
      overlay.append(close, h, grid);
      document.body.appendChild(overlay);
    }
  }

  console.log("Done.");
})();
