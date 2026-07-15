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
 * Capabilities:
 *   - Multi-source title scoring with length / blacklist filters
 *   - Bing `.iusc` metadata (`m` JSON: murl + t) as highest-confidence source
 *   - Parent <a title>, card containers, figcaption, data-*, img alt/title fallbacks
 *   - Manifest export to avoid Chrome .crdownload / CORS download failures
 *   - srcset/lazy attributes, open shadow DOM, CSS backgrounds, and page metadata
 *   - Bounded auto-scroll and non-destructive URL normalization
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
    maxScrollSteps: 60,
    downloadDelay: 1200,
    batchPauseEvery: 10,
    batchPauseMs: 3000,
    revokeDelayMs: 15000,
    fetchTimeoutMs: 30000,
    maxTitleLength: 120,
    minTitleLength: 3,
    minImageDimension: 48,
    includeCssBackgrounds: true,
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
    "google-analytics",
    "adservice",
    "beacon",
    "scorecardresearch",
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
    "avatar",
    "spacer",
    "blank",
    "...",
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
      .replace(/[\\/*?:"<>|]/g, "")
      .replace(/[\u0000-\u001f\u007f]/g, "")
      .replace(/\s+/g, " ")
      .trim();
  }

  function normalizeFilename(text) {
    let name = sanitize(text)
      .replace(/\s+/g, "_")
      .replace(/[. ]+$/g, "")
      .substring(0, CONFIG.maxTitleLength);
    if (/^(con|prn|aux|nul|com[1-9]|lpt[1-9])$/i.test(name)) {
      name = `_${name}`;
    }
    return name;
  }

  function isGarbage(text) {
    if (!text) return true;
    const t = text.toLowerCase().trim();
    if (t.length < CONFIG.minTitleLength) return true;
    if (t.length > CONFIG.maxTitleLength) return true;
    if (BLACKLIST_TITLES.has(t)) return true;
    if (/^\d+$/.test(t)) return true;
    if (/^[a-f0-9]{16,}$/i.test(t)) return true;
    if (/^(?:https?:\/\/|www\.)/i.test(t)) return true;
    return false;
  }

  function createScoringEngine() {
    const candidates = [];

    function add(text, score, source) {
      const cleaned = sanitize(text);
      if (isGarbage(cleaned)) return;
      const existing = candidates.find(
        (candidate) => candidate.text.toLowerCase() === cleaned.toLowerCase()
      );
      if (!existing) {
        candidates.push({ text: cleaned, score, source });
      } else if (score > existing.score) {
        Object.assign(existing, { text: cleaned, score, source });
      }
    }

    function best() {
      candidates.sort((a, b) => {
        if (b.score !== a.score) return b.score - a.score;
        return b.text.length - a.text.length;
      });
      if (CONFIG.debug && candidates.length) {
        log("🧠 title candidates:", candidates.slice(0, 5));
      }
      return candidates[0] || null;
    }

    return { add, best, candidates };
  }

  function normalizeUrl(url) {
    try {
      const parsed = new URL(url, location.href);
      if (!/^https?:$/.test(parsed.protocol)) return "";
      parsed.hash = "";
      return parsed.href;
    } catch {
      return "";
    }
  }

  function urlKey(url) {
    try {
      const parsed = new URL(url);
      parsed.hash = "";
      return parsed.href;
    } catch {
      return url;
    }
  }

  function bestFromSrcset(srcset) {
    if (!srcset) return "";
    let best = "";
    let bestRank = -1;
    for (const candidate of srcset.split(",")) {
      const [url, descriptor = ""] = candidate.trim().split(/\s+/);
      if (!url || url.startsWith("data:")) continue;
      const rank = descriptor.endsWith("w")
        ? Number.parseFloat(descriptor)
        : descriptor.endsWith("x")
          ? Number.parseFloat(descriptor) * 10000
          : 0;
      if (Number.isFinite(rank) && rank >= bestRank) {
        best = url;
        bestRank = rank;
      }
    }
    return best;
  }

  function mediaUrl(element) {
    if (!element) return "";
    const lazyAttributes = [
      "data-original",
      "data-src",
      "data-lazy-src",
      "data-full",
      "data-url",
    ];
    const srcset =
      element.getAttribute("data-srcset") || element.getAttribute("srcset");
    const candidates = [
      bestFromSrcset(srcset),
      ...lazyAttributes.map((name) => element.getAttribute(name)),
      element.currentSrc,
      element.getAttribute("src"),
    ];
    return normalizeUrl(candidates.find(Boolean) || "");
  }

  function deepQueryAll(selector) {
    const matches = [];
    const visited = new Set();
    function walk(root) {
      if (!root || visited.has(root)) return;
      visited.add(root);
      matches.push(...root.querySelectorAll(selector));
      root.querySelectorAll("*").forEach((element) => {
        if (element.shadowRoot) walk(element.shadowRoot);
      });
    }
    walk(document);
    return [...new Set(matches)];
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
    let lastMediaCount = 0;
    let stableCount = 0;
    let steps = 0;

    while (
      stableCount < CONFIG.stableThreshold &&
      steps < CONFIG.maxScrollSteps
    ) {
      window.scrollTo({ top: document.body.scrollHeight, behavior: "smooth" });
      await sleep(CONFIG.scrollDelay);
      const newHeight = document.body.scrollHeight;
      const mediaCount = deepQueryAll("img, video, source").length;
      if (newHeight === lastHeight && mediaCount === lastMediaCount) {
        stableCount += 1;
      } else {
        stableCount = 0;
        lastHeight = newHeight;
        lastMediaCount = mediaCount;
      }
      steps += 1;
    }
    window.scrollTo(0, 0);
    console.log(`✅ Page expand complete (${steps} scroll steps)`);
  }

  // =========================================================================
  // BING CARD PARSER (.iusc metadata is the native title source)
  // =========================================================================
  function parseBingCard(card) {
    try {
      const iusc = card.matches?.(".iusc")
        ? card
        : card.querySelector(".iusc") || card;
      const scope = card.matches?.(".iusc")
        ? card.closest(".imgpt, .iuscp") || card.parentElement || card
        : card;
      let meta = {};
      const raw = iusc.getAttribute?.("m");
      if (raw) {
        try {
          meta = JSON.parse(raw);
        } catch {
          meta = {};
        }
      }

      const img = scope.matches?.("img") ? scope : scope.querySelector("img");
      const imageUrl =
        normalizeUrl(meta.murl || meta.imgurl || mediaUrl(img) || "");
      if (!imageUrl || isSpamUrl(imageUrl)) {
        return null;
      }

      const engine = createScoringEngine();
      // Highest confidence: Bing native metadata title
      engine.add(meta.t, 100, "bing.meta.t");
      engine.add(meta.title, 98, "bing.meta.title");
      engine.add(meta.desc, 85, "bing.meta.desc");

      scope.querySelectorAll("a[title]").forEach((a) => {
        engine.add(a.getAttribute("title"), 95, "a.title");
      });

      if (img) {
        engine.add(img.alt, 90, "img.alt");
        engine.add(img.title, 88, "img.title");
        engine.add(img.getAttribute("aria-label"), 87, "img.aria-label");
        Object.entries(img.dataset || {}).forEach(([k, v]) => {
          engine.add(v, 82, `img.dataset.${k}`);
        });
      }

      // Parent link wrapping the image
      const parentLink = img?.closest("a") || scope.closest("a");
      if (parentLink) {
        engine.add(parentLink.getAttribute("title"), 93, "parent.a.title");
        engine.add(parentLink.innerText, 70, "parent.a.text");
      }

      engine.add(extractFilename(imageUrl), 60, "url.filename");

      const best = engine.best();
      return {
        url: imageUrl,
        suggestedName: normalizeFilename(best?.text) || "bing_image",
        thumbnail: mediaUrl(img) || imageUrl,
        type: "image",
        source: "bing",
        sourcePage: normalizeUrl(meta.purl || meta.surl || ""),
        titleSource: best?.source || "none",
        width: Number(meta.w || img?.naturalWidth) || null,
        height: Number(meta.h || img?.naturalHeight) || null,
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
      const src = mediaUrl(img);
      if (!src || isSpamUrl(src)) return null;
      if (
        img.complete &&
        img.naturalWidth &&
        img.naturalHeight &&
        Math.max(img.naturalWidth, img.naturalHeight) < CONFIG.minImageDimension
      ) {
        return null;
      }

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
      Object.entries(img.dataset || {}).forEach(([k, v]) => {
        if (/title|caption|alt|name|desc/i.test(k)) {
          engine.add(v, 86, `dataset.${k}`);
        }
      });

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
        container.querySelectorAll("h1, h2, h3, h4, [class*='title']").forEach((h) => {
          engine.add(h.innerText || h.getAttribute("title"), 80, "container.heading");
        });
      }

      // 4) URL filename last resort
      engine.add(extractFilename(src), 55, "url.filename");

      const best = engine.best();
      return {
        url: src,
        suggestedName: normalizeFilename(best?.text) || "image",
        thumbnail: src,
        type: "image",
        source: "generic",
        sourcePage: parentLink?.href || location.href,
        titleSource: best?.source || "none",
        width: img.naturalWidth || null,
        height: img.naturalHeight || null,
      };
    } catch (err) {
      log("Generic parse error:", err);
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
  function addEntry(entry) {
    if (!entry?.url) return;
    entry.url = normalizeUrl(entry.url);
    const key = urlKey(entry.url);
    if (!entry.url || isSpamUrl(entry.url) || seen.has(key)) return;
    seen.add(key);
    mediaEntries.push(entry);
  }

  if (isBing) {
    console.log("🟦 Bing Images mode");
    const metadataCards = deepQueryAll(".iusc");
    const cards = metadataCards.length
      ? metadataCards
      : deepQueryAll(".imgpt, .iuscp");
    // Prefer unique .iusc nodes (metadata carriers)
    const nodes = cards.length
      ? cards
      : deepQueryAll("[m]");
    console.log(`🔎 Scanning ${nodes.length} Bing cards`);
    for (const card of nodes) {
      const parsed = parseBingCard(card);
      addEntry(parsed);
    }
  }

  // Always scan regular images too: search engines and mixed pages contain
  // useful media outside their result-card markup.
  const images = deepQueryAll(
    "img, [data-src], [data-original], [data-lazy-src], [data-srcset]"
  ).filter(
    (element) =>
      element.matches("img") && !(isBing && element.closest(".iusc, .imgpt, .iuscp"))
  );
  console.log(`🔎 Scanning ${images.length} DOM images`);
  for (const img of images) addEntry(parseGenericImage(img));

  // Videos, including nested <source> candidates.
  deepQueryAll("video, video source").forEach((vid) => {
    const url = mediaUrl(vid);
    if (!url) return;
    const owner = vid.closest("video") || vid;
    const title =
      owner.getAttribute("title") ||
      owner.getAttribute("aria-label") ||
      owner.closest("a")?.getAttribute("title") ||
      owner.closest("figure")?.querySelector("figcaption")?.innerText ||
      extractFilename(url) ||
      "";
    addEntry({
      url,
      suggestedName: normalizeFilename(sanitize(title)) || "video",
      thumbnail: normalizeUrl(owner.getAttribute("poster")) || "",
      type: "video",
      source: isBing ? "bing" : "generic",
      sourcePage: location.href,
      titleSource: "video",
    });
  });

  // CSS backgrounds are common in galleries and product cards.
  if (CONFIG.includeCssBackgrounds) {
    deepQueryAll("[style*='background']").forEach((element) => {
      const background = getComputedStyle(element).backgroundImage || "";
      for (const match of background.matchAll(/url\((['"]?)(.*?)\1\)/g)) {
        const url = normalizeUrl(match[2]);
        if (!url || match[2].startsWith("data:")) continue;
        const engine = createScoringEngine();
        engine.add(element.getAttribute("aria-label"), 90, "background.aria");
        engine.add(element.getAttribute("title"), 88, "background.title");
        engine.add(
          element.closest("figure")?.querySelector("figcaption")?.innerText,
          94,
          "background.figcaption"
        );
        engine.add(extractFilename(url), 55, "url.filename");
        const best = engine.best();
        addEntry({
          url,
          suggestedName: normalizeFilename(best?.text) || "background_image",
          thumbnail: url,
          type: "image",
          source: "css-background",
          sourcePage: location.href,
          titleSource: best?.source || "none",
          width: element.clientWidth || null,
          height: element.clientHeight || null,
        });
      }
    });
  }

  // Page-level originals can exist even when no corresponding <img> is rendered.
  document
    .querySelectorAll(
      'meta[property="og:image"], meta[name="twitter:image"], link[rel="image_src"]'
    )
    .forEach((element) => {
      const url = normalizeUrl(
        element.getAttribute("content") || element.getAttribute("href")
      );
      if (!url) return;
      addEntry({
        url,
        suggestedName:
          normalizeFilename(
            document.querySelector('meta[property="og:title"]')?.content ||
              document.title
          ) || "page_image",
        thumbnail: url,
        type: "image",
        source: "page-metadata",
        sourcePage: location.href,
        titleSource: "page.title",
      });
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
    schemaVersion: 2,
    generator: "image-title-scraper/browser-extractor-v5",
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
      sourcePage: e.sourcePage || location.href,
      thumbnail: e.thumbnail || "",
      width: e.width || null,
      height: e.height || null,
    })),
  };

  const jsonText = JSON.stringify(manifest, null, 2);

  async function copyManifest() {
    try {
      await navigator.clipboard.writeText(jsonText);
      console.log("📋 Manifest copied to clipboard");
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

    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), CONFIG.fetchTimeoutMs);
    try {
      const resp = await fetch(entry.url, {
        mode: "cors",
        credentials: "omit",
        signal: controller.signal,
      });
      if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
      const blob = await resp.blob();
      if (!/^(image|video)\//i.test(blob.type) && blob.type) {
        throw new Error(`unexpected content type ${blob.type}`);
      }
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
    } finally {
      clearTimeout(timeout);
    }
  }

  // Decide mode via prompt (default: export for Python)
  const requestedChoice = (window.prompt(
    `Found ${mediaEntries.length} items.\n` +
      `Type: export | download | both\n` +
      `(export → Python download.py avoids .crdownload)`,
    CONFIG.mode
  ) || "export")
    .trim()
    .toLowerCase();
  const choice = ["export", "download", "both"].includes(requestedChoice)
    ? requestedChoice
    : CONFIG.mode;
  if (choice !== requestedChoice) {
    console.warn(`Unknown mode "${requestedChoice}"; using "${choice}".`);
  }

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
