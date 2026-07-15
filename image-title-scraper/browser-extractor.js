/**
 * Image Title Scraper — Browser Extractor v5
 *
 * Purpose:
 *   Scroll a page (Bing Images, Google Images, or generic sites), discover
 *   media, score candidate native titles, then either download in-browser or
 *   export a JSON/CSV manifest for the companion Python downloader
 *   (recommended).
 *
 * How to use:
 *   1. Open the target page (image results list works best).
 *   2. Open DevTools → Console.
 *   3. Paste this entire file and press Enter.
 *   4. When prompted, prefer "export" then run: python download.py manifest.json
 *
 * Optional config override (set BEFORE pasting the script):
 *   window.__SCRAPER_CONFIG__ = { minImageSize: 120, maxItems: 200, mode: "export" };
 *
 * Emergency stop during in-browser downloads:
 *   window.__SCRAPER_STOP__ = true;
 *
 * Upgrades vs v4:
 *   - Incremental collection DURING scrolling (virtualized galleries such as
 *     Bing/Google unload offscreen cards; v4 only scanned at the end and lost them)
 *   - Best-resolution source resolution: srcset / <picture> / data-src /
 *     data-lazy-src / data-original lazy-load attributes
 *   - Google Images adapter (parses /imgres?imgurl=… for full-size URLs)
 *   - Shadow DOM + same-origin iframe traversal
 *   - CSS background-image harvesting
 *   - Minimum-rendered-size filter to skip icons/sprites/trackers
 *   - CSV export next to the JSON manifest, per-item referer + dimensions
 *   - Safety cap on scroll rounds, maxItems limit, runtime config overrides
 */
(async function () {
  "use strict";

  console.clear();
  console.log("🚀 Image Title Scraper v5 — browser extractor");

  // =========================================================================
  // CONFIG (override any key via window.__SCRAPER_CONFIG__ before running)
  // =========================================================================
  const CONFIG = Object.assign(
    {
      scrollDelay: 1500,
      stableThreshold: 3,
      maxScrollRounds: 60, // hard cap so infinite feeds terminate
      downloadDelay: 1200,
      batchPauseEvery: 10,
      batchPauseMs: 3000,
      revokeDelayMs: 15000,
      fetchTimeoutMs: 30000,
      maxTitleLength: 120,
      minTitleLength: 3,
      minImageSize: 80, // skip images rendered smaller than this (px); 0 disables
      maxItems: 0, // stop collecting after N items; 0 = unlimited
      includeBackgroundImages: true,
      includeVideos: true,
      debug: true,
      mode: "export", // "export" | "download" | "both"
      exportCsv: true, // also produce a CSV manifest on export
    },
    typeof window !== "undefined" ? window.__SCRAPER_CONFIG__ : null
  );

  const SPAM_URL_KEYWORDS = [
    "analytics",
    "tracker",
    "pixel",
    "doubleclick",
    "googleads",
    "googlesyndication",
    "facebook.com/tr",
    "bat.bing",
    "/ads/",
    "adgeek",
    "adservice",
    "/sprite",
    "spacer.gif",
    "blank.gif",
    "1x1.",
  ];

  const BLACKLIST_TITLES = new Set([
    "image",
    "img",
    "photo",
    "picture",
    "thumbnail",
    "thumb",
    "icon",
    "logo",
    "avatar",
    "banner",
    "untitled",
    "click",
    "click here",
    "download",
    "view image",
    "see more",
    "loading",
    "link",
    "...",
    "read more",
    "open",
    "close",
    "next",
    "previous",
    "share",
  ]);

  // =========================================================================
  // UTILS
  // =========================================================================
  const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
  const log = (...args) => {
    if (CONFIG.debug) console.log(...args);
  };

  // innerText is undefined for detached nodes / non-browser DOMs; fall back
  const textOf = (el) => (el ? el.innerText || el.textContent || "" : "");

  function sanitize(text) {
    if (!text) return "";
    return String(text)
      .replace(/[\n\r\t]+/g, " ")
      .replace(/[\u200B-\u200D\uFEFF]/g, "") // zero-width chars
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
    if (/^[\W_]+$/u.test(t)) return true; // punctuation/symbols only
    if (/^[a-f0-9-]{16,}$/i.test(t)) return true; // hex ids / uuids
    if (/^(jpe?g|png|gif|webp|avif|svg|mp4|webm)$/i.test(t)) return true;
    return false;
  }

  function createScoringEngine() {
    // Dedupe candidates by lowercase text, keeping the highest score
    const byText = new Map();

    function add(text, score, source) {
      const cleaned = sanitize(text);
      if (isGarbage(cleaned)) return;
      const key = cleaned.toLowerCase();
      const existing = byText.get(key);
      if (!existing || score > existing.score) {
        byText.set(key, { text: cleaned, score, source });
      }
    }

    function best() {
      const candidates = [...byText.values()];
      candidates.sort((a, b) => {
        if (b.score !== a.score) return b.score - a.score;
        return b.text.length - a.text.length;
      });
      return candidates[0] || null;
    }

    return { add, best };
  }

  const TRACKING_QUERY_PARAMS = new Set([
    "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content",
    "gclid", "fbclid", "msclkid", "mc_eid", "mc_cid",
  ]);

  function cleanImageUrl(url) {
    if (!url) return "";
    try {
      const u = new URL(url, location.href);
      if (!/^https?:$/.test(u.protocol)) return "";
      // Tracking parameters are safe to remove. Preserve resize and quality
      // parameters because they can be part of signed CDN URLs.
      for (const key of [...u.searchParams.keys()]) {
        if (TRACKING_QUERY_PARAMS.has(key.toLowerCase())) {
          u.searchParams.delete(key);
        }
      }
      u.hash = "";
      return u.href;
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
      // Bing/CDN noise like OIP.xxxxx, pure hashes, overly long slugs
      if (/^(OIP|ODF|OIF|thid|th\b)/i.test(file)) return "";
      if (/^[a-f0-9]{12,}$/i.test(file.replace(/\s/g, ""))) return "";
      if (file.length > 60) return "";
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
      "image/bmp": ".bmp",
      "video/mp4": ".mp4",
      "video/webm": ".webm",
    };
    return map[mime] || fallback;
  }

  // =========================================================================
  // DEEP DOM TRAVERSAL (shadow roots + same-origin iframes)
  // =========================================================================
  function collectRoots() {
    const roots = [document];
    const walk = (root) => {
      const iterator = root.querySelectorAll("*");
      for (const el of iterator) {
        if (el.shadowRoot) {
          roots.push(el.shadowRoot);
          walk(el.shadowRoot);
        }
      }
    };
    try {
      walk(document);
    } catch (err) {
      log("shadow walk error:", err);
    }
    for (const frame of document.querySelectorAll("iframe")) {
      try {
        const doc = frame.contentDocument;
        if (doc) {
          roots.push(doc);
          walk(doc);
        }
      } catch {
        // cross-origin iframe — skip silently
      }
    }
    return roots;
  }

  function deepQueryAll(selector) {
    const out = [];
    for (const root of collectRoots()) {
      try {
        out.push(...root.querySelectorAll(selector));
      } catch {
        // invalid selector for this root type — skip
      }
    }
    return out;
  }

  // =========================================================================
  // BEST-RESOLUTION SOURCE RESOLUTION (srcset / <picture> / lazy attributes)
  // =========================================================================
  function parseSrcset(srcset) {
    // Returns { url, width } of the widest candidate
    if (!srcset) return null;
    let best = null;
    for (const part of srcset.split(",")) {
      const tokens = part.trim().split(/\s+/);
      const url = tokens[0];
      if (!url) continue;
      let width = 0;
      const descriptor = tokens[1] || "";
      if (/^\d+w$/.test(descriptor)) width = parseInt(descriptor, 10);
      else if (/^[\d.]+x$/.test(descriptor)) width = parseFloat(descriptor) * 1000;
      if (!best || width > best.width) best = { url, width };
    }
    return best;
  }

  const LAZY_SRC_ATTRS = [
    "data-src",
    "data-lazy-src",
    "data-original",
    "data-full-src",
    "data-hi-res-src",
    "data-large-src",
    "data-zoom-src",
    "data-image",
    "data-url",
  ];

  function resolveImageSource(img) {
    const candidates = [];

    const srcsetBest = parseSrcset(img.getAttribute("srcset"));
    if (srcsetBest) candidates.push({ ...srcsetBest, via: "srcset" });

    const picture = img.closest("picture");
    if (picture) {
      for (const source of picture.querySelectorAll("source[srcset]")) {
        const b = parseSrcset(source.getAttribute("srcset"));
        if (b) candidates.push({ ...b, via: "picture.source" });
      }
    }

    for (const attr of LAZY_SRC_ATTRS) {
      const v = img.getAttribute(attr);
      if (v && /^(https?:)?\/\//.test(v.trim())) {
        // Lazy full-size attributes usually beat the rendered thumbnail
        candidates.push({ url: v.trim(), width: 1e9, via: attr });
      }
    }

    const rendered = img.currentSrc || img.src;
    if (rendered) {
      candidates.push({ url: rendered, width: img.naturalWidth || 0, via: "src" });
    }

    candidates.sort((a, b) => b.width - a.width);
    const winner = candidates.find(
      (c) => c.url && !c.url.startsWith("data:") && !isSpamUrl(c.url)
    );
    return winner ? { url: winner.url, via: winner.via } : null;
  }

  function isTooSmall(img) {
    if (!CONFIG.minImageSize) return false;
    const w = img.naturalWidth || img.width || 0;
    const h = img.naturalHeight || img.height || 0;
    // Unknown size (not loaded yet) — keep, downloader can filter by bytes
    if (!w && !h) return false;
    return Math.max(w, h) < CONFIG.minImageSize;
  }

  // =========================================================================
  // SHARED TITLE MINING AROUND AN <img>
  // =========================================================================
  function mineImageTitles(engine, img) {
    engine.add(img.alt, 92, "img.alt");
    engine.add(img.title, 90, "img.title");
    engine.add(img.getAttribute("aria-label"), 88, "img.aria-label");
    engine.add(img.getAttribute("data-title"), 94, "img.data-title");
    engine.add(img.getAttribute("data-original-title"), 93, "img.data-original-title");
    Object.entries(img.dataset || {}).forEach(([k, v]) => {
      if (/title|caption|alt|name|desc/i.test(k)) {
        engine.add(v, 86, `dataset.${k}`);
      }
    });

    const parentLink = img.closest("a");
    if (parentLink) {
      engine.add(parentLink.getAttribute("title"), 95, "parent.a.title");
      engine.add(parentLink.getAttribute("aria-label"), 89, "parent.a.aria");
      const linkText = sanitize(textOf(parentLink));
      if (linkText.length > CONFIG.minTitleLength) {
        engine.add(linkText, 72, "parent.a.text");
      }
    }

    const figure = img.closest("figure");
    if (figure) {
      const caption = figure.querySelector("figcaption");
      if (caption) engine.add(textOf(caption), 96, "figcaption");
    }

    const container = img.closest(
      'li, article, .imgpt, .dg_u, .infopt, .item, .card, [class*="item"], [class*="card"], [class*="result"]'
    );
    if (container) {
      const infopt = container.querySelector(".infopt a");
      if (infopt) {
        engine.add(infopt.getAttribute("title"), 94, ".infopt a.title");
        engine.add(textOf(infopt), 78, ".infopt a.text");
      }
      container.querySelectorAll("a[title]").forEach((a) => {
        engine.add(a.getAttribute("title"), 91, "container.a.title");
      });
      container
        .querySelectorAll("h1, h2, h3, h4, [class*='title'], [class*='caption']")
        .forEach((h) => {
          engine.add(textOf(h) || h.getAttribute("title"), 80, "container.heading");
        });
    }
  }

  function buildEntry({ url, engine, thumbnail, type, source, fallbackName, referer, width, height }) {
    // Absolute last resort before the generic fallback name
    if (engine) engine.add(document.title, 40, "document.title");
    const best = engine ? engine.best() : null;
    return {
      url: cleanImageUrl(url),
      suggestedName: normalizeFilename(best?.text || "") || fallbackName,
      thumbnail: thumbnail || url,
      type: type || "image",
      source,
      titleSource: best?.source || "none",
      referer: referer || location.href,
      sourcePage: referer || location.href,
      width: width || 0,
      height: height || 0,
    };
  }

  // =========================================================================
  // SITE ADAPTERS
  // =========================================================================

  // ---- Bing Images: .iusc `m` JSON metadata is the native title source ----
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
      const resolved = img ? resolveImageSource(img) : null;
      const imageUrl = meta.murl || meta.imgurl || resolved?.url || "";
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
      if (img) mineImageTitles(engine, img);
      engine.add(extractFilename(imageUrl), 60, "url.filename");

      return buildEntry({
        url: imageUrl,
        engine,
        thumbnail: img?.src || imageUrl,
        source: "bing",
        fallbackName: "bing_image",
        referer: meta.purl || location.href,
        width: Number(meta.mw || meta.w || img?.naturalWidth) || 0,
        height: Number(meta.mh || meta.h || img?.naturalHeight) || 0,
      });
    } catch (err) {
      log("Bing card parse error:", err);
      return null;
    }
  }

  function collectBing(push) {
    const cards = deepQueryAll(".iusc, .imgpt, .iuscp");
    const nodes = cards.length ? cards : deepQueryAll("[m]");
    for (const card of nodes) push(parseBingCard(card));
  }

  // ---- Google Images: /imgres?imgurl=… anchors carry the full-size URL ----
  function collectGoogle(push) {
    // Primary: result anchors that link to the imgres viewer
    for (const a of deepQueryAll('a[href*="/imgres"], a[href*="imgurl="]')) {
      try {
        const href = a.getAttribute("href") || "";
        const params = new URL(href, location.href).searchParams;
        const imgurl = params.get("imgurl");
        if (!imgurl || isSpamUrl(imgurl)) continue;

        const engine = createScoringEngine();
        const img = a.querySelector("img");
        engine.add(a.getAttribute("aria-label"), 96, "google.a.aria");
        engine.add(a.getAttribute("title"), 95, "google.a.title");
        if (img) mineImageTitles(engine, img);
        const card = a.closest("[data-lpage], [data-ri], div[jsdata]") || a.parentElement;
        if (card) {
          card.querySelectorAll("h3, [role='heading']").forEach((h) => {
            engine.add(textOf(h), 90, "google.heading");
          });
        }
        engine.add(extractFilename(imgurl), 60, "url.filename");

        push(
          buildEntry({
            url: imgurl,
            engine,
            thumbnail: img?.src || imgurl,
            source: "google",
            fallbackName: "google_image",
            referer: params.get("imgrefurl") || location.href,
          })
        );
      } catch {
        // malformed href — skip
      }
    }
    // Fallback: any rendered result thumbnails (lower quality but better than nothing)
    for (const img of deepQueryAll("img")) {
      push(parseGenericImage(img));
    }
  }

  // ---- Generic sites: multi-fallback title mining ----
  function parseGenericImage(img) {
    try {
      if (isTooSmall(img)) return null;
      const resolved = resolveImageSource(img);
      if (!resolved) return null;

      const engine = createScoringEngine();
      mineImageTitles(engine, img);
      engine.add(extractFilename(resolved.url), 55, "url.filename");

      return buildEntry({
        url: resolved.url,
        engine,
        thumbnail: img.currentSrc || img.src || resolved.url,
        source: "generic",
        fallbackName: "image",
        width: img.naturalWidth || 0,
        height: img.naturalHeight || 0,
      });
    } catch (err) {
      log("Generic parse error:", err);
      return null;
    }
  }

  function collectGeneric(push) {
    for (const img of deepQueryAll("img")) push(parseGenericImage(img));

    for (const meta of document.querySelectorAll(
      'meta[property="og:image"], meta[name="twitter:image"], link[rel="image_src"]'
    )) {
      const url = meta.getAttribute("content") || meta.getAttribute("href");
      if (!url || isSpamUrl(url)) continue;
      const engine = createScoringEngine();
      engine.add(
        document.querySelector('meta[property="og:title"]')?.content,
        95,
        "page.og-title"
      );
      engine.add(document.title, 80, "document.title");
      push(
        buildEntry({
          url,
          engine,
          thumbnail: url,
          source: "page-metadata",
          fallbackName: "page_image",
        })
      );
    }

    if (CONFIG.includeBackgroundImages) {
      for (const el of deepQueryAll("div, section, span, a, li, figure, header")) {
        let bg;
        try {
          bg = getComputedStyle(el).backgroundImage;
        } catch {
          continue;
        }
        if (!bg || bg === "none") continue;
        const match = bg.match(/url\(["']?(.*?)["']?\)/i);
        const url = match?.[1];
        if (!url || url.startsWith("data:") || isSpamUrl(url)) continue;
        const rect = el.getBoundingClientRect();
        if (CONFIG.minImageSize && Math.max(rect.width, rect.height) < CONFIG.minImageSize) {
          continue;
        }
        const engine = createScoringEngine();
        engine.add(el.getAttribute("aria-label"), 90, "bg.aria-label");
        engine.add(el.getAttribute("title"), 89, "bg.title");
        engine.add(el.closest("a")?.getAttribute("title"), 88, "bg.parent.a.title");
        engine.add(extractFilename(url), 55, "url.filename");
        push(
          buildEntry({
            url,
            engine,
            thumbnail: url,
            source: "css-background",
            fallbackName: "background_image",
            width: Math.round(rect.width),
            height: Math.round(rect.height),
          })
        );
      }
    }
  }

  function collectVideos(push, sourceLabel) {
    if (!CONFIG.includeVideos) return;
    for (const vid of deepQueryAll("video, video source")) {
      const url = vid.currentSrc || vid.src;
      if (!url || url.startsWith("blob:")) continue;
      const holder = vid.closest("video") || vid;
      const engine = createScoringEngine();
      engine.add(holder.getAttribute("title"), 95, "video.title");
      engine.add(holder.getAttribute("aria-label"), 90, "video.aria-label");
      engine.add(holder.closest("a")?.getAttribute("title"), 88, "video.parent.a.title");
      engine.add(
        textOf(holder.closest("figure")?.querySelector("figcaption")),
        96,
        "video.figcaption"
      );
      engine.add(extractFilename(url), 55, "url.filename");
      push(
        buildEntry({
          url,
          engine,
          thumbnail: holder.poster || url,
          type: "video",
          source: sourceLabel,
          fallbackName: "video",
        })
      );
    }
  }

  // =========================================================================
  // COLLECTION DRIVER (incremental — runs during scrolling so virtualized
  // galleries that unload offscreen cards do not lose entries)
  // =========================================================================
  const seen = new Set();
  const mediaEntries = [];
  const host = location.hostname;
  const site = /(^|\.)bing\./i.test(host)
    ? "bing"
    : /(^|\.)google\./i.test(host)
      ? "google"
      : "generic";

  function urlKey(url) {
    try {
      const u = new URL(url, location.href);
      u.hash = "";
      return u.href;
    } catch {
      return url;
    }
  }

  function push(parsed) {
    if (!parsed || !parsed.url) return;
    if (CONFIG.maxItems && mediaEntries.length >= CONFIG.maxItems) return;
    const key = urlKey(parsed.url);
    if (seen.has(key)) return;
    seen.add(key);
    mediaEntries.push(parsed);
  }

  function collectNow() {
    const before = mediaEntries.length;
    if (site === "bing") collectBing(push);
    else if (site === "google") collectGoogle(push);
    else collectGeneric(push);
    collectVideos(push, site);
    return mediaEntries.length - before;
  }

  console.log(
    site === "bing" ? "🟦 Bing Images mode" : site === "google" ? "🟨 Google Images mode" : "🌐 Generic site mode"
  );
  console.log("🔄 Deep-scrolling and collecting incrementally...");

  let lastHeight = document.body.scrollHeight;
  let stableCount = 0;
  let rounds = 0;
  collectNow();

  while (stableCount < CONFIG.stableThreshold && rounds < CONFIG.maxScrollRounds) {
    if (CONFIG.maxItems && mediaEntries.length >= CONFIG.maxItems) {
      console.log(`🛑 maxItems (${CONFIG.maxItems}) reached — stopping scroll`);
      break;
    }
    window.scrollTo({ top: document.body.scrollHeight, behavior: "smooth" });
    await sleep(CONFIG.scrollDelay);
    rounds += 1;

    const added = collectNow();
    const newHeight = document.body.scrollHeight;
    if (newHeight === lastHeight && added === 0) {
      stableCount += 1;
    } else {
      stableCount = 0;
      lastHeight = newHeight;
    }
    if (rounds % 5 === 0) {
      log(`   …round ${rounds}: ${mediaEntries.length} items so far`);
    }
  }
  window.scrollTo(0, 0);
  await sleep(400);
  collectNow(); // final sweep back at the top

  console.table(
    mediaEntries.slice(0, 200).map((x) => ({
      title: x.suggestedName,
      via: x.titleSource,
      type: x.type,
      src: x.source,
      url: x.url.substring(0, 72),
    }))
  );
  console.log(`✅ Collected ${mediaEntries.length} media items (${rounds} scroll rounds)`);

  if (mediaEntries.length === 0) {
    alert("⚠️ No media found. Try the image results list page (not the viewer).");
    return;
  }

  // =========================================================================
  // EXPORT MANIFEST (recommended — avoids .crdownload / CORS issues)
  // =========================================================================
  const manifest = {
    version: 5,
    schemaVersion: "image-title-scraper.manifest.v2",
    generatedAt: new Date().toISOString(),
    pageUrl: location.href,
    pageTitle: document.title,
    site,
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
      referer: e.referer,
      sourcePage: e.sourcePage || e.referer || location.href,
      thumbnail: e.thumbnail || "",
      width: e.width || undefined,
      height: e.height || undefined,
    })),
  };

  const jsonText = JSON.stringify(manifest, null, 2);

  function toCsv() {
    const escape = (v) => `"${String(v ?? "").replace(/"/g, '""')}"`;
    const rows = [["index", "title", "type", "source", "titleSource", "url", "referer"]];
    for (const item of manifest.items) {
      rows.push([item.index, item.title, item.type, item.source, item.titleSource, item.url, item.referer]);
    }
    return rows.map((r) => r.map(escape).join(",")).join("\n");
  }

  function downloadTextFile(text, filename, mime) {
    const blob = new Blob([text], { type: mime });
    const blobUrl = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = blobUrl;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    setTimeout(() => {
      a.remove();
      URL.revokeObjectURL(blobUrl);
    }, 2000);
  }

  async function copyManifest() {
    let copied = false;
    try {
      await navigator.clipboard.writeText(jsonText);
      console.log("📋 Manifest JSON copied to clipboard");
      console.log(
        "💡 Tip: in console run copy(JSON.stringify(window.__IMAGE_TITLE_MANIFEST__, null, 2)) if needed."
      );
      copied = true;
    } catch {
      downloadTextFile(jsonText, `image-title-manifest_${Date.now()}.json`, "application/json");
      console.log("💾 Manifest JSON file download triggered (clipboard unavailable)");
    }
    if (CONFIG.exportCsv) {
      downloadTextFile(toCsv(), `image-title-manifest_${Date.now()}.csv`, "text/csv");
      console.log("💾 CSV manifest download triggered");
    }
    return copied;
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
    const stopMonitor = setInterval(() => {
      if (window.__SCRAPER_STOP__) controller.abort();
    }, 250);
    try {
      const resp = await fetch(entry.url, {
        mode: "cors",
        credentials: "omit",
        signal: controller.signal,
      });
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
    } finally {
      clearTimeout(timeout);
      clearInterval(stopMonitor);
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

  if (choice === "export" || choice === "both") {
    const copied = await copyManifest();
    console.log(
      copied
        ? "➡️ Next: save clipboard JSON as manifest.json, then run:\n" +
            "   python download.py manifest.json"
        : "➡️ Next: run download.py with the downloaded JSON manifest."
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
    console.warn("🛑 To abort mid-run: window.__SCRAPER_STOP__ = true");
    window.__SCRAPER_STOP__ = false;

    let ok = 0;
    let fail = 0;
    const failed = [];

    for (let i = 0; i < mediaEntries.length; i++) {
      if (window.__SCRAPER_STOP__) {
        console.warn(`🛑 Stopped by user at item ${i + 1}/${mediaEntries.length}`);
        break;
      }
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
