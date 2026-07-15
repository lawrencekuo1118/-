(function (root, factory) {
  const api = factory();
  if (typeof module === "object" && module.exports) module.exports = api;
  if (root) root.MediaExtractor = api;
})(typeof globalThis !== "undefined" ? globalThis : this, function () {
  "use strict";

  const MEDIA_EXTENSION = /\.(avif|bmp|gif|heic|jpe?g|m3u8|m4v|mov|mp4|mpeg|oga|ogg|ogv|png|svg|tiff?|ts|webm|webp)(?:$|[?#])/i;
  const VIDEO_EXTENSION = /\.(m3u8|m4v|mov|mp4|mpeg|oga|ogg|ogv|ts|webm)(?:$|[?#])/i;
  const GENERIC_TITLES = new Set([
    "download", "image", "img", "loading", "media", "photo", "picture",
    "thumbnail", "untitled", "video", "view image"
  ]);
  const TRACKER_PARTS = [
    "analytics", "bat.bing.com", "doubleclick", "facebook.com/tr",
    "googleads", "/pixel", "pixel.", "/tracker"
  ];

  function cleanText(value) {
    return String(value || "")
      .replace(/[\u0000-\u001f\u007f]/g, " ")
      .replace(/\s+/g, " ")
      .trim();
  }

  function isUsefulTitle(value) {
    const text = cleanText(value);
    if (text.length < 2 || text.length > 300) return false;
    const lower = text.toLocaleLowerCase();
    return !GENERIC_TITLES.has(lower) && !/^\d+$/.test(lower);
  }

  function pickTitle(candidates, fallback) {
    const usable = candidates
      .map((candidate) => ({
        text: cleanText(candidate && candidate.text),
        score: Number(candidate && candidate.score) || 0
      }))
      .filter((candidate) => isUsefulTitle(candidate.text))
      .sort((a, b) => b.score - a.score || b.text.length - a.text.length);
    return usable[0] ? usable[0].text : cleanText(fallback) || "media";
  }

  function sanitizeFilename(value, maxLength = 120) {
    let name = cleanText(value)
      .replace(/[\\/:*?"<>|]/g, "")
      .replace(/[. ]+$/g, "")
      .replace(/\s+/g, "_");
    if (!name) name = "media";
    if (/^(con|prn|aux|nul|com[1-9]|lpt[1-9])(?:\.|$)/i.test(name)) {
      name = `_${name}`;
    }
    return Array.from(name).slice(0, maxLength).join("");
  }

  function absoluteUrl(value, baseUrl) {
    const text = cleanText(value).replace(/&amp;/g, "&");
    if (!text || /^(?:data|blob|javascript):/i.test(text)) return "";
    try {
      const url = new URL(text, baseUrl);
      return /^https?:$/i.test(url.protocol) ? url.href : "";
    } catch {
      return "";
    }
  }

  function filenameFromUrl(url) {
    try {
      const segment = new URL(url).pathname.split("/").pop() || "";
      return decodeURIComponent(segment)
        .replace(MEDIA_EXTENSION, "")
        .replace(/[-_]+/g, " ");
    } catch {
      return "";
    }
  }

  function mediaType(url, hint) {
    if (hint === "video" || VIDEO_EXTENSION.test(url)) return "video";
    return "image";
  }

  function extensionFromUrl(url) {
    const match = String(url).match(MEDIA_EXTENSION);
    return match ? match[1].toLocaleLowerCase().replace("jpeg", "jpg") : "";
  }

  function srcsetUrls(value, baseUrl) {
    return String(value || "")
      .split(",")
      .map((part) => part.trim().split(/\s+/)[0])
      .map((url) => absoluteUrl(url, baseUrl))
      .filter(Boolean);
  }

  function cssUrls(value, baseUrl) {
    const urls = [];
    const pattern = /url\(\s*(['"]?)(.*?)\1\s*\)/gi;
    let match;
    while ((match = pattern.exec(String(value || "")))) {
      const url = absoluteUrl(match[2], baseUrl);
      if (url) urls.push(url);
    }
    return urls;
  }

  function titleCandidates(element, extra) {
    const candidates = [...(extra || [])];
    if (!element) return candidates;
    candidates.push(
      { text: element.getAttribute && element.getAttribute("aria-label"), score: 92 },
      { text: element.getAttribute && element.getAttribute("alt"), score: 90 },
      { text: element.getAttribute && element.getAttribute("title"), score: 88 }
    );
    const link = element.closest && element.closest("a");
    const figure = element.closest && element.closest("figure");
    const container = element.closest && element.closest("article, li, [class*='card'], [class*='item']");
    candidates.push(
      { text: link && link.getAttribute("title"), score: 86 },
      { text: link && link.getAttribute("aria-label"), score: 84 },
      { text: figure && figure.querySelector("figcaption")?.textContent, score: 96 },
      { text: container && container.querySelector("h1,h2,h3,h4")?.textContent, score: 78 }
    );
    return candidates;
  }

  function extractMedia(doc, options = {}) {
    const baseUrl = options.baseUrl || doc.baseURI || doc.location?.href || "";
    const includeComputedStyles = options.includeComputedStyles !== false;
    const includePerformance = options.includePerformance !== false;
    const resources = new Map();

    function add(rawUrl, details = {}) {
      const url = absoluteUrl(rawUrl, baseUrl);
      if (!url || TRACKER_PARTS.some((part) => url.toLocaleLowerCase().includes(part))) return;
      const type = mediaType(url, details.type);
      if (!details.allowUnknown && !MEDIA_EXTENSION.test(url) && !details.element) return;
      const fallback = filenameFromUrl(url);
      const title = pickTitle(
        titleCandidates(details.element, details.candidates),
        fallback
      );
      const existing = resources.get(url);
      const entry = {
        url,
        type,
        title,
        filename: sanitizeFilename(title),
        extension: extensionFromUrl(url),
        source: details.source || "page"
      };
      if (!existing || isUsefulTitle(title) && !isUsefulTitle(existing.title)) {
        resources.set(url, entry);
      }
    }

    doc.querySelectorAll("img, video, audio, source, input[type='image']").forEach((element) => {
      const tag = element.tagName.toLocaleLowerCase();
      const type = ["video", "audio"].includes(tag) ||
        String(element.getAttribute("type") || "").startsWith("video/") ? "video" : "image";
      [
        element.currentSrc,
        element.src,
        element.getAttribute("src"),
        element.getAttribute("data-src"),
        element.getAttribute("data-original"),
        element.getAttribute("data-url"),
        element.getAttribute("data-lazy-src")
      ].forEach((url) => add(url, { element, type, source: "dom", allowUnknown: true }));
      ["srcset", "data-srcset"].forEach((attribute) => {
        srcsetUrls(element.getAttribute(attribute), baseUrl)
          .forEach((url) => add(url, { element, type, source: attribute, allowUnknown: true }));
      });
      if (tag === "video") {
        add(element.poster || element.getAttribute("poster"), {
          element, type: "image", source: "poster", allowUnknown: true
        });
      }
    });

    doc.querySelectorAll("a[href], link[href]").forEach((element) => {
      const href = element.getAttribute("href");
      if (MEDIA_EXTENSION.test(href || "")) {
        add(href, { element, source: "link" });
      }
    });

    doc.querySelectorAll(".iusc[m]").forEach((element) => {
      try {
        const metadata = JSON.parse(element.getAttribute("m") || "{}");
        add(metadata.murl || metadata.imgurl, {
          element,
          type: "image",
          source: "bing-metadata",
          allowUnknown: true,
          candidates: [
            { text: metadata.t, score: 110 },
            { text: metadata.title, score: 108 }
          ]
        });
      } catch {
        // Ignore malformed third-party metadata.
      }
    });

    doc.querySelectorAll("[style*='url(']").forEach((element) => {
      cssUrls(element.getAttribute("style"), baseUrl)
        .forEach((url) => add(url, { element, source: "inline-style" }));
    });

    if (includeComputedStyles && doc.defaultView?.getComputedStyle) {
      doc.querySelectorAll("body *").forEach((element) => {
        const style = doc.defaultView.getComputedStyle(element);
        [style.backgroundImage, style.content, style.cursor].forEach((value) => {
          cssUrls(value, baseUrl)
            .forEach((url) => add(url, { element, source: "computed-style" }));
        });
      });
    }

    const markup = doc.documentElement?.innerHTML || "";
    const rawUrlPattern = /(?:https?:)?\/\/[^\s"'<>()[\]{}\\]+?\.(?:avif|bmp|gif|heic|jpe?g|m3u8|m4v|mov|mp4|mpeg|oga|ogg|ogv|png|svg|tiff?|ts|webm|webp)(?:\?[^\s"'<>()[\]{}\\]*)?/gi;
    (markup.match(rawUrlPattern) || []).forEach((url) => add(url, { source: "markup" }));

    if (includePerformance && doc.defaultView?.performance?.getEntriesByType) {
      doc.defaultView.performance.getEntriesByType("resource").forEach((entry) => {
        if (MEDIA_EXTENSION.test(entry.name || "")) {
          add(entry.name, { source: "performance" });
        }
      });
    }

    return [...resources.values()].sort((a, b) =>
      a.type.localeCompare(b.type) || a.filename.localeCompare(b.filename)
    );
  }

  return {
    absoluteUrl,
    cssUrls,
    extractMedia,
    extensionFromUrl,
    filenameFromUrl,
    isUsefulTitle,
    mediaType,
    pickTitle,
    sanitizeFilename,
    srcsetUrls
  };
});
