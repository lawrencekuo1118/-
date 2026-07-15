"use strict";

const test = require("node:test");
const assert = require("node:assert/strict");
const extractor = require("../extractor.js");

test("absoluteUrl resolves relative links and rejects unsafe schemes", () => {
  assert.equal(
    extractor.absoluteUrl("../image.jpg", "https://example.com/articles/page/"),
    "https://example.com/articles/image.jpg"
  );
  assert.equal(extractor.absoluteUrl("data:image/png;base64,abc", "https://example.com"), "");
  assert.equal(extractor.absoluteUrl("javascript:alert(1)", "https://example.com"), "");
});

test("sanitizeFilename produces portable bounded names", () => {
  assert.equal(
    extractor.sanitizeFilename('  An <invalid>: "title".  '),
    "An_invalid_title"
  );
  assert.equal(extractor.sanitizeFilename("CON"), "_CON");
  assert.equal(extractor.sanitizeFilename("abcdef", 4), "abcd");
});

test("pickTitle prefers meaningful high-scoring candidates", () => {
  assert.equal(
    extractor.pickTitle([
      { text: "image", score: 200 },
      { text: "A useful caption", score: 90 },
      { text: "Lower priority", score: 50 }
    ], "fallback"),
    "A useful caption"
  );
});

test("srcsetUrls and cssUrls resolve every discovered URL", () => {
  assert.deepEqual(
    extractor.srcsetUrls("/small.jpg 1x, /large.jpg 2x", "https://example.com/page"),
    ["https://example.com/small.jpg", "https://example.com/large.jpg"]
  );
  assert.deepEqual(
    extractor.cssUrls('linear-gradient(#000,#fff), url("../hero.webp")', "https://example.com/a/"),
    ["https://example.com/hero.webp"]
  );
});

test("extractMedia finds and deduplicates media URLs embedded in markup", () => {
  const document = {
    baseURI: "https://example.com/gallery/",
    querySelectorAll() {
      return [];
    },
    documentElement: {
      innerHTML: [
        '<script>window.asset = "https://cdn.example.com/photo.webp?size=2"</script>',
        '<a href="https://cdn.example.com/photo.webp?size=2">duplicate</a>',
        '<script>window.video = "//cdn.example.com/movie.mp4"</script>'
      ].join("")
    }
  };

  const resources = extractor.extractMedia(document, {
    includeComputedStyles: false,
    includePerformance: false
  });

  assert.equal(resources.length, 2);
  assert.deepEqual(resources.map((item) => item.type), ["image", "video"]);
  assert.equal(resources[0].extension, "webp");
  assert.equal(resources[1].url, "https://cdn.example.com/movie.mp4");
});
