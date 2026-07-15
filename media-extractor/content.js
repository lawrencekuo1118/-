(function () {
  "use strict";

  const sleep = (milliseconds) =>
    new Promise((resolve) => setTimeout(resolve, milliseconds));

  async function expandLazyContent(options) {
    if (!options.autoScroll) return;
    const originalX = window.scrollX;
    const originalY = window.scrollY;
    let previousHeight = 0;
    let stablePasses = 0;

    for (let pass = 0; pass < 40 && stablePasses < 3; pass += 1) {
      window.scrollTo({ top: document.documentElement.scrollHeight, behavior: "instant" });
      await sleep(750);
      const height = document.documentElement.scrollHeight;
      stablePasses = height === previousHeight ? stablePasses + 1 : 0;
      previousHeight = height;
    }

    window.scrollTo({ left: originalX, top: originalY, behavior: "instant" });
    await sleep(100);
  }

  chrome.runtime.onMessage.addListener((message, _sender, sendResponse) => {
    if (message?.type !== "SCAN_PAGE_MEDIA") return false;

    (async () => {
      await expandLazyContent(message.options || {});
      const resources = globalThis.MediaExtractor.extractMedia(document, {
        includeComputedStyles: message.options?.includeComputedStyles !== false,
        includePerformance: true
      });
      return {
        resources,
        pageTitle: document.title,
        pageUrl: location.href
      };
    })()
      .then(sendResponse)
      .catch((error) => sendResponse({ error: error.message }));

    return true;
  });
})();
