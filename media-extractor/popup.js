"use strict";

const state = {
  resources: [],
  pageTitle: "",
  activeTabId: null
};

const elements = {
  autoScroll: document.querySelector("#auto-scroll"),
  computedStyles: document.querySelector("#computed-styles"),
  download: document.querySelector("#download"),
  list: document.querySelector("#resource-list"),
  results: document.querySelector("#results"),
  scan: document.querySelector("#scan"),
  selectAll: document.querySelector("#select-all"),
  status: document.querySelector("#status"),
  typeFilter: document.querySelector("#type-filter")
};

function setStatus(message, error = false) {
  elements.status.textContent = message;
  elements.status.classList.toggle("error", error);
}

function visibleResources() {
  const filter = elements.typeFilter.value;
  return state.resources.filter((resource) => filter === "all" || resource.type === filter);
}

function renderResources() {
  elements.list.replaceChildren();
  const filter = elements.typeFilter.value;

  state.resources.forEach((resource, index) => {
    if (filter !== "all" && resource.type !== filter) return;

    const item = document.createElement("li");
    const checkbox = document.createElement("input");
    checkbox.type = "checkbox";
    checkbox.checked = true;
    checkbox.dataset.index = String(index);
    checkbox.setAttribute("aria-label", `Select ${resource.title}`);

    let preview;
    if (resource.type === "image") {
      preview = document.createElement("img");
      preview.src = resource.url;
      preview.alt = "";
      preview.referrerPolicy = "no-referrer";
    } else {
      preview = document.createElement("span");
      preview.className = "media-icon";
      preview.textContent = "🎬";
      preview.setAttribute("aria-hidden", "true");
    }

    const copy = document.createElement("div");
    copy.className = "resource-copy";
    const title = document.createElement("strong");
    title.textContent = resource.title;
    title.title = resource.title;
    const details = document.createElement("small");
    details.textContent = `${resource.type} · ${resource.source}`;
    details.title = resource.url;
    copy.append(title, details);
    item.append(checkbox, preview, copy);
    elements.list.append(item);
  });

  const count = visibleResources().length;
  elements.selectAll.checked = true;
  elements.download.disabled = count === 0;
  elements.download.textContent = `Download selected (${count})`;
}

async function sendScan(tabId, options) {
  try {
    return await chrome.tabs.sendMessage(tabId, { type: "SCAN_PAGE_MEDIA", options });
  } catch {
    await chrome.scripting.executeScript({
      target: { tabId },
      files: ["extractor.js", "content.js"]
    });
    return chrome.tabs.sendMessage(tabId, { type: "SCAN_PAGE_MEDIA", options });
  }
}

async function scanPage() {
  elements.scan.disabled = true;
  elements.results.hidden = true;
  setStatus("Scanning the current page…");

  try {
    const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
    if (!tab?.id || !/^https?:/i.test(tab.url || "")) {
      throw new Error("Open a regular HTTP or HTTPS page before scanning.");
    }
    state.activeTabId = tab.id;
    const response = await sendScan(tab.id, {
      autoScroll: elements.autoScroll.checked,
      includeComputedStyles: elements.computedStyles.checked
    });
    if (response?.error) throw new Error(response.error);
    state.resources = response?.resources || [];
    state.pageTitle = response?.pageTitle || tab.title || "Untitled page";
    renderResources();
    elements.results.hidden = false;
    const images = state.resources.filter((item) => item.type === "image").length;
    const videos = state.resources.length - images;
    setStatus(`Found ${images} images and ${videos} videos.`);
  } catch (error) {
    setStatus(error.message || "The page could not be scanned.", true);
  } finally {
    elements.scan.disabled = false;
  }
}

async function downloadSelected() {
  const selected = [...elements.list.querySelectorAll("input[type='checkbox']:checked")]
    .map((checkbox) => state.resources[Number(checkbox.dataset.index)])
    .filter(Boolean);
  if (!selected.length) {
    setStatus("Select at least one resource.", true);
    return;
  }

  elements.download.disabled = true;
  try {
    const result = await chrome.runtime.sendMessage({
      type: "START_DOWNLOADS",
      resources: selected,
      pageTitle: state.pageTitle
    });
    setStatus(`Queued 0 of ${result.total} downloads…`);
  } catch (error) {
    setStatus(error.message || "Downloads could not be started.", true);
    elements.download.disabled = false;
  }
}

elements.scan.addEventListener("click", scanPage);
elements.typeFilter.addEventListener("change", renderResources);
elements.selectAll.addEventListener("change", () => {
  elements.list.querySelectorAll("input[type='checkbox']")
    .forEach((checkbox) => { checkbox.checked = elements.selectAll.checked; });
});
elements.list.addEventListener("change", () => {
  const checkboxes = [...elements.list.querySelectorAll("input[type='checkbox']")];
  const selected = checkboxes.filter((checkbox) => checkbox.checked).length;
  elements.selectAll.checked = selected === checkboxes.length;
  elements.selectAll.indeterminate = selected > 0 && selected < checkboxes.length;
  elements.download.textContent = `Download selected (${selected})`;
});
elements.download.addEventListener("click", downloadSelected);

chrome.runtime.onMessage.addListener((message) => {
  if (message?.type !== "DOWNLOAD_PROGRESS") return;
  const job = message.job;
  if (job.status === "completed" || job.status === "completed-with-errors") {
    setStatus(`Queued ${job.queued} downloads; ${job.failed} rejected.`);
    elements.download.disabled = false;
  } else if (job.status === "failed") {
    setStatus(job.errors.at(-1)?.message || "The download job failed.", true);
    elements.download.disabled = false;
  } else {
    setStatus(`Queued ${job.queued} of ${job.total}; ${job.failed} rejected.`);
  }
});

scanPage();
