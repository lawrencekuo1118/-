"use strict";

const state = {
  resources: [],
  pageTitle: "",
  activeTabId: null,
  selected: new Set(),
  currentJobId: null
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

function renderResources() {
  elements.list.replaceChildren();
  const filter = elements.typeFilter.value;

  state.resources.forEach((resource, index) => {
    if (filter !== "all" && resource.type !== filter) return;

    const item = document.createElement("li");
    const checkbox = document.createElement("input");
    checkbox.type = "checkbox";
    checkbox.checked = state.selected.has(index);
    checkbox.dataset.index = String(index);
    checkbox.setAttribute("aria-label", `Select ${resource.title}`);

    const preview = document.createElement("span");
    preview.className = "media-icon";
    preview.textContent = resource.type === "image" ? "🖼️" : "🎬";
    preview.setAttribute("aria-hidden", "true");

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

  const visibleIndexes = state.resources
    .map((resource, index) => ({ resource, index }))
    .filter(({ resource }) => filter === "all" || resource.type === filter)
    .map(({ index }) => index);
  const visibleSelected = visibleIndexes.filter((index) => state.selected.has(index)).length;
  elements.selectAll.checked = visibleIndexes.length > 0 && visibleSelected === visibleIndexes.length;
  elements.selectAll.indeterminate = visibleSelected > 0 && visibleSelected < visibleIndexes.length;
  elements.download.disabled = state.selected.size === 0 || Boolean(state.currentJobId);
  elements.download.textContent = `Download selected (${state.selected.size})`;
}

async function sendScan(tabId, options) {
  await chrome.scripting.executeScript({
    target: { tabId },
    files: ["extractor.js", "content.js"]
  });
  return chrome.tabs.sendMessage(tabId, { type: "SCAN_PAGE_MEDIA", options });
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
    state.selected = new Set(state.resources.map((_resource, index) => index));
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
  const selected = [...state.selected]
    .map((index) => state.resources[index])
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
    if (result.error) throw new Error(result.error);
    state.currentJobId = result.jobId;
    setStatus(`Started 0 of ${result.total} downloads…`);
  } catch (error) {
    setStatus(error.message || "Downloads could not be started.", true);
    elements.download.disabled = false;
  }
}

elements.scan.addEventListener("click", scanPage);
elements.typeFilter.addEventListener("change", renderResources);
elements.selectAll.addEventListener("change", () => {
  elements.list.querySelectorAll("input[type='checkbox']").forEach((checkbox) => {
    const index = Number(checkbox.dataset.index);
    if (elements.selectAll.checked) state.selected.add(index);
    else state.selected.delete(index);
  });
  renderResources();
});
elements.list.addEventListener("change", (event) => {
  if (!event.target.matches("input[type='checkbox']")) return;
  const index = Number(event.target.dataset.index);
  if (event.target.checked) state.selected.add(index);
  else state.selected.delete(index);
  renderResources();
});
elements.download.addEventListener("click", downloadSelected);

chrome.runtime.onMessage.addListener((message) => {
  if (message?.type !== "DOWNLOAD_PROGRESS") return;
  const job = message.job;
  if (state.currentJobId && job.id !== state.currentJobId) return;
  state.currentJobId = job.id;
  if (job.status === "completed" || job.status === "completed-with-errors") {
    setStatus(`Downloaded ${job.completed} files; ${job.failed} failed.`);
    state.currentJobId = null;
    elements.download.disabled = false;
  } else if (job.status === "failed") {
    setStatus(job.errors.at(-1)?.message || "The download job failed.", true);
    state.currentJobId = null;
    elements.download.disabled = false;
  } else {
    setStatus(`Downloaded ${job.completed} of ${job.total}; ${job.failed} failed.`);
  }
});

async function restoreDownloadJob() {
  const job = await chrome.runtime.sendMessage({ type: "GET_DOWNLOAD_JOB" });
  if (!job || !["queued", "running"].includes(job.status)) return;
  state.currentJobId = job.id;
  elements.download.disabled = true;
  setStatus(`Downloaded ${job.completed} of ${job.total}; ${job.failed} failed.`);
}

scanPage().then(restoreDownloadJob);
