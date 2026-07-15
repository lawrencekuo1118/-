/* global chrome */
"use strict";

const JOB_KEY = "mediaDownloadJob";
let processing = false;

function safePart(value, fallback) {
  const cleaned = String(value || "")
    .replace(/[\u0000-\u001f\u007f\\/:*?"<>|]/g, " ")
    .replace(/\s+/g, " ")
    .replace(/[. ]+$/g, "")
    .trim();
  return (cleaned || fallback).slice(0, 100);
}

function downloadUrl(options) {
  return new Promise((resolve, reject) => {
    chrome.downloads.download(options, (downloadId) => {
      const error = chrome.runtime.lastError;
      if (error || downloadId === undefined) {
        reject(new Error(error?.message || "The browser rejected the download."));
      } else {
        resolve(downloadId);
      }
    });
  });
}

function snapshot(job) {
  return {
    id: job.id,
    total: job.total,
    started: job.started,
    completed: job.completed,
    failed: job.failed,
    status: job.status,
    errors: job.errors.slice(-20)
  };
}

async function saveAndPublish(job) {
  const publicJob = snapshot(job);
  await chrome.storage.local.set({
    [JOB_KEY]: job,
    latestDownloadJob: publicJob
  });
  chrome.runtime.sendMessage({ type: "DOWNLOAD_PROGRESS", job: publicJob }).catch(() => {});
}

async function getJob() {
  const result = await chrome.storage.local.get(JOB_KEY);
  return result[JOB_KEY] || null;
}

function scheduleNext() {
  chrome.alarms.create("continueMediaDownloads", { when: Date.now() + 500 });
  setTimeout(runNext, 300);
}

async function settleDownload(job, state, errorMessage) {
  if (!job.pending) return;
  if (state === "complete") {
    job.completed += 1;
  } else {
    job.failed += 1;
    job.errors.push({
      url: job.pending.url,
      message: errorMessage || "The download was interrupted."
    });
  }
  job.nextIndex += 1;
  job.pending = null;
  await saveAndPublish(job);
  scheduleNext();
}

async function runNext() {
  if (processing) return;
  processing = true;
  try {
    const job = await getJob();
    if (!job || !["queued", "running"].includes(job.status)) return;

    if (job.pending) {
      const [item] = await chrome.downloads.search({ id: job.pending.downloadId });
      if (item?.state === "complete" || item?.state === "interrupted") {
        await settleDownload(job, item.state, item.error);
      }
      return;
    }

    if (job.nextIndex >= job.total) {
      job.status = job.failed ? "completed-with-errors" : "completed";
      await saveAndPublish(job);
      await chrome.storage.local.remove(JOB_KEY);
      return;
    }

    const index = job.nextIndex;
    const resource = job.resources[index];
    const extension = /^[a-z0-9]{2,5}$/i.test(resource.extension || "")
      ? resource.extension.toLocaleLowerCase()
      : resource.type === "video" ? "mp4" : "jpg";
    const ordinal = String(index + 1).padStart(4, "0");
    const filename = safePart(resource.filename, resource.type || "media");

    try {
      const downloadId = await downloadUrl({
        url: resource.url,
        filename: `${job.folder}/${filename}_${ordinal}.${extension}`,
        conflictAction: "uniquify",
        saveAs: false
      });
      job.started += 1;
      job.status = "running";
      job.pending = { downloadId, url: resource.url };
      await saveAndPublish(job);

      // Catch downloads that finished before the event listener observed them.
      const [item] = await chrome.downloads.search({ id: downloadId });
      if (item?.state === "complete" || item?.state === "interrupted") {
        await settleDownload(job, item.state, item.error);
      }
    } catch (error) {
      job.failed += 1;
      job.nextIndex += 1;
      job.errors.push({ url: resource.url, message: error.message });
      await saveAndPublish(job);
      scheduleNext();
    }
  } catch (error) {
    const job = await getJob();
    if (job) {
      job.status = "failed";
      job.errors.push({ message: error.message });
      await saveAndPublish(job);
    }
  } finally {
    processing = false;
  }
}

chrome.downloads.onChanged.addListener((delta) => {
  const state = delta.state?.current;
  if (state !== "complete" && state !== "interrupted") return;
  (async () => {
    const job = await getJob();
    if (!job?.pending || job.pending.downloadId !== delta.id) return;
    await settleDownload(job, state, delta.error?.current);
  })().catch(() => {});
});

chrome.alarms.onAlarm.addListener((alarm) => {
  if (alarm.name === "continueMediaDownloads") runNext();
});

chrome.runtime.onStartup.addListener(runNext);
chrome.runtime.onInstalled.addListener(runNext);

chrome.runtime.onMessage.addListener((message, _sender, sendResponse) => {
  if (message?.type === "START_DOWNLOADS") {
    (async () => {
      const existing = await getJob();
      if (existing && ["queued", "running"].includes(existing.status)) {
        sendResponse({ error: "A download job is already running.", job: snapshot(existing) });
        return;
      }

      const resources = Array.isArray(message.resources)
        ? message.resources.filter((item) => /^https?:\/\//i.test(item?.url || ""))
        : [];
      const id = crypto.randomUUID();
      const job = {
        id,
        total: resources.length,
        started: 0,
        completed: 0,
        failed: 0,
        nextIndex: 0,
        pending: null,
        status: "queued",
        errors: [],
        folder: `Page Media/${safePart(message.pageTitle, "Untitled page")}`,
        resources
      };
      await saveAndPublish(job);
      sendResponse({ jobId: id, total: resources.length });
      runNext();
    })().catch((error) => sendResponse({ error: error.message }));
    return true;
  }

  if (message?.type === "GET_DOWNLOAD_JOB") {
    chrome.storage.local.get("latestDownloadJob", (result) => {
      sendResponse(result.latestDownloadJob || null);
    });
    return true;
  }

  return false;
});

// Resume persisted work whenever an event wakes the service worker.
runNext();
