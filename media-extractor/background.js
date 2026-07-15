/* global chrome */
"use strict";

const activeJobs = new Map();

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

function publish(job) {
  const snapshot = {
    id: job.id,
    total: job.total,
    queued: job.queued,
    failed: job.failed,
    status: job.status,
    errors: job.errors.slice(-20)
  };
  chrome.storage.local.set({ latestDownloadJob: snapshot });
  chrome.runtime.sendMessage({ type: "DOWNLOAD_PROGRESS", job: snapshot }).catch(() => {});
}

async function runJob(job, resources, pageTitle) {
  const folder = `Page Media/${safePart(pageTitle, "Untitled page")}`;
  job.status = "running";
  publish(job);

  for (let index = 0; index < resources.length; index += 1) {
    const resource = resources[index];
    const extension = /^[a-z0-9]{2,5}$/i.test(resource.extension || "")
      ? `.${resource.extension.toLocaleLowerCase()}`
      : "";
    const ordinal = String(index + 1).padStart(4, "0");
    const filename = safePart(resource.filename, resource.type || "media");

    try {
      await downloadUrl({
        url: resource.url,
        filename: `${folder}/${filename}_${ordinal}${extension}`,
        conflictAction: "uniquify",
        saveAs: false
      });
      job.queued += 1;
    } catch (error) {
      job.failed += 1;
      job.errors.push({
        url: resource.url,
        message: error.message
      });
    }

    publish(job);
    await new Promise((resolve) => setTimeout(resolve, 250));
  }

  job.status = job.failed ? "completed-with-errors" : "completed";
  publish(job);
  activeJobs.delete(job.id);
}

chrome.runtime.onMessage.addListener((message, _sender, sendResponse) => {
  if (message?.type === "START_DOWNLOADS") {
    const resources = Array.isArray(message.resources)
      ? message.resources.filter((item) => /^https?:\/\//i.test(item?.url || ""))
      : [];
    const id = crypto.randomUUID();
    const job = {
      id,
      total: resources.length,
      queued: 0,
      failed: 0,
      status: "queued",
      errors: []
    };
    activeJobs.set(id, job);
    runJob(job, resources, message.pageTitle).catch((error) => {
      job.status = "failed";
      job.errors.push({ message: error.message });
      publish(job);
      activeJobs.delete(id);
    });
    sendResponse({ jobId: id, total: resources.length });
    return false;
  }

  if (message?.type === "GET_DOWNLOAD_JOB") {
    chrome.storage.local.get("latestDownloadJob", (result) => {
      sendResponse(result.latestDownloadJob || null);
    });
    return true;
  }

  return false;
});
