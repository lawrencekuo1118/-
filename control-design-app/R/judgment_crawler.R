# 司法院裁判書查詢（Default_AD.aspx）爬蟲 + 判決內文 + 結果分析
# 資料來源：https://judgment.judicial.gov.tw/FJUD/Default_AD.aspx（非官方 API）

JUDGMENT_SEARCH_URL <- "https://judgment.judicial.gov.tw/FJUD/Default_AD.aspx"
JUDGMENT_SITE_ORIGIN <- "https://judgment.judicial.gov.tw"

# 對齊進階查詢表單 jud_court（value → 標籤）
JUDGMENT_COURT_CHOICES <- c(
  "所有法院" = "",
  "憲法法庭" = "JCC",
  "司法院刑事補償法庭" = "TPC",
  "司法院－訴願決定" = "TPU",
  "最高法院" = "TPS",
  "最高行政法院(含改制前行政法院)" = "TPA",
  "懲戒法院－懲戒法庭" = "TPP",
  "懲戒法院－職務法庭" = "TPJ",
  "臺灣高等法院" = "TPH",
  "臺灣高等法院－訴願決定" = "001",
  "臺北高等行政法院 高等庭" = "TPB",
  "臺北高等行政法院 地方庭" = "TPT",
  "臺中高等行政法院 高等庭" = "TCB",
  "臺中高等行政法院 地方庭" = "TCT",
  "高雄高等行政法院 高等庭" = "KSB",
  "高雄高等行政法院 地方庭" = "KST",
  "智慧財產及商業法院" = "IPC",
  "臺灣高等法院 臺中分院" = "TCH",
  "臺灣高等法院 臺南分院" = "TNH",
  "臺灣高等法院 高雄分院" = "KSH",
  "臺灣高等法院 花蓮分院" = "HLH",
  "臺灣臺北地方法院" = "TPD",
  "臺灣士林地方法院" = "SLD",
  "臺灣新北地方法院" = "PCD",
  "臺灣宜蘭地方法院" = "ILD",
  "臺灣基隆地方法院" = "KLD",
  "臺灣桃園地方法院" = "TYD",
  "臺灣新竹地方法院" = "SCD",
  "臺灣苗栗地方法院" = "MLD",
  "臺灣臺中地方法院" = "TCD",
  "臺灣彰化地方法院" = "CHD",
  "臺灣南投地方法院" = "NTD",
  "臺灣雲林地方法院" = "ULD",
  "臺灣嘉義地方法院" = "CYD",
  "臺灣臺南地方法院" = "TND",
  "臺灣高雄地方法院" = "KSD",
  "臺灣橋頭地方法院" = "CTD",
  "臺灣花蓮地方法院" = "HLD",
  "臺灣臺東地方法院" = "TTD",
  "臺灣屏東地方法院" = "PTD",
  "臺灣澎湖地方法院" = "PHD",
  "福建高等法院金門分院" = "KMH",
  "福建金門地方法院" = "KMD",
  "福建連江地方法院" = "LCD",
  "臺灣高雄少年及家事法院" = "KSY"
)

JUDGMENT_CASE_TYPE_CHOICES <- c(
  "憲法" = "C",
  "民事" = "V",
  "刑事" = "M",
  "行政" = "A",
  "懲戒" = "P"
)

judgment_user_agent <- function() {
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
}

judgment_trim <- function(x) {
  trimws(as.character(x %||% ""))
}

judgment_date_to_form_parts <- function(x) {
  empty <- list(dy = "", dm = "", dd = "")
  if (is.null(x) || !length(x)) return(empty)
  if (length(x) > 1L) x <- x[[1]]
  if (inherits(x, "POSIXt")) x <- as.Date(x)
  if (is.character(x)) {
    x <- suppressWarnings(as.Date(x))
  }
  if (inherits(x, "Date") && is.na(x)) return(empty)
  if (!inherits(x, "Date")) return(empty)
  ad_year <- as.integer(format(x, "%Y"))
  list(
    dy = as.character(ad_year - 1911L),
    dm = as.character(as.integer(format(x, "%m"))),
    dd = as.character(as.integer(format(x, "%d")))
  )
}

judgment_format_period <- function(dy, dm, dd) {
  dy <- judgment_trim(dy)
  dm <- judgment_trim(dm)
  dd <- judgment_trim(dd)
  if (!any(nzchar(c(dy, dm, dd)))) return("")
  sprintf("民國%s年%s月%s日", dy, dm, dd)
}

judgment_extract_hidden <- function(name, html) {
  pat <- paste0('name="', name, '" id="', name, '" value="([^"]*)"')
  m <- regexpr(pat, html, perl = TRUE)
  if (m[1] == -1) return("")
  sub('.*value="', "", regmatches(html, m))
}

judgment_response_error_text <- function(html) {
  html <- judgment_trim(html)
  if (!nzchar(html)) return("未取得回應內容")
  if (grepl("連線逾時", html, fixed = TRUE)) return("司法院裁判書系統回應：連線逾時，請稍後再試或縮小查詢條件")
  if (grepl("Errorpage\\.aspx", html, ignore.case = TRUE)) return("查詢被拒或表單驗證失敗，請確認條件後重試")
  if (grepl("請輸入查詢條件", html, fixed = TRUE)) return("請至少輸入一項查詢條件")
  if (grepl("系統訊息", html, fixed = TRUE) && grepl("重新查詢", html, fixed = TRUE)) {
    return("司法院裁判書系統無法完成查詢（可能為連線限制或查詢過於寬泛）")
  }
  ""
}

judgment_validate_params <- function(params) {
  params <- as.list(params)
  msgs <- character()
  result_url <- judgment_trim(params$result_url %||% "")
  if (nzchar(result_url) &&
      !grepl("^https?://[^/]*judicial\\.gov\\.tw/.+(qryresultlst|data)\\.aspx", result_url, ignore.case = TRUE)) {
    msgs <- c(msgs, "查詢結果 URL 須為司法院 qryresultlst.aspx 或 data.aspx 完整網址")
    result_url <- ""
  }
  kw <- judgment_trim(params$jud_kw)
  title <- judgment_trim(params$jud_title)
  jmain <- judgment_trim(params$jud_jmain)
  has_period <- any(nzchar(judgment_trim(c(
    params$dy1, params$dm1, params$dd1, params$dy2, params$dm2, params$dd2
  ))))
  has_case_no <- any(nzchar(judgment_trim(c(
    params$jud_year, params$jud_case, params$jud_no, params$jud_no_end
  ))))
  if (nzchar(result_url)) {
    max_n <- suppressWarnings(as.integer(params$max_results %||% 30L))
    if (is.na(max_n) || max_n < 1L) max_n <- 30L
    if (max_n > 100L) msgs <- c(msgs, "單次最多抓取 100 筆（司法院單次查詢上限 500 筆）")
    return(list(
      ok = !length(msgs),
      msg = if (length(msgs)) paste(unique(msgs), collapse = "；") else "OK",
      max_results = min(max_n, 100L),
      result_url = result_url
    ))
  }
  if (!nzchar(kw) && !nzchar(title) && !nzchar(jmain) && !has_period && !has_case_no) {
    msgs <- c(msgs, "請至少填寫：全文內容、裁判案由、裁判主文、裁判字號、裁判期間之一；或貼上查詢結果 URL")
  }
  if (grepl("(^[\\+\\-&\\)])|([\\+\\-&\\(]$)", kw, perl = TRUE)) {
    msgs <- c(msgs, "全文檢索字詞首尾不可為 + - & ( )")
  }
  max_n <- suppressWarnings(as.integer(params$max_results %||% 30L))
  if (is.na(max_n) || max_n < 1L) max_n <- 30L
  if (max_n > 100L) msgs <- c(msgs, "單次最多抓取 100 筆（司法院單次查詢上限 500 筆）")
  list(
    ok = !length(msgs),
    msg = if (length(msgs)) paste(unique(msgs), collapse = "；") else "OK",
    max_results = min(max_n, 100L),
    result_url = ""
  )
}

judgment_normalize_result_url <- function(url) {
  url <- judgment_trim(url)
  if (!nzchar(url)) return("")
  if (!grepl("^https?://[^/]*judicial\\.gov\\.tw/.+(qryresultlst|data)\\.aspx", url, ignore.case = TRUE)) {
    stop("查詢結果 URL 須為司法院裁判書系統之 qryresultlst.aspx 或 data.aspx 完整網址")
  }
  url
}

judgment_build_post_body <- function(params, viewstate, viewstate_gen, event_validation) {
  p <- as.list(params)
  vals <- c(
    viewstate, viewstate_gen, event_validation,
    paste(judgment_trim(unlist(p$jud_court %||% "")), collapse = ","),
    judgment_trim(p$jud_year), judgment_trim(p$jud_case),
    judgment_trim(p$jud_no), judgment_trim(p$jud_no_end),
    judgment_trim(p$dy1), judgment_trim(p$dm1), judgment_trim(p$dd1),
    judgment_trim(p$dy2), judgment_trim(p$dm2), judgment_trim(p$dd2),
    judgment_trim(p$jud_title), judgment_trim(p$jud_jmain), judgment_trim(p$jud_kw),
    judgment_trim(p$KbStart), judgment_trim(p$KbEnd),
    "JUDBOOK", "1", "送出查詢"
  )
  names(vals) <- c(
    "__VIEWSTATE", "__VIEWSTATEGENERATOR", "__EVENTVALIDATION",
    "jud_court", "jud_year", "jud_case", "jud_no", "jud_no_end",
    "dy1", "dm1", "dd1", "dy2", "dm2", "dd2",
    "jud_title", "jud_jmain", "jud_kw", "KbStart", "KbEnd",
    "judtype", "whosub", "ctl00$cp_content$btnQry"
  )
  # 案件類別：未勾選＝全選；有勾選則附加 jud_sys
  sys <- p$jud_sys %||% character()
  sys <- unique(judgment_trim(sys))
  sys <- sys[nzchar(sys) & sys %in% unname(JUDGMENT_CASE_TYPE_CHOICES)]
  out <- as.list(vals)
  if (length(sys)) out$jud_sys <- sys
  out
}

judgment_encode_form_body <- function(body) {
  parts <- character()
  for (nm in names(body)) {
    val <- body[[nm]]
    if (identical(nm, "jud_sys") && length(val) > 1L) {
      for (v in val) {
        parts <- c(parts, paste0("jud_sys=", URLencode(as.character(v), reserved = TRUE)))
      }
    } else {
      parts <- c(
        parts,
        paste0(
          URLencode(nm, reserved = TRUE), "=",
          URLencode(as.character(val %||% ""), reserved = TRUE)
        )
      )
    }
  }
  paste(parts, collapse = "&")
}

judgment_perform_get <- function(url, referer = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("需要 httr2 套件以連線司法院裁判書系統")
  }
  req <- httr2::request(url) |>
    httr2::req_headers(
      `User-Agent` = judgment_user_agent(),
      Accept = "text/html,application/xhtml+xml"
    )
  if (nzchar(referer %||% "")) {
    req <- httr2::req_headers(req, Referer = referer)
  }
  resp <- httr2::req_perform(req)
  list(
    status = httr2::resp_status(resp),
    html = httr2::resp_body_string(resp)
  )
}

judgment_perform_post_form <- function(url, body, referer = NULL) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("需要 httr2 套件以連線司法院裁判書系統")
  }
  payload <- judgment_encode_form_body(body)
  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `User-Agent` = judgment_user_agent(),
      `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8",
      `Content-Length` = as.character(nchar(payload, type = "bytes")),
      Referer = referer %||% JUDGMENT_SEARCH_URL,
      Origin = JUDGMENT_SITE_ORIGIN,
      Accept = "text/html,application/xhtml+xml"
    ) |>
    httr2::req_body_raw(payload)
  resp <- httr2::req_perform(req)
  list(
    status = httr2::resp_status(resp),
    html = httr2::resp_body_string(resp)
  )
}

judgment_absolute_url <- function(href) {
  href <- judgment_trim(href)
  if (!nzchar(href)) return("")
  if (grepl("^https?://", href, ignore.case = TRUE)) return(href)
  if (startsWith(href, "/")) return(paste0(JUDGMENT_SITE_ORIGIN, href))
  paste0(JUDGMENT_SITE_ORIGIN, "/FJUD/", sub("^\\./", "", href))
}

judgment_parse_result_links <- function(html) {
  html <- judgment_trim(html)
  if (!nzchar(html)) return(data.frame())
  hrefs <- regmatches(html, gregexpr('href="(data\\.aspx\\?[^"]+|qryresultlst\\.aspx\\?[^"]+)"', html, perl = TRUE))[[1]]
  hrefs <- unique(sub('^href="([^"]+)"$', "\\1", hrefs))
  hrefs <- hrefs[grepl("data\\.aspx", hrefs, ignore.case = TRUE)]
  if (!length(hrefs)) return(data.frame())

  titles <- regmatches(html, gregexpr('>([^<]{8,200})</a>', html, perl = TRUE))[[1]]
  titles <- sub("^>(.*)<$", "\\1", titles)
  titles <- titles[!grepl("^(下一頁|上一頁|回上一頁|列印|友善列印)", titles)]

  rows <- lapply(seq_along(hrefs), function(i) {
    href <- hrefs[[i]]
    title <- if (i <= length(titles)) judgment_trim(titles[[i]]) else ""
    jid <- sub(".*fld=([^&\"']+).*", "\\1", href, perl = TRUE)
    if (identical(jid, href)) jid <- ""
    data.frame(
      裁判字號 = title,
      連結 = judgment_absolute_url(href),
      judgment_id = jid,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$連結), , drop = FALSE]
  rownames(out) <- NULL
  out
}

judgment_extract_section <- function(text, heading) {
  text <- judgment_trim(text)
  if (!nzchar(text)) return("")
  pat <- paste0("(?m)^\\s*", heading, "\\s*\\n([\\s\\S]*?)(?=\\n\\s*(主文|事實|理由|中\\s*華|附表|據上論結)|\\z)")
  m <- regexpr(pat, text, perl = TRUE)
  if (m[1] == -1) return("")
  judgment_trim(sub(paste0("^\\s*", heading), "", regmatches(text, m)[[1]], perl = TRUE))
}

judgment_clean_text <- function(html) {
  html <- judgment_trim(html)
  if (!nzchar(html)) return("")
  txt <- html
  txt <- gsub("(?s)<script[^>]*>.*?</script>", "", txt, perl = TRUE)
  txt <- gsub("(?s)<style[^>]*>.*?</style>", "", txt, perl = TRUE)
  txt <- gsub("<br[^>]*>", "\n", txt, perl = TRUE)
  txt <- gsub("</p>|</div>|</tr>", "\n", txt, perl = TRUE)
  txt <- gsub("<[^>]+>", "", txt, perl = TRUE)
  txt <- gsub("&nbsp;", " ", txt, fixed = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("&lt;", "<", txt, fixed = TRUE)
  txt <- gsub("&gt;", ">", txt, fixed = TRUE)
  txt <- gsub("[ \t]+", " ", txt, perl = TRUE)
  txt <- gsub("\n{3,}", "\n\n", txt, perl = TRUE)
  judgment_trim(txt)
}

judgment_parse_case_type <- function(title) {
  title <- judgment_trim(title)
  if (!nzchar(title)) return("")
  if (grepl("刑事", title)) return("刑事")
  if (grepl("行政", title)) return("行政")
  if (grepl("憲法", title)) return("憲法")
  if (grepl("懲戒", title)) return("懲戒")
  if (grepl("民事", title)) return("民事")
  ""
}

judgment_extract_cause <- function(html, txt, title = "") {
  html <- judgment_trim(html)
  txt <- judgment_trim(txt)
  title <- judgment_trim(title)
  if (nzchar(html)) {
    m <- regexpr("裁判案由[^<]*</[^>]+>\\s*<[^>]+>([^<]+)", html, perl = TRUE, ignore.case = TRUE)
    if (m[1] > 0) {
      cause <- judgment_trim(sub(".*>([^<]+)$", "\\1", regmatches(html, m)[[1]], perl = TRUE))
      if (nzchar(cause)) return(cause)
    }
  }
  if (nzchar(txt)) {
    m <- regexpr("(?m)^裁判案由\\s*[:：]?\\s*(.+)$", txt, perl = TRUE)
    if (m[1] > 0) {
      cause <- judgment_trim(regmatches(txt, m)[[1]])
      cause <- sub("^裁判案由\\s*[:：]?\\s*", "", cause, perl = TRUE)
      if (nzchar(cause)) return(cause)
    }
  }
  if (nzchar(title)) {
    m <- regexpr("號\\s*(?:民事|刑事|行政|憲法|懲戒)?[^\\s]*\\s+(.+)$", title, perl = TRUE)
    if (m[1] > 0) {
      cause <- judgment_trim(sub("^.*號\\s*(?:民事|刑事|行政|憲法|懲戒)?[^\\s]*\\s+", "", title, perl = TRUE))
      if (nzchar(cause) && !grepl("判決|裁定", cause)) return(cause)
    }
  }
  ""
}

judgment_parse_detail <- function(html, url = "") {
  txt <- judgment_clean_text(html)
  main <- judgment_extract_section(txt, "主文")
  if (!nzchar(main)) {
    m <- regexpr("(?s)主文\\s*\\n(.{20,1200})", txt, perl = TRUE)
    if (m[1] > 0) main <- judgment_trim(sub("^主文\\s*", "", regmatches(txt, m)[[1]], perl = TRUE))
  }
  facts <- judgment_extract_section(txt, "事實")
  reasoning <- judgment_extract_section(txt, "理由")
  title <- ""
  m_title <- regexpr("<title>\\s*([^<]+)\\s*</title>", html, perl = TRUE, ignore.case = TRUE)
  if (m_title[1] > 0) title <- judgment_trim(sub(".*<title>\\s*([^<]+)\\s*</title>.*", "\\1", regmatches(html, m_title)[[1]], perl = TRUE))
  cause <- judgment_extract_cause(html, txt, title = title)
  case_type <- judgment_parse_case_type(title)
  list(
    裁判字號 = title,
    裁判主文 = main,
    事實及理由摘要來源 = paste(c(facts, reasoning), collapse = "\n"),
    全文 = txt,
    連結 = url,
    案由 = cause,
    案件類別 = case_type
  )
}

judgment_first_sentences <- function(text, n = 2L, max_chars = 280L) {
  text <- gsub("[ \t\r\n]+", " ", judgment_trim(text), perl = TRUE)
  if (!nzchar(text)) return("")
  parts <- unlist(strsplit(text, "(?<=[。！？])\\s*", perl = TRUE))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(substr(text, 1, max_chars))
  out <- paste(parts[seq_len(min(length(parts), n))], collapse = "")
  if (nchar(out) > max_chars) out <- paste0(substr(out, 1, max_chars), "…")
  out
}

judgment_summarize <- function(detail, target_company = "") {
  main <- judgment_trim(detail$裁判主文 %||% "")
  body <- judgment_trim(detail$事實及理由摘要來源 %||% detail$全文 %||% "")
  title <- judgment_trim(detail$裁判字號 %||% "")
  bits <- character()
  if (nzchar(title)) bits <- c(bits, sprintf("【字號】%s", title))
  if (nzchar(main)) bits <- c(bits, sprintf("【主文】%s", judgment_first_sentences(main, 3L, 360L)))
  if (nzchar(body)) bits <- c(bits, sprintf("【理由摘要】%s", judgment_first_sentences(body, 2L, 360L)))
  company <- judgment_trim(target_company)
  if (nzchar(company) && grepl(company, paste(c(title, main, body), collapse = "\n"), fixed = TRUE)) {
    bits <- c(bits, sprintf("【標的公司】文中提及「%s」", company))
  }
  if (!length(bits)) return("（未能擷取有效摘要；請人工檢視全文）")
  paste(bits, collapse = "\n")
}

empty_judgment_results_frame <- function() {
  data.frame(
    序號 = integer(),
    法院 = character(),
    裁判字號 = character(),
    裁判日期 = character(),
    案由 = character(),
    案件類別 = character(),
    連結 = character(),
    裁判主文 = character(),
    結果分析 = character(),
    全文 = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

judgment_search_submit <- function(params) {
  chk <- judgment_validate_params(params)
  if (!isTRUE(chk$ok)) stop(chk$msg)

  page <- judgment_perform_get(JUDGMENT_SEARCH_URL)
  err <- judgment_response_error_text(page$html)
  if (nzchar(err)) stop(err)

  body <- judgment_build_post_body(
    params,
    judgment_extract_hidden("__VIEWSTATE", page$html),
    judgment_extract_hidden("__VIEWSTATEGENERATOR", page$html),
    judgment_extract_hidden("__EVENTVALIDATION", page$html)
  )
  post <- judgment_perform_post_form(JUDGMENT_SEARCH_URL, body, referer = JUDGMENT_SEARCH_URL)
  err <- judgment_response_error_text(post$html)
  if (nzchar(err)) {
    stop(paste0(
      err,
      "。若持續失敗，請至官網手動查詢後，將左側「查詢結果」之 qryresultlst.aspx 完整網址貼入「查詢結果 URL」欄再執行。"
    ))
  }

  list(html = post$html, max_results = chk$max_results)
}

judgment_fetch_result_list <- function(search_html, max_results = 30L) {
  links <- judgment_parse_result_links(search_html)
  if (!nrow(links)) {
    # 可能包在 iframe：再抓 qryresultlst
    m <- regexpr('id="iframe-data"[^>]*src="([^"]+)"', search_html, perl = TRUE, ignore.case = TRUE)
    if (m[1] > 0) {
      iframe <- sub('.*src="([^"]+)".*', "\\1", regmatches(search_html, m)[[1]], perl = TRUE)
      lst <- judgment_perform_get(judgment_absolute_url(iframe), referer = JUDGMENT_SEARCH_URL)
      links <- judgment_parse_result_links(lst$html)
    }
  }
  if (!nrow(links)) return(links)
  head(links, as.integer(max_results))
}

judgment_fetch_detail <- function(url) {
  page <- judgment_perform_get(url, referer = JUDGMENT_SEARCH_URL)
  judgment_parse_detail(page$html, url = url)
}

judgment_crawl_listing <- function(
    listing,
    progress_cb = NULL,
    data_dir = NULL) {
  if (!nrow(listing)) return(empty_judgment_results_frame())
  step <- function(msg) {
    if (is.function(progress_cb)) progress_cb(msg)
  }
  rows <- list()
  for (i in seq_len(nrow(listing))) {
    step(sprintf("抓取全文 %d / %d…", i, nrow(listing)))
    url <- listing$連結[[i]]
    detail <- tryCatch(
      judgment_fetch_detail(url),
      error = function(e) list(
        裁判字號 = listing$裁判字號[[i]],
        裁判主文 = "",
        事實及理由摘要來源 = "",
        全文 = paste0("（抓取失敗：", conditionMessage(e), "）"),
        連結 = url
      )
    )
    if (!nzchar(detail$裁判字號)) detail$裁判字號 <- listing$裁判字號[[i]]
    analysis <- judgment_analyze_detail(detail, data_dir = data_dir)
    rows[[length(rows) + 1L]] <- data.frame(
      序號 = i,
      法院 = sub("\\s+.*", "", detail$裁判字號 %||% listing$裁判字號[[i]]),
      裁判字號 = detail$裁判字號 %||% listing$裁判字號[[i]],
      裁判日期 = "",
      案由 = analysis$案由 %||% detail$案由 %||% "",
      案件類別 = analysis$案件類別 %||% detail$案件類別 %||% "",
      連結 = url,
      裁判主文 = detail$裁判主文,
      結果分析 = analysis$結果分析,
      全文 = detail$全文,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    Sys.sleep(0.35)
  }
  do.call(rbind, rows)
}

judgment_crawl <- function(
    params,
    progress_cb = NULL,
    data_dir = NULL) {
  chk <- judgment_validate_params(params)
  if (!isTRUE(chk$ok)) stop(chk$msg)

  step <- function(msg) {
    if (is.function(progress_cb)) progress_cb(msg)
  }

  if (nzchar(chk$result_url %||% "")) {
    step("讀取查詢結果 URL…")
    page <- judgment_perform_get(chk$result_url, referer = JUDGMENT_SEARCH_URL)
    err <- judgment_response_error_text(page$html)
    if (nzchar(err)) stop(err)
    step("解析查詢結果…")
    listing <- judgment_fetch_result_list(page$html, chk$max_results)
    return(judgment_crawl_listing(listing, progress_cb, data_dir = data_dir))
  }

  step("連線司法院裁判書查詢…")
  search <- judgment_search_submit(params)
  step("解析查詢結果…")
  listing <- judgment_fetch_result_list(search$html, search$max_results)
  judgment_crawl_listing(listing, progress_cb, data_dir = data_dir)
}

judgment_params_sheet <- function(params) {
  p <- as.list(params)
  labs <- c(
    jud_court = "法院",
    jud_sys = "案件類別",
    jud_year = "裁判字號年度",
    jud_case = "裁判字號字別",
    jud_no = "裁判字號起始號",
    jud_no_end = "裁判字號結束號",
    period_start = "裁判期間起日",
    period_end = "裁判期間迄日",
    jud_title = "裁判案由",
    jud_jmain = "裁判主文",
    jud_kw = "全文內容",
    KbStart = "裁判大小起(K)",
    KbEnd = "裁判大小迄(K)",
    max_results = "抓取筆數上限",
    result_url = "查詢結果 URL"
  )
  vals <- lapply(names(labs), function(nm) {
    if (identical(nm, "period_start")) {
      return(judgment_format_period(p$dy1, p$dm1, p$dd1))
    }
    if (identical(nm, "period_end")) {
      return(judgment_format_period(p$dy2, p$dm2, p$dd2))
    }
    if (identical(nm, "jud_court")) {
      courts <- p$jud_court %||% character()
      if (!length(courts) || !any(nzchar(courts))) return("所有法院")
      return(paste(names(JUDGMENT_COURT_CHOICES)[match(courts, JUDGMENT_COURT_CHOICES)], collapse = "、"))
    }
    if (identical(nm, "jud_sys")) {
      sys <- p$jud_sys %||% character()
      if (!length(sys)) return("（未勾選＝全選）")
      return(paste(names(JUDGMENT_CASE_TYPE_CHOICES)[match(sys, JUDGMENT_CASE_TYPE_CHOICES)], collapse = "、"))
    }
    judgment_trim(p[[nm]])
  })
  data.frame(
    查詢欄位 = unname(labs),
    輸入值 = vapply(vals, function(x) paste(x, collapse = "、"), character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

write_judgment_xlsx <- function(results, params, path) {
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("需要 writexl 套件以匯出 xlsx：install.packages(\"writexl\")")
  }
  if (!is.data.frame(results)) results <- empty_judgment_results_frame()
  export <- results
  if ("全文" %in% names(export)) {
    export$全文 <- vapply(export$全文, function(x) {
      x <- as.character(x %||% "")
      if (nchar(x) > 32000L) paste0(substr(x, 1, 32000L), "…（以下省略）") else x
    }, character(1))
  }
  writexl::write_xlsx(
    list(
      查詢條件 = judgment_params_sheet(params),
      判決分析 = export
    ),
    path
  )
}
