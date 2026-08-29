# 司法院裁判書查詢（Default_AD.aspx）爬蟲 + 摘要 + 財務營運影響評估
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

JUDGMENT_IMPACT_LEVELS <- c("高", "中", "低", "無明顯影響")

judgment_user_agent <- function() {
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
}

judgment_trim <- function(x) {
  trimws(as.character(x %||% ""))
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
    max_n <- suppressWarnings(as.integer(params$max_results %||% 20L))
    if (is.na(max_n) || max_n < 1L) max_n <- 20L
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
  max_n <- suppressWarnings(as.integer(params$max_results %||% 20L))
  if (is.na(max_n) || max_n < 1L) max_n <- 20L
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
  list(
    裁判字號 = title,
    裁判主文 = main,
    事實及理由摘要來源 = paste(c(facts, reasoning), collapse = "\n"),
    全文 = txt,
    連結 = url
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

judgment_impact_keywords <- function() {
  list(
    high = c(
      "破產", "重整", "清算", "停止上市", "下市", "摘牌", "退市",
      "財報不实", "財報不實", "虛增", "掏空", "背信", "侵占", "詐欺",
      "內線交易", "無法表示意見", "否定意見", "持續經營重大疑慮",
      "重大影響", "鉅額賠償", "有期徒刑", "假扣押", "假處分", "解聘董事",
      "財務報表", "內部控制重大缺失", "證券交易法", "公開發行"
    ),
    medium = c(
      "損害賠償", "違約金", "罰鍰", "罰款", "行政裁罰", "解僱", "勞資爭議",
      "契約解除", "股東", "董事", "負責人", "連帶責任", "支付命令",
      "定讞", "詐術", "背信罪", "侵占罪", "營業秘密", "競業禁止"
    ),
    low = c(
      "駁回", "不受理", "撤回", "和解成立", "調解成立", "部分勝訴", "部分敗訴"
    )
  )
}

judgment_score_keyword_hits <- function(text, keywords) {
  text <- judgment_trim(text)
  if (!nzchar(text)) return(character())
  keywords[vapply(keywords, function(k) grepl(k, text, fixed = TRUE), logical(1))]
}

judgment_assess_financial_impact <- function(summary, full_text, target_company = "") {
  text <- paste(summary, full_text, sep = "\n")
  kw <- judgment_impact_keywords()
  high <- judgment_score_keyword_hits(text, kw$high)
  medium <- judgment_score_keyword_hits(text, kw$medium)
  low <- judgment_score_keyword_hits(text, kw$low)
  score <- length(high) * 18L + length(medium) * 8L + length(low) * 2L
  company <- judgment_trim(target_company)
  mentions_company <- nzchar(company) && grepl(company, text, fixed = TRUE)
  if (mentions_company) score <- score + 25L
  if (grepl("億|千萬|百萬", text)) score <- score + 10L
  if (grepl("刑事|詐欺|背信|侵占|洗錢", text)) score <- score + 12L

  level <- if (score >= 70L) {
    "高"
  } else if (score >= 40L) {
    "中"
  } else if (score >= 15L) {
    "低"
  } else {
    "無明顯影響"
  }

  rationale <- character()
  if (mentions_company) {
    rationale <- c(rationale, sprintf("判決內容提及標的公司「%s」", company))
  } else if (nzchar(company)) {
    rationale <- c(rationale, sprintf("未直接提及標的公司「%s」；影響評估僅供參考", company))
  }
  if (length(high)) rationale <- c(rationale, paste0("高度相關關鍵字：", paste(high, collapse = "、")))
  if (length(medium)) rationale <- c(rationale, paste0("中度相關關鍵字：", paste(medium, collapse = "、")))
  if (length(low)) rationale <- c(rationale, paste0("低度／程序性關鍵字：", paste(low, collapse = "、")))
  if (!length(rationale)) rationale <- "未命中明顯財務或營運衝擊關鍵字"

  list(
    財務營運影響等級 = level,
    影響分數 = as.integer(score),
    影響分析說明 = paste(rationale, collapse = "；"),
    命中關鍵字 = paste(c(high, medium, low), collapse = "、")
  )
}

# --- LLM 摘要與影響分析（OpenAI Chat Completions；無 API key 時自動退回關鍵字規則）---

judgment_llm_default_model <- function() {
  m <- judgment_trim(Sys.getenv("OPENAI_MODEL", unset = "gpt-4o-mini"))
  if (!nzchar(m)) "gpt-4o-mini" else m
}

judgment_llm_api_key <- function(api_key = NULL) {
  key <- judgment_trim(api_key %||% Sys.getenv("OPENAI_API_KEY", unset = ""))
  if (!nzchar(key)) "" else key
}

judgment_llm_available <- function(api_key = NULL) {
  nzchar(judgment_llm_api_key(api_key))
}

judgment_llm_truncate <- function(text, max_chars = 12000L) {
  text <- judgment_trim(text)
  if (!nzchar(text)) return("")
  if (nchar(text) <= max_chars) return(text)
  paste0(substr(text, 1, max_chars), "\n…（以下省略）")
}

judgment_llm_build_prompt <- function(detail, target_company = "") {
  company <- judgment_trim(target_company)
  company_line <- if (nzchar(company)) {
    sprintf("查詢標的公司：%s（請特別評估對該公司整體財務與營運之可能衝擊）", company)
  } else {
    "查詢標的公司：（未指定）"
  }
  body <- judgment_llm_truncate(paste(
    c(
      sprintf("裁判字號：%s", judgment_trim(detail$裁判字號 %||% "")),
      sprintf("主文：%s", judgment_trim(detail$裁判主文 %||% "")),
      sprintf("全文：%s", judgment_llm_truncate(detail$全文 %||% "", 10000L))
    ),
    collapse = "\n\n"
  ))
  list(
    system = paste(
      "你是台灣司法判決書分析助理，協助內部審計與內控人員快速理解判決重點。",
      "請以繁體中文回答，且僅輸出單一 JSON 物件（勿 markdown），欄位：",
      "summary（200字內判決摘要）、impact_level（高/中/低/無明顯影響）、",
      "impact_score（0-100整數）、impact_rationale（影響說明）、keywords（逗號分隔關鍵字）。",
      "impact_level 須為：高、中、低、無明顯影響 其中之一。",
      sep = ""
    ),
    user = paste(company_line, body, sep = "\n\n")
  )
}

judgment_llm_parse_response <- function(raw) {
  raw <- judgment_trim(raw)
  if (!nzchar(raw)) stop("LLM 回應為空")
  json_txt <- raw
  if (grepl("```", raw, fixed = TRUE)) {
    m <- regexpr("```(?:json)?\\s*([\\s\\S]*?)\\s*```", raw, perl = TRUE, ignore.case = TRUE)
    if (m[1] > 0) json_txt <- regmatches(raw, m)[[1]]
    json_txt <- sub("^```(?:json)?\\s*", "", json_txt, perl = TRUE, ignore.case = TRUE)
    json_txt <- sub("\\s*```$", "", json_txt, perl = TRUE)
  }
  m_obj <- regexpr("\\{[\\s\\S]*\\}", json_txt, perl = TRUE)
  if (m_obj[1] > 0) json_txt <- regmatches(json_txt, m_obj)[[1]]
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("需要 jsonlite 以解析 LLM 回應")
  obj <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE)
  level <- judgment_trim(obj$impact_level %||% obj$財務營運影響等級 %||% "")
  if (!level %in% JUDGMENT_IMPACT_LEVELS) {
    level <- if (grepl("^高", level)) "高" else if (grepl("^中", level)) "中" else if (grepl("^低", level)) "低" else "無明顯影響"
  }
  score <- suppressWarnings(as.integer(obj$impact_score %||% obj$影響分數 %||% 0L))
  if (is.na(score)) score <- 0L
  score <- max(0L, min(100L, score))
  summary <- judgment_trim(obj$summary %||% obj$內容摘要 %||% "")
  rationale <- judgment_trim(obj$impact_rationale %||% obj$影響分析說明 %||% "")
  keywords <- judgment_trim(obj$keywords %||% obj$命中關鍵字 %||% "")
  if (!nzchar(summary)) stop("LLM 回應缺少 summary")
  list(
    內容摘要 = summary,
    財務營運影響等級 = level,
    影響分數 = score,
    影響分析說明 = if (nzchar(rationale)) paste0("【LLM】", rationale) else "【LLM】已完成影響評估",
    命中關鍵字 = keywords
  )
}

judgment_llm_chat <- function(messages, api_key = NULL, model = NULL, timeout_secs = 90L) {
  key <- judgment_llm_api_key(api_key)
  if (!nzchar(key)) stop("未設定 OPENAI_API_KEY，無法使用 LLM 分析")
  if (!requireNamespace("httr2", quietly = TRUE)) stop("需要 httr2 套件以呼叫 LLM API")
  model <- judgment_trim(model %||% judgment_llm_default_model())
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0.2,
      response_format = list(type = "json_object"),
      messages = messages
    ), auto_unbox = TRUE) |>
    httr2::req_timeout(as.numeric(timeout_secs))
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  body <- httr2::resp_body_string(resp)
  if (status >= 400L) {
    err <- tryCatch(jsonlite::fromJSON(body, simplifyVector = TRUE)$error$message, error = function(e) body)
    stop(sprintf("LLM API 錯誤 (%s): %s", status, judgment_trim(as.character(err))))
  }
  parsed <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  content <- parsed$choices[[1]]$message$content %||% ""
  judgment_llm_parse_response(as.character(content))
}

judgment_llm_analyze <- function(detail, target_company = "", api_key = NULL, model = NULL) {
  prompt <- judgment_llm_build_prompt(detail, target_company = target_company)
  judgment_llm_chat(
    list(
      list(role = "system", content = prompt$system),
      list(role = "user", content = prompt$user)
    ),
    api_key = api_key,
    model = model
  )
}

judgment_analyze_detail <- function(
    detail,
    target_company = "",
    use_llm = FALSE,
    api_key = NULL,
    model = NULL,
    progress_cb = NULL) {
  if (isTRUE(use_llm) && judgment_llm_available(api_key)) {
    step <- function(msg) {
      if (is.function(progress_cb)) progress_cb(msg)
    }
    step("LLM 摘要與影響分析中…")
    llm <- tryCatch(
      judgment_llm_analyze(detail, target_company = target_company, api_key = api_key, model = model),
      error = function(e) {
        step(sprintf("LLM 失敗，改用關鍵字規則：%s", conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(llm)) return(llm)
  }
  summary <- judgment_summarize(detail, target_company = target_company)
  impact <- judgment_assess_financial_impact(summary, detail$全文, target_company = target_company)
  list(
    內容摘要 = summary,
    財務營運影響等級 = impact$財務營運影響等級,
    影響分數 = impact$影響分數,
    影響分析說明 = impact$影響分析說明,
    命中關鍵字 = impact$命中關鍵字
  )
}

empty_judgment_results_frame <- function() {
  data.frame(
    序號 = integer(),
    查詢標的公司 = character(),
    法院 = character(),
    裁判字號 = character(),
    裁判日期 = character(),
    案由 = character(),
    案件類別 = character(),
    連結 = character(),
    裁判主文 = character(),
    內容摘要 = character(),
    財務營運影響等級 = character(),
    影響分數 = integer(),
    影響分析說明 = character(),
    命中關鍵字 = character(),
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

judgment_fetch_result_list <- function(search_html, max_results = 20L) {
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
    target_company = "",
    progress_cb = NULL,
    use_llm = FALSE,
    api_key = NULL,
    model = NULL) {
  target_company <- judgment_trim(target_company)
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
    analysis <- judgment_analyze_detail(
      detail,
      target_company = target_company,
      use_llm = use_llm,
      api_key = api_key,
      model = model,
      progress_cb = progress_cb
    )
    rows[[length(rows) + 1L]] <- data.frame(
      序號 = i,
      查詢標的公司 = target_company,
      法院 = sub("\\s+.*", "", detail$裁判字號 %||% listing$裁判字號[[i]]),
      裁判字號 = detail$裁判字號 %||% listing$裁判字號[[i]],
      裁判日期 = "",
      案由 = "",
      案件類別 = "",
      連結 = url,
      裁判主文 = detail$裁判主文,
      內容摘要 = analysis$內容摘要,
      財務營運影響等級 = analysis$財務營運影響等級,
      影響分數 = analysis$影響分數,
      影響分析說明 = analysis$影響分析說明,
      命中關鍵字 = analysis$命中關鍵字,
      全文 = detail$全文,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    Sys.sleep(if (isTRUE(use_llm)) 0.6 else 0.35)
  }
  do.call(rbind, rows)
}

judgment_crawl <- function(
    params,
    target_company = "",
    progress_cb = NULL,
    use_llm = FALSE,
    api_key = NULL,
    model = NULL) {
  chk <- judgment_validate_params(params)
  if (!isTRUE(chk$ok)) stop(chk$msg)
  target_company <- judgment_trim(target_company)

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
    return(judgment_crawl_listing(
      listing, target_company, progress_cb,
      use_llm = use_llm, api_key = api_key, model = model
    ))
  }

  step("連線司法院裁判書查詢…")
  search <- judgment_search_submit(params)
  step("解析查詢結果…")
  listing <- judgment_fetch_result_list(search$html, search$max_results)
  judgment_crawl_listing(
    listing, target_company, progress_cb,
    use_llm = use_llm, api_key = api_key, model = model
  )
}

judgment_params_sheet <- function(params, target_company = "") {
  p <- as.list(params)
  labs <- c(
    jud_court = "法院",
    jud_sys = "案件類別",
    jud_year = "裁判字號年度",
    jud_case = "裁判字號字別",
    jud_no = "裁判字號起始號",
    jud_no_end = "裁判字號結束號",
    dy1 = "裁判期間起年", dm1 = "裁判期間起月", dd1 = "裁判期間起日",
    dy2 = "裁判期間迄年", dm2 = "裁判期間迄月", dd2 = "裁判期間迄日",
    jud_title = "裁判案由",
    jud_jmain = "裁判主文",
    jud_kw = "全文內容",
    KbStart = "裁判大小起(K)",
    KbEnd = "裁判大小迄(K)",
    max_results = "抓取筆數上限",
    result_url = "查詢結果 URL",
    use_llm = "使用 LLM 分析",
    target_company = "查詢標的公司"
  )
  vals <- lapply(names(labs), function(nm) {
    if (identical(nm, "target_company")) return(judgment_trim(target_company))
    if (identical(nm, "use_llm")) {
      v <- p$use_llm %||% FALSE
      return(if (isTRUE(v)) "是" else "否")
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

write_judgment_xlsx <- function(results, params, target_company, path) {
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
      查詢條件 = judgment_params_sheet(params, target_company = target_company),
      判決分析 = export
    ),
    path
  )
}
