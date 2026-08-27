# Guided cascade: cycle → 子作業 → 風險 → 目標 → 活動(單一預防/偵測) → IUC
# Company status unlocks only after cascade is complete.
# 「六大控制項目」規則：組裝現況描述前必須就緒的設計要素。

# 六大控制項目（現況書寫前置規則）
SIX_CONTROL_STATUS_RULES <- c(
  control_objective = "控制目標",
  control_activity = "控制活動",
  nature = "控制性質（人工／自動）",
  approach = "控制方式（預防／偵測／矯正）",
  frequency = "控制頻率",
  responsible_unit = "控制點負責單位",
  iuc_or_system = if (exists("CONTROL_IUC_DOCUMENT_LABEL", inherits = TRUE)) {
    CONTROL_IUC_DOCUMENT_LABEL
  } else {
    "相關文件-控制用文件"
  }
)
# Note: objective+activity are cascade-selected; the six operational items for
# status scaffolding are nature/approach/frequency/owner/IUC + steps derived from activity.
# User-facing「六大」bundle used for status template gates:
SIX_STATUS_GATE_FIELDS <- c(
  "nature", "approach", "frequency", "responsible_unit", "iuc_or_system", "control_activity"
)

nzchar_trim <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!length(x) || is.na(x[[1]])) return("")
  x[[1]]
}

# Enforce: one activity ↔ exactly one 預防/偵測/矯正 attribute
normalize_single_activity_type <- function(approach) {
  raw <- nzchar_trim(approach)
  if (!nzchar(raw)) return("")
  # Reject mixed before normalizing
  n_hit <- sum(c(
    grepl("預防|Preventive", raw, ignore.case = TRUE),
    grepl("偵測|Detective", raw, ignore.case = TRUE),
    grepl("矯正|Corrective", raw, ignore.case = TRUE)
  ))
  if (n_hit > 1L) return("")
  if (grepl("預防\\+|預防＋|Preventive\\s*\\+", raw, ignore.case = TRUE)) return("")
  at <- if (exists("normalize_control_activity_type_pd", mode = "function")) {
    normalize_control_activity_type_pd(raw)
  } else raw
  if (at %in% c("預防性", "偵測性", "矯正性", "預防性控制", "偵測性控制")) {
    return(normalize_control_activity_type_pd(at))
  }
  if (grepl("預防", at)) return("預防性")
  if (grepl("偵測", at)) return("偵測性")
  if (grepl("矯正", at)) return("矯正性")
  ""
}

activity_type_ok <- function(approach) {
  nzchar(normalize_single_activity_type(approach))
}

# Flat rows from library for cascade indexing
library_controls_flat <- function(library, cycle = NULL) {
  items <- library %||% list()
  if (!is.null(cycle) && nzchar(cycle)) {
    items <- Filter(function(x) {
      cy <- x$cycle %||% x$control$cycle %||% ""
      identical(cy, cycle) ||
        (grepl("資訊|電腦", cycle) && grepl("資訊|電腦", cy))
    }, items)
  }
  lapply(items, function(it) {
    c <- it$control %||% it
    list(
      library_id = it$library_id %||% c$library_id %||% "",
      cycle = c$cycle %||% it$cycle %||% "",
      sub_process_id = nzchar_trim(c$sub_process_id),
      sub_process = nzchar_trim(c$sub_process),
      risk_factor = nzchar_trim(c$risk_factor %||% c$risk_name),
      risk_name = nzchar_trim(c$risk_name %||% c$risk_factor),
      risk_principle = nzchar_trim(c$risk_principle),
      risk_area = nzchar_trim(c$risk_area),
      risk_description = nzchar_trim(c$risk_description),
      risk_category = {
        if (exists("normalize_risk_category", mode = "function")) {
          normalize_risk_category(c)
        } else nzchar_trim(c$risk_category)
      },
      risk_attr_financial = nzchar_trim(c$risk_attr_financial),
      risk_attr_operations = nzchar_trim(c$risk_attr_operations),
      risk_attr_compliance = nzchar_trim(c$risk_attr_compliance),
      control_objective = nzchar_trim(c$control_objective),
      control_activity = nzchar_trim(c$control_activity),
      approach = normalize_single_activity_type(c$approach %||% c$control_activity_type),
      nature = {
        if (exists("normalize_control_type_manual_auto", mode = "function")) {
          normalize_control_type_manual_auto(c$nature %||% c$control_type)
        } else nzchar_trim(c$nature)
      },
      frequency = nzchar_trim(c$frequency),
      responsible_unit = nzchar_trim(c$responsible_unit),
      iuc_or_system = nzchar_trim(c$iuc %||% c$iuc_or_system),
      related_system = nzchar_trim(c$related_system),
      romm_classification = nzchar_trim(c$romm_classification),
      significant_account = nzchar_trim(c$significant_account),
      assertions = nzchar_trim(c$assertions),
      related_policy = nzchar_trim(c$related_policy),
      related_law = nzchar_trim(c$related_law),
      related_document = nzchar(nzchar_trim(c$related_document %||% c$outputs)) ||
        pbc_ids_are_filled(c$related_document_pbc_ids),
      type = nzchar_trim(c$type),
      inputs = nzchar_trim(c$inputs),
      review_steps = nzchar_trim(c$review_steps),
      outputs = nzchar_trim(c$outputs),
      investigation_threshold = nzchar_trim(c$investigation_threshold),
      company_status = nzchar_trim(c$company_status %||% c$detailed_description),
      design_gap_note = nzchar_trim(c$design_gap_note),
      control_id = nzchar_trim(c$control_id),
      raw = c
    )
  })
}

# 引導候選來源：永遠合併內建種子（九大循環可直接選），毋須先匯入底稿
# 快取種子，避免每次刷新選單都重跑 seed（約 1–2 秒）導致 UI 卡住／訊息錯序
.cascade_builtin_library_cache <- new.env(parent = emptyenv())

cascade_builtin_library <- function(force = FALSE) {
  if (!isTRUE(force) && !is.null(.cascade_builtin_library_cache$items)) {
    return(.cascade_builtin_library_cache$items)
  }
  items <- tryCatch(seed_control_library(TRUE), error = function(e) list())
  .cascade_builtin_library_cache$items <- items
  items
}

cascade_source_library <- function(user_library = list()) {
  # 磁碟／session 庫已夠大時直接用，避免每次選單刷新都 merge 種子
  if (length(user_library) >= 100L) return(user_library)
  builtin <- cascade_builtin_library()
  if (!length(user_library)) return(builtin)
  if (!exists("merge_libraries", mode = "function")) {
    return(c(builtin, user_library))
  }
  merge_libraries(builtin, user_library, overwrite = FALSE)
}

sub_process_key <- function(spid, spn) {
  sprintf("%s||%s", nzchar_trim(spid), nzchar_trim(spn))
}

parse_sub_process_key <- function(key) {
  key <- nzchar_trim(key)
  if (!nzchar(key)) return(list(id = "", name = ""))
  if (!grepl("\\|\\|", key, fixed = FALSE)) {
    # 純名稱（APP 顯示用）或純編號
    if (grepl("^[A-Z]{2}-\\d+", key)) {
      return(list(id = key, name = ""))
    }
    return(list(id = "", name = key))
  }
  parts <- strsplit(key, "\\|\\|", perl = TRUE)[[1]]
  list(id = parts[[1]] %||% "", name = if (length(parts) > 1) parts[[2]] else "")
}

# 子作業名稱 selectize：畫面永遠只顯示／選取純名稱；編號由關聯查表帶入
sub_process_filter_key <- function(spid, spn) {
  spn <- trimws(as.character(spn %||% ""))
  spid <- trimws(as.character(spid %||% ""))
  if (grepl("\\|\\|", spn, fixed = FALSE)) return(spn)
  nm <- sub_process_name_from_value(spn)
  if (nzchar(nm) && nzchar(spid)) return(sub_process_key(spid, nm))
  if (nzchar(nm)) return(nm)
  if (nzchar(spid)) return(spid)
  ""
}

sub_process_name_from_value <- function(val) {
  val <- trimws(as.character(val %||% ""))
  if (!nzchar(val)) return("")
  if (grepl("\\|\\|", val, fixed = FALSE)) {
    return(parse_sub_process_key(val)$name)
  }
  # 純編號不當成名稱
  if (grepl("^[A-Z]{2}-\\d+(-\\d+)?$", val)) return("")
  val
}

sub_process_id_from_value <- function(val, fallback_id = "") {
  val <- trimws(as.character(val %||% ""))
  fb <- trimws(as.character(fallback_id %||% ""))
  if (grepl("\\|\\|", val, fixed = FALSE)) {
    sp <- parse_sub_process_key(val)
    if (nzchar(sp$id)) sp$id else fb
  } else if (grepl("^[A-Z]{2}-\\d+", val)) {
    val
  } else {
    fb
  }
}

# 依名稱從範本列查出關聯子作業編號（同名取第一筆；可指定偏好編號）
lookup_sub_process_id_for_name <- function(rows, name, preferred_id = "") {
  nm <- nzchar_trim(name)
  if (!nzchar(nm) || !length(rows)) return("")
  pref <- nzchar_trim(preferred_id)
  if (nzchar(pref)) {
    hit <- Filter(function(r) {
      identical(nzchar_trim(r$sub_process), nm) &&
        identical(nzchar_trim(r$sub_process_id), pref)
    }, rows)
    if (length(hit)) return(pref)
  }
  for (r in rows) {
    if (identical(nzchar_trim(r$sub_process), nm)) {
      id <- nzchar_trim(r$sub_process_id)
      if (nzchar(id)) return(id)
    }
  }
  ""
}

# 子作業／控制編號前綴是否與目前循環編號一致（不一致則應清空／重選）
id_matches_cycle_code <- function(id, cycle_code) {
  id <- trimws(as.character(id %||% ""))
  cycle_code <- trimws(as.character(cycle_code %||% ""))
  if (!nzchar(id) || !nzchar(cycle_code)) return(TRUE)
  if (!grepl("^[A-Z]{2}-", id)) return(TRUE)
  id_cc <- sub("^([A-Z]{2})-.*$", "\\1", id)
  known <- unique(unname(CYCLE_CODE_MAP))
  if (!(id_cc %in% known)) return(TRUE)
  identical(id_cc, cycle_code)
}

# 編號組成規則：
#   子作業編號 = [循環編號]-[子作業序號]          例：EC-101
#   控制編號   = [循環編號]-[子作業序號]-[控制序號] 例：EC-101-01
parse_rcm_id_parts <- function(id) {
  id <- trimws(as.character(id %||% ""))
  if (!nzchar(id)) {
    return(list(cycle = "", sub = "", ctrl = "", ok = FALSE))
  }
  # EC-101-01
  m3 <- regmatches(id, regexec("^([A-Za-z0-9]+)-([A-Za-z0-9]+)-([0-9]+)$", id, perl = TRUE))[[1]]
  if (length(m3) >= 4L) {
    return(list(cycle = m3[[2]], sub = m3[[3]], ctrl = m3[[4]], ok = TRUE))
  }
  # EC-101
  m2 <- regmatches(id, regexec("^([A-Za-z0-9]+)-([A-Za-z0-9]+)$", id, perl = TRUE))[[1]]
  if (length(m2) >= 3L) {
    return(list(cycle = m2[[2]], sub = m2[[3]], ctrl = "", ok = TRUE))
  }
  list(cycle = "", sub = id, ctrl = "", ok = FALSE)
}

compose_sub_process_id <- function(cycle_code, sub_no) {
  cc <- trimws(as.character(cycle_code %||% ""))
  sn <- trimws(as.character(sub_no %||% ""))
  if (!nzchar(cc) || !nzchar(sn)) return("")
  sprintf("%s-%s", cc, sn)
}

compose_control_id <- function(cycle_code, sub_no, ctrl_no) {
  cc <- trimws(as.character(cycle_code %||% ""))
  sn <- trimws(as.character(sub_no %||% ""))
  if (!nzchar(cc) || !nzchar(sn)) return("")
  n <- suppressWarnings(as.integer(ctrl_no))
  if (is.na(n) || n < 1L) n <- 1L
  sprintf("%s-%s-%02d", cc, sn, n)
}

# 自子作業編號取出序號（EC-101 → 101；已是純序號則原樣）
sub_process_seq_from_id <- function(sub_process_id, cycle_code = "") {
  sp <- trimws(as.character(sub_process_id %||% ""))
  if (!nzchar(sp)) return("")
  parts <- parse_rcm_id_parts(sp)
  if (isTRUE(parts$ok) && nzchar(parts$sub)) return(parts$sub)
  cc <- trimws(as.character(cycle_code %||% ""))
  if (nzchar(cc) && startsWith(sp, paste0(cc, "-"))) {
    return(sub(paste0("^", cc, "-"), "", sp))
  }
  sp
}

# 依循環編號重寫既有 ID 前綴（保留子作業序號／控制序號）
recode_id_cycle_prefix <- function(id, new_cycle_code) {
  id <- trimws(as.character(id %||% ""))
  new_cycle_code <- trimws(as.character(new_cycle_code %||% ""))
  if (!nzchar(id) || !nzchar(new_cycle_code)) return(id)
  parts <- parse_rcm_id_parts(id)
  if (!isTRUE(parts$ok) || !nzchar(parts$sub)) return(id)
  known <- unique(unname(CYCLE_CODE_MAP))
  if (!(parts$cycle %in% known)) return(id)
  if (nzchar(parts$ctrl)) {
    compose_control_id(new_cycle_code, parts$sub, parts$ctrl)
  } else {
    compose_sub_process_id(new_cycle_code, parts$sub)
  }
}

sub_process_choice_label <- function(key) {
  key <- trimws(as.character(key %||% ""))
  if (!nzchar(key)) return("")
  if (grepl("\\|\\|", key, fixed = FALSE)) {
    sp <- parse_sub_process_key(key)
    if (nzchar(sp$name)) return(sp$name)
    return(sp$id)
  }
  key
}

activity_key <- function(activity, approach) {
  sprintf("%s||%s", nzchar_trim(activity), normalize_single_activity_type(approach))
}

parse_activity_key <- function(key) {
  parts <- strsplit(nzchar_trim(key), "\\|\\|", perl = TRUE)[[1]]
  list(activity = parts[[1]] %||% "", approach = if (length(parts) > 1) parts[[2]] else "")
}

filter_cascade_rows <- function(rows,
                                sub_key = NULL,
                                risk_factor = NULL,
                                objective = NULL,
                                activity_key_sel = NULL) {
  out <- rows
  if (!is.null(sub_key) && nzchar(sub_key) && !identical(sub_key, "__custom__")) {
    sp <- parse_sub_process_key(sub_key)
    out <- Filter(function(r) {
      (nzchar(sp$id) && identical(r$sub_process_id, sp$id)) ||
        (nzchar(sp$name) && identical(r$sub_process, sp$name))
    }, out)
  }
  if (!is.null(risk_factor) && !identical(risk_factor, "__custom__") &&
      length(parse_risk_factor_values(risk_factor))) {
    out <- Filter(function(r) row_matches_risk_factor(r, risk_factor), out)
  }
  if (!is.null(objective) && nzchar(objective) && !identical(objective, "__custom__")) {
    out <- Filter(function(r) identical(r$control_objective, objective), out)
  }
  if (!is.null(activity_key_sel) && nzchar(activity_key_sel) &&
      !identical(activity_key_sel, "__custom__")) {
    ak <- parse_activity_key(activity_key_sel)
    out <- Filter(function(r) {
      identical(r$control_activity, ak$activity) &&
        (identical(r$approach, ak$approach) || !nzchar(ak$approach))
    }, out)
  }
  out
}

cascade_sub_process_choices <- function(rows) {
  # 選單 value／label 皆為純名稱（永不夾帶編號）；同名保留第一筆
  labels <- unique(vapply(rows, function(r) nzchar_trim(r$sub_process), character(1)))
  labels <- labels[nzchar(labels)]
  stats::setNames(labels, labels)
}

# Short tag label for 風險因素（風險描述之 tag；不含 []）
risk_factor_tag <- function(x) {
  x <- trimws(as.character(x %||% ""))
  if (!nzchar(x)) return("")
  x <- gsub("\\[|\\]", "", x)
  parts <- trimws(strsplit(x, "\\s*/\\s*")[[1]])
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return("")
  tag <- parts[[1]]
  if (nchar(tag) > 20) paste0(substr(tag, 1, 19), "…") else tag
}

row_risk_factor_tags <- function(r) {
  vals <- parse_risk_factor_values(r$risk_factor %||% r$risk_name %||% "")
  tags <- unique(c(vals, vapply(vals, risk_factor_tag, character(1))))
  tags[nzchar(tags)]
}

row_matches_risk_factor <- function(r, risk_factors) {
  want <- unique(c(
    parse_risk_factor_values(risk_factors),
    vapply(parse_risk_factor_values(risk_factors), risk_factor_tag, character(1))
  ))
  want <- want[nzchar(want) & want != "__custom__"]
  if (!length(want)) return(FALSE)
  have <- row_risk_factor_tags(r)
  any(want %in% have)
}

# 風險因素複選：僅以；分隔（保留名稱中的 /）
parse_risk_factor_values <- function(x) {
  if (is.character(x) && length(x) > 1L) {
    vals <- trimws(x)
    return(unique(vals[nzchar(vals)]))
  }
  raw <- trimws(as.character(x %||% ""))
  if (!nzchar(raw)) return(character())
  vals <- trimws(unlist(strsplit(raw, "[;；]+")))
  unique(vals[nzchar(vals)])
}

join_risk_factor_values <- function(x) {
  vals <- parse_risk_factor_values(x)
  if (!length(vals)) return("")
  paste(vals, collapse = "；")
}

# 風險因素複選：解析、正規化 tag、以；接合（RCM／必填檢核）
format_risk_factor_text <- function(x) {
  vals <- parse_risk_factor_values(x)
  if (!length(vals)) return("")
  tags <- unique(vapply(vals, risk_factor_tag, character(1)))
  tags <- tags[nzchar(tags)]
  join_risk_factor_values(tags)
}

risk_factors_are_filled <- function(x) {
  length(parse_risk_factor_values(x)) > 0L
}

risk_factor_selection_from_ctrl <- function(ctrl) {
  parse_risk_factor_values(ctrl$risk_factor %||% ctrl$risk_name %||% "")
}

# Apply library / RCM control cycle (form fields filled by apply_supplement_from_ctrl)
apply_ctrl_to_cascade <- function(session, ctrl, current_cycle = NULL) {
  ctrl <- as.list(ctrl)
  if (nzchar(ctrl$cycle %||% "")) {
    cur_cy <- trimws(as.character(current_cycle %||% ""))
    if (!identical(cur_cy, ctrl$cycle)) {
      updateSelectInput(session, "cycle", selected = ctrl$cycle)
    }
    cc <- trimws(ctrl$cycle_code %||% "")
    if (!nzchar(cc)) cc <- cycle_code_for(ctrl$cycle)
    updateTextInput(session, "cycle_code", value = cc)
  }
  invisible(ctrl)
}

apply_supplement_from_ctrl <- function(session, ctrl, pbc_registry = NULL) {
  ctrl <- as.list(ctrl)
  # 範本庫不回填子作業／控制編號；僅當來源本身已有執行階段編號時才帶入
  cid <- trimws(as.character(ctrl$control_id %||% ""))
  if (nzchar(cid) && !grepl("^(LIB|JL|PL)-", cid)) {
    updateTextInput(session, "control_id", value = cid)
  }
  updateTextInput(session, "cycle_code", value = {
    cc <- trimws(ctrl$cycle_code %||% "")
    if (nzchar(cc)) cc else cycle_code_for(ctrl$cycle %||% "")
  })
  spid <- trimws(as.character(ctrl$sub_process_id %||% ""))
  if (nzchar(spid)) {
    updateTextInput(session, "sub_process_id", value = spid)
  }
  # 選單永遠只選純名稱（編號另欄維護）
  sp_sel <- sub_process_name_from_value(ctrl$sub_process %||% "")
  updateSelectizeInput(session, "sub_process", selected = sp_sel)
  rf_sel <- risk_factor_selection_from_ctrl(ctrl)
  updateSelectizeInput(session, "risk_factor", selected = rf_sel)
  if (nzchar(trimws(ctrl$risk_principle %||% ""))) {
    rp <- trimws(ctrl$risk_principle)
    updateSelectizeInput(session, "risk_principle",
                         choices = stats::setNames(rp, rp), selected = rp, server = FALSE)
  }
  if (nzchar(trimws(ctrl$risk_area %||% ""))) {
    ra <- trimws(ctrl$risk_area)
    updateSelectizeInput(session, "risk_area",
                         choices = stats::setNames(ra, ra), selected = ra, server = FALSE)
  }
  rd <- trimws(as.character(ctrl$risk_description %||% ""))
  if (nzchar(rd)) {
    updateTextAreaInput(session, "risk_description", value = rd)
  }
  if (nzchar(trimws(ctrl$risk_category %||% ""))) {
    updateSelectInput(session, "risk_category", selected = ctrl$risk_category)
  }
  if (nzchar(trimws(ctrl$romm_classification %||% ""))) {
    updateSelectInput(session, "romm_classification", selected = ctrl$romm_classification)
  }
  updateSelectizeInput(
    session, "significant_account",
    choices = account_select_choices(),
    selected = {
      ac <- trimws(as.character(ctrl$significant_account %||% ""))
      if (is_reporting_risk_category(ctrl$risk_category %||% "")) {
        expand_account_selection(ac)
      } else {
        character(0)
      }
    }
  )
  updateSelectizeInput(
    session, "related_policy",
    selected = expand_pbc_selection(ctrl$related_policy %||% "", pbc_registry)
  )
  updateSelectizeInput(
    session, "related_documents",
    choices = {
      rd <- trimws(as.character(ctrl$related_documents %||% ""))
      if (nzchar(rd)) stats::setNames(rd, rd) else character(0)
    },
    selected = trimws(as.character(ctrl$related_documents %||% "")),
    server = FALSE
  )
  updateSelectizeInput(session, "related_law",
                       selected = {
                         raw <- trimws(as.character(ctrl$related_law %||% ""))
                         if (!nzchar(raw)) character(0) else trimws(unlist(strsplit(raw, "[;；|/]+")))
                       })
  updateTextInput(session, "related_law_url",
                  value = trimws(as.character(ctrl$related_law_url %||% "")))
  doc_ids <- parse_pbc_id_values(ctrl$related_document_pbc_ids)
  doc_sel <- expand_pbc_selection(
    ctrl$related_document %||% ctrl$outputs %||% "",
    pbc_registry,
    stored_ids = doc_ids
  )
  updateSelectizeInput(session, "related_document_pbc", selected = doc_sel)
  updateTextAreaInput(session, "control_objective", value = ctrl$control_objective %||% "")
  updateTextAreaInput(session, "control_activity", value = ctrl$control_activity %||% "")
  at <- normalize_control_activity_type_pd(ctrl$approach %||% ctrl$control_activity_type)
  ct <- normalize_control_type_manual_auto(ctrl$nature %||% ctrl$control_type)
  if (nzchar(at)) updateSelectInput(session, "approach", selected = at)
  if (nzchar(ct)) updateSelectInput(session, "nature", selected = ct)
  freq <- resolve_control_frequency(ct, ctrl$frequency %||% "")
  if (nzchar(freq)) {
    if (!(freq %in% FREQUENCY_CHOICES)) {
      updateSelectInput(session, "frequency",
                        choices = unique(c(FREQUENCY_CHOICES, freq)), selected = freq)
    } else {
      updateSelectInput(session, "frequency", selected = freq)
    }
  }
  updateSelectizeInput(
    session, "responsible_unit",
    choices = {
      ru <- trimws(as.character(ctrl$responsible_unit %||% ""))
      if (nzchar(ru)) stats::setNames(ru, ru) else character(0)
    },
    selected = trimws(as.character(ctrl$responsible_unit %||% "")),
    server = FALSE
  )
  updateSelectizeInput(
    session, "iuc",
    selected = expand_pbc_selection(
      ctrl$iuc %||% ctrl$iuc_or_system %||% "",
      pbc_registry
    )
  )
  updateTextInput(session, "related_system", value = ctrl$related_system %||% "")
  as_vals <- parse_assertion_values(normalize_assertions_for_category(
    ctrl$assertions, ctrl$risk_category %||% ""
  ))
  updateSelectizeInput(
    session, "assertions",
    choices = assertion_choices_for_category(ctrl$risk_category %||% ""),
    selected = as_vals
  )
  if (nzchar(ctrl$type %||% "")) {
    updateSelectizeInput(session, "type", selected = ctrl$type)
  }
  updateTextAreaInput(session, "inputs", value = ctrl$inputs %||% "")
  updateTextAreaInput(session, "review_steps", value = ctrl$review_steps %||% "")
  updateTextAreaInput(session, "outputs", value = ctrl$outputs %||% ctrl$related_document %||% "")
  updateTextAreaInput(session, "investigation_threshold", value = ctrl$investigation_threshold %||% "")
  invisible(ctrl)
}

build_risk_factor_choices <- function(rows,
                                      empty_label = "請選擇風險因素…",
                                      include_custom = TRUE,
                                      extra_selected = NULL) {
  ch_risk <- if (length(rows)) cascade_risk_choices(rows) else character()
  extra <- trimws(as.character(extra_selected %||% ""))
  if (nzchar(extra) && !(extra %in% unname(ch_risk))) {
    ch_risk <- c(stats::setNames(extra, risk_factor_tag(extra)), ch_risk)
  }
  ch <- c(stats::setNames("", empty_label), ch_risk)
  if (isTRUE(include_custom)) ch <- c(ch, "＋自訂新增風險" = "__custom__")
  ch
}

cycle_risk_rows <- function(lib, cycle, sub_key = NULL) {
  if (!nzchar(trimws(cycle %||% ""))) return(list())
  rows <- library_controls_flat(lib, cycle = cycle)
  if (!is.null(sub_key) && nzchar(sub_key) && !identical(sub_key, "__custom__")) {
    rows <- filter_cascade_rows(rows, sub_key = sub_key)
  }
  rows
}

apply_risk_detail_to_inputs <- function(session, rows, risk_factor) {
  if (!nzchar(risk_factor) || identical(risk_factor, "__custom__")) {
    return(invisible(NULL))
  }
  det <- cascade_risk_detail(rows, risk_factor)
  updateSelectizeInput(session, "risk_factor", selected = risk_factor)
  # 風險描述為質性文字：選 TAG 不覆寫已填內容
  if (nzchar(det$risk_category)) {
    updateSelectInput(session, "risk_category", selected = det$risk_category)
  }
  r <- det$sample
  if (is.list(r) && length(r) && nzchar(trimws(r$romm_classification %||% ""))) {
    updateSelectInput(session, "romm_classification", selected = r$romm_classification)
  }
  invisible(det)
}

cascade_risk_choices <- function(rows) {
  # 風險因素＝風險描述上的 TAG；選單 value／label 皆為短標記，可自訂新增
  if (!length(rows)) return(character())
  tags <- unique(unlist(lapply(rows, function(r) {
    vals <- parse_risk_factor_values(r$risk_factor %||% r$risk_name %||% "")
    if (!length(vals)) return(character())
    vapply(vals, risk_factor_tag, character(1))
  }), use.names = FALSE))
  tags <- tags[nzchar(tags)]
  if (!length(tags)) return(character())
  stats::setNames(tags, tags)
}

# 依已選 TAG 推薦曾被標記的風險描述；未選 TAG 時列出範圍內全部描述（仍可自訂）
cascade_risk_description_choices <- function(rows, risk_factors = character(0)) {
  if (!length(rows)) return(character(0))
  tags <- unique(c(
    parse_risk_factor_values(risk_factors),
    vapply(parse_risk_factor_values(risk_factors), risk_factor_tag, character(1))
  ))
  tags <- tags[nzchar(tags) & tags != "__custom__"]
  scoped <- if (length(tags)) {
    Filter(function(r) row_matches_risk_factor(r, tags), rows)
  } else {
    rows
  }
  descs <- unique(vapply(scoped, function(r) nzchar_trim(r$risk_description), character(1)))
  descs[nzchar(descs)]
}

risk_description_select_choices <- function(descs) {
  descs <- unique(trimws(as.character(descs)))
  descs <- descs[nzchar(descs)]
  if (!length(descs)) return(character())
  labels <- vapply(descs, function(d) {
    if (nchar(d) > 80) paste0(substr(d, 1, 79), "…") else d
  }, character(1))
  stats::setNames(descs, labels)
}

cascade_risk_detail <- function(rows, risk_factor) {
  hit <- Filter(function(r) row_matches_risk_factor(r, risk_factor), rows)
  if (!length(hit)) {
    return(list(
      risk_factor = risk_factor, risk_category = "", risk_description = "",
      attrs = character()
    ))
  }
  r <- hit[[1]]
  attrs <- c(
    if (nzchar(r$risk_attr_financial)) paste0("財務報導：", gsub("^\\[[^\\]]+\\]\\s*", "", r$risk_attr_financial)),
    if (nzchar(r$risk_attr_operations)) paste0("營運：", gsub("^\\[[^\\]]+\\]\\s*", "", r$risk_attr_operations)),
    if (nzchar(r$risk_attr_compliance)) paste0("法令遵循：", gsub("^\\[[^\\]]+\\]\\s*", "", r$risk_attr_compliance)),
    if (nzchar(r$risk_category)) paste0("風險類別：", r$risk_category)
  )
  list(
    risk_factor = r$risk_factor,
    risk_category = r$risk_category,
    risk_description = r$risk_description,
    attrs = attrs,
    sample = r
  )
}

cascade_objective_choices <- function(rows) {
  objs <- unique(vapply(rows, function(r) r$control_objective, character(1)))
  objs <- objs[nzchar(objs)]
  labels <- vapply(objs, function(o) {
    if (nchar(o) > 56) paste0(substr(o, 1, 56), "…") else o
  }, character(1))
  stats::setNames(objs, labels)
}

cascade_activity_choices <- function(rows) {
  # Only activities with a single PD attribute
  keys <- character()
  labels <- character()
  seen <- character()
  for (r in rows) {
    if (!nzchar(r$control_activity)) next
    at <- r$approach
    if (!nzchar(at)) next # skip until typed; custom path can set
    k <- activity_key(r$control_activity, at)
    if (k %in% seen) next
    seen <- c(seen, k)
    keys <- c(keys, k)
    short <- if (nchar(r$control_activity) > 42) {
      paste0(substr(r$control_activity, 1, 42), "…")
    } else r$control_activity
    labels <- c(labels, sprintf("[%s] %s", at, short))
  }
  stats::setNames(keys, labels)
}

cascade_iuc_choices <- function(rows, pbc_df = NULL) {
  iucs <- unique(vapply(rows, function(r) r$iuc_or_system, character(1)))
  iucs <- iucs[nzchar(iucs)]
  if (!is.null(pbc_df) && nrow(pbc_df)) {
    pbc_use <- pbc_df
    if (exists("filter_pbc_registry", mode = "function")) {
      pbc_use <- filter_pbc_registry(pbc_df, exclude_kinds = PBC_KIND_POLICY)
    } else if (exists("PBC_KIND_POLICY", mode = "variable")) {
      pbc_use <- pbc_df[!vapply(pbc_df$pbc_kind, function(k) {
        identical(trimws(as.character(k %||% "")), PBC_KIND_POLICY)
      }, logical(1)), , drop = FALSE]
    }
    extra <- unique(c(
      as.character(pbc_use$reviewed_name),
      as.character(pbc_use$client_pbc_name),
      as.character(pbc_use$iuc_or_system),
      if (exists("format_pbc_reviewed_label", mode = "function")) {
        vapply(seq_len(nrow(pbc_use)), function(i) {
          format_pbc_reviewed_label(pbc_use$reviewed_name[i], pbc_use$pbc_kind[i])
        }, character(1))
      } else character()
    ))
    extra <- extra[nzchar(extra) & !is.na(extra)]
    iucs <- unique(c(iucs, extra))
  }
  stats::setNames(iucs, iucs)
}

# Selection completeness for unlocking 公司現況
cascade_selection_ready <- function(sel) {
  req <- c("cycle", "risk_factor", "control_objective",
           "control_activity", "approach", "iuc_or_system")
  missing <- character()
  for (f in req) {
    if (!nzchar(nzchar_trim(sel[[f]]))) missing <- c(missing, f)
  }
  if (!activity_type_ok(sel$approach %||% "")) {
    missing <- c(missing, "approach_single")
  }
  list(ready = !length(missing), missing = missing)
}

# Six-rule gate for status writing (前置六大控制項目)
six_status_rules_check <- function(ctrl) {
  missing <- character()
    labels <- c(
    nature = "控制性質（人工／自動）",
    approach = "控制方式（預防／偵測／矯正）",
    frequency = "控制頻率",
    responsible_unit = "控制點負責單位",
    iuc_or_system = CONTROL_IUC_DOCUMENT_LABEL,
    control_activity = "控制活動"
  )
  for (f in names(labels)) {
    val <- if (identical(f, "iuc_or_system")) {
      ctrl_iuc_value(ctrl)
    } else if (identical(f, "approach")) {
      normalize_single_activity_type(ctrl$approach)
    } else nzchar_trim(ctrl[[f]])
    if (!nzchar(val)) missing <- c(missing, labels[[f]])
  }
  list(ok = !length(missing), missing = missing)
}

# Scaffold 公司現況 using six rules + selected cascade fields
assemble_status_scaffold <- function(ctrl) {
  chk <- six_status_rules_check(ctrl)
  header <- paste0(
    "【六大控制項目就緒】\n",
    sprintf("1. 控制類型：%s\n", nzchar_or(ctrl$nature, "（缺）")),
    sprintf("2. 控制活動類型：%s\n", nzchar_or(normalize_single_activity_type(ctrl$approach), "（缺）")),
    sprintf("3. 控制頻率：%s\n", nzchar_or(ctrl$frequency, "（缺）")),
    sprintf("4. 負責單位：%s\n", nzchar_or(ctrl$responsible_unit, "（缺）")),
    sprintf("5. IUC：%s\n", nzchar_or(ctrl_iuc_value(ctrl), "（缺）")),
    sprintf("   相關系統：%s\n",
            if (is_automatic_control(ctrl$nature %||% ctrl$control_type)) {
              nzchar_or(ctrl_related_system_value(ctrl), "（缺）")
            } else {
              nzchar_or(ctrl_related_system_value(ctrl), "（可空）")
            }),
    sprintf("6. 控制活動：%s\n", nzchar_or(ctrl$control_activity, "（缺）")),
    "----\n",
    sprintf("控制目標：%s\n", nzchar_or(ctrl$control_objective, "（缺）")),
    sprintf("風險：%s｜%s\n",
            nzchar_or(ctrl$risk_factor %||% ctrl$risk_name, "（缺）"),
            nzchar_or(ctrl$risk_description, "（無描述）")),
    "----\n請依上列六大項目書寫公司實際現況（可改寫下列草稿）：\n"
  )
  draft <- if (exists("assemble_control_paragraph", mode = "function")) {
    tryCatch(assemble_control_paragraph(ctrl), error = function(e) "")
  } else ""
  if (!chk$ok) {
    return(paste0(
      "（尚不可書寫現況，缺：", paste(chk$missing, collapse = "、"), "）\n",
      header
    ))
  }
  paste0(header, draft)
}

# Auto-sequence control ID like EC-101-01 based on sub_process_id
collect_existing_control_ids <- function(..., lists = list()) {
  ids <- character()
  for (lst in lists) {
    if (!length(lst)) next
    for (item in lst) {
      cid <- if (is.list(item) && !is.null(item$control)) {
        item$control$control_id %||% item$control_id %||% ""
      } else {
        item$control_id %||% ""
      }
      ids <- c(ids, nzchar_trim(cid))
    }
  }
  unique(ids[nzchar(ids)])
}

next_rcm_control_id <- function(sub_process_id, existing_ids = character(),
                                cycle_code = "") {
  sp <- nzchar_trim(sub_process_id)
  cc <- trimws(as.character(cycle_code %||% ""))
  # 正規化為 [循環]-[子作業序號]
  if (nzchar(sp)) {
    parts <- parse_rcm_id_parts(sp)
    if (isTRUE(parts$ok) && nzchar(parts$sub)) {
      if (!nzchar(cc)) cc <- parts$cycle
      if (nzchar(cc)) sp <- compose_sub_process_id(cc, parts$sub)
    } else if (nzchar(cc)) {
      sn <- sub_process_seq_from_id(sp, cc)
      if (nzchar(sn)) sp <- compose_sub_process_id(cc, sn)
    }
  }
  if (!nzchar(sp)) {
    if (nzchar(cc)) sp <- compose_sub_process_id(cc, "001") else sp <- "SP-001"
  }
  # Match {sp}-NN at end → 控制編號 = [循環]-[子作業序號]-[控制序號]
  pat <- paste0("^", gsub("([.\\-])", "\\\\\\1", sp), "-(\\d+)$")
  nums <- integer()
  for (id in existing_ids) {
    m <- regmatches(id, regexec(pat, id, perl = TRUE))[[1]]
    if (length(m) >= 2) nums <- c(nums, as.integer(m[[2]]))
  }
  next_n <- if (length(nums)) max(nums) + 1L else 1L
  parts <- parse_rcm_id_parts(sp)
  if (isTRUE(parts$ok) && nzchar(parts$cycle) && nzchar(parts$sub)) {
    return(compose_control_id(parts$cycle, parts$sub, next_n))
  }
  sprintf("%s-%02d", sp, next_n)
}

# Pick best matching library row for a cascade selection
match_cascade_control <- function(rows, sel) {
  filtered <- filter_cascade_rows(
    rows,
    sub_key = if (nzchar(sel$sub_process_id %||% "")) {
      sub_process_key(sel$sub_process_id, sel$sub_process %||% "")
    } else NULL,
    risk_factor = sel$risk_factor,
    objective = sel$control_objective,
    activity_key_sel = if (nzchar(sel$control_activity %||% "")) {
      activity_key(sel$control_activity, sel$approach)
    } else NULL
  )
  if (!length(filtered)) return(NULL)
  # Prefer rows matching IUC if set
  iuc <- nzchar_trim(sel$iuc_or_system)
  if (nzchar(iuc)) {
    hit <- Filter(function(r) identical(r$iuc_or_system, iuc), filtered)
    if (length(hit)) return(hit[[1]]$raw %||% hit[[1]])
  }
  filtered[[1]]$raw %||% filtered[[1]]
}

# Persist custom cascade entry into library as new template
custom_cascade_to_library_item <- function(sel, tags = c("自訂新增")) {
  ctrl <- list(
    cycle = sel$cycle %||% "",
    sub_process_id = sel$sub_process_id %||% "",
    sub_process = sel$sub_process %||% "",
    risk_factor = sel$risk_factor %||% "",
    risk_name = sel$risk_name %||% sel$risk_factor %||% "",
    risk_description = sel$risk_description %||% "",
    risk_category = sel$risk_category %||% "",
    control_objective = sel$control_objective %||% "",
    control_activity = sel$control_activity %||% "",
    nature = sel$nature %||% "",
    approach = normalize_single_activity_type(sel$approach),
    frequency = sel$frequency %||% "",
    responsible_unit = sel$responsible_unit %||% "",
    iuc_or_system = sel$iuc %||% sel$iuc_or_system %||% "",
    iuc = sel$iuc %||% sel$iuc_or_system %||% "",
    related_system = sel$related_system %||% "",
    control_id = sel$control_id %||% "",
    significant_account = {
      ac <- nzchar_trim(sel$significant_account)
      if (is_reporting_risk_category(sel$risk_category %||% "")) ac else ""
    }
  )
  if (exists("library_item_from_control", mode = "function")) {
    library_item_from_control(ctrl, tags = tags)
  } else {
    list(library_id = paste0("CUSTOM-", as.integer(Sys.time())), title = ctrl$control_objective,
         tags = tags, cycle = ctrl$cycle, control = ctrl)
  }
}

# ── 設計頁籤頂部簡約搜尋（依範本庫快速找欄位值）──────────────────────────
.tab_search_limit <- 12L

search_sub_process_hits <- function(rows, keyword = "", limit = .tab_search_limit) {
  kw <- trimws(as.character(keyword %||% ""))
  seen <- character()
  out <- list()
  for (r in rows) {
    nm <- nzchar_trim(r$sub_process)
    id <- nzchar_trim(r$sub_process_id)
    if (!nzchar(nm)) next
    if (nzchar(kw) && !grepl(kw, nm, fixed = TRUE) &&
        !grepl(kw, id, fixed = TRUE) &&
        !grepl(kw, nm, ignore.case = TRUE)) next
    key <- paste(id, nm, sep = "\t")
    if (key %in% seen) next
    seen <- c(seen, key)
    out[[length(out) + 1L]] <- list(
      sub_process = nm,
      sub_process_id = id,
      label = nm
    )
    if (length(out) >= limit) break
  }
  out
}

search_risk_description_hits <- function(rows, category = "", factor_kw = "",
                                         limit = .tab_search_limit) {
  cat <- trimws(as.character(category %||% ""))
  fkw <- trimws(as.character(factor_kw %||% ""))
  seen <- character()
  out <- list()
  for (r in rows) {
    if (nzchar(cat) && !identical(nzchar_trim(r$risk_category), cat)) next
    rf <- nzchar_trim(r$risk_factor %||% r$risk_name)
    desc <- nzchar_trim(r$risk_description)
    if (!nzchar(desc)) next
    if (nzchar(fkw) && !grepl(fkw, rf, ignore.case = TRUE) &&
        !grepl(fkw, desc, ignore.case = TRUE)) next
    key <- paste(rf, desc, sep = "\t")
    if (key %in% seen) next
    seen <- c(seen, key)
    out[[length(out) + 1L]] <- list(
      risk_factor = rf,
      risk_category = nzchar_trim(r$risk_category),
      risk_description = desc,
      label = {
        short <- if (nchar(desc) > 48) paste0(substr(desc, 1, 48), "…") else desc
        if (nzchar(rf)) sprintf("[%s] %s", rf, short) else short
      }
    )
    if (length(out) >= limit) break
  }
  out
}

search_control_activity_hits <- function(rows, approach = "", nature = "",
                                         limit = .tab_search_limit) {
  ap <- trimws(as.character(approach %||% ""))
  nat <- trimws(as.character(nature %||% ""))
  if (nzchar(ap) && exists("normalize_single_activity_type", mode = "function")) {
    ap_n <- normalize_single_activity_type(ap)
    if (nzchar(ap_n)) ap <- ap_n
  }
  if (nzchar(nat) && exists("normalize_control_type_manual_auto", mode = "function")) {
    nat_n <- normalize_control_type_manual_auto(nat)
    if (nzchar(nat_n)) nat <- nat_n
  }
  seen <- character()
  out <- list()
  for (r in rows) {
    act <- nzchar_trim(r$control_activity)
    if (!nzchar(act)) next
    r_ap <- nzchar_trim(r$approach)
    r_nat <- nzchar_trim(r$nature)
    if (nzchar(ap)) {
      r_ap_n <- if (exists("normalize_single_activity_type", mode = "function"))
        normalize_single_activity_type(r_ap) else r_ap
      if (!identical(r_ap_n, ap) && !grepl(ap, r_ap, fixed = TRUE)) next
    }
    if (nzchar(nat)) {
      r_nat_n <- if (exists("normalize_control_type_manual_auto", mode = "function"))
        normalize_control_type_manual_auto(r_nat) else r_nat
      if (!identical(r_nat_n, nat) && !identical(r_nat, nat)) next
    }
    key <- act
    if (key %in% seen) next
    seen <- c(seen, key)
    out[[length(out) + 1L]] <- list(
      control_activity = act,
      approach = r_ap,
      nature = r_nat,
      label = {
        short <- if (nchar(act) > 56) paste0(substr(act, 1, 56), "…") else act
        bits <- c(if (nzchar(r_nat)) r_nat, if (nzchar(r_ap)) r_ap)
        if (length(bits)) sprintf("%s — %s", paste(bits, collapse = "／"), short) else short
      }
    )
    if (length(out) >= limit) break
  }
  out
}
