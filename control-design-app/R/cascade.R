# Guided cascade: cycle → 子作業 → 風險 → 目標 → 活動(單一預防/偵測) → IUC
# Company status unlocks only after cascade is complete.
# 「六大控制項目」規則：組裝現況描述前必須就緒的設計要素。

# 六大控制項目（現況書寫前置規則）
SIX_CONTROL_STATUS_RULES <- c(
  control_objective = "控制目標",
  control_activity = "控制活動",
  nature = "控制類型（人工／自動）",
  approach = "控制活動類型（預防／偵測）",
  frequency = "控制頻率",
  responsible_unit = "流程負責單位",
  iuc_or_system = "IUC"
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

# Enforce: one activity ↔ exactly one 預防/偵測 attribute
normalize_single_activity_type <- function(approach) {
  raw <- nzchar_trim(approach)
  if (!nzchar(raw)) return("")
  # Reject mixed before normalizing
  if (grepl("預防|Preventive", raw, ignore.case = TRUE) &&
      grepl("偵測|Detective", raw, ignore.case = TRUE)) {
    return("")
  }
  if (grepl("預防\\+|預防＋|Preventive\\s*\\+", raw, ignore.case = TRUE)) return("")
  at <- if (exists("normalize_control_activity_type_pd", mode = "function")) {
    normalize_control_activity_type_pd(raw)
  } else raw
  if (at %in% c("預防性控制", "偵測性控制")) return(at)
  if (grepl("預防", at)) return("預防性控制")
  if (grepl("偵測", at)) return("偵測性控制")
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
cascade_source_library <- function(user_library = list()) {
  builtin <- tryCatch(seed_control_library(TRUE), error = function(e) list())
  if (!length(user_library)) return(builtin)
  if (!exists("merge_libraries", mode = "function")) {
    return(c(builtin, user_library))
  }
  merge_libraries(builtin, user_library, overwrite = FALSE)
}

# 循環編號前綴（子作業／控制編號同步用）
KNOWN_CYCLE_CODES <- unique(unname(CYCLE_CODE_MAP))

recode_id_cycle_prefix <- function(id, new_cycle_code) {
  id <- trimws(as.character(id %||% ""))
  new_cycle_code <- trimws(as.character(new_cycle_code %||% ""))
  if (!nzchar(id) || !nzchar(new_cycle_code)) return(id)
  m <- regmatches(id, regexec("^([A-Z]{2})-", id))[[1]]
  if (length(m) < 2L) return(id)
  old_code <- m[[2]]
  if (!(old_code %in% KNOWN_CYCLE_CODES)) return(id)
  sub(paste0("^", old_code, "-"), paste0(new_cycle_code, "-"), id)
}

recode_sub_process_key <- function(key, new_cycle_code) {
  key <- trimws(as.character(key %||% ""))
  if (!nzchar(key) || !grepl("\\|\\|", key, fixed = FALSE)) return(key)
  sp <- parse_sub_process_key(key)
  new_id <- recode_id_cycle_prefix(sp$id, new_cycle_code)
  if (identical(new_id, sp$id)) return(key)
  sub_process_key(new_id, sp$name)
}

# 循環編號變更時，同步子作業編號、控制編號與子作業名稱 key
sync_form_ids_to_cycle_code <- function(session, new_cycle_code,
                                        sub_process_id = "",
                                        control_id = "",
                                        sub_process = "") {
  new_cycle_code <- trimws(as.character(new_cycle_code %||% ""))
  if (!nzchar(new_cycle_code)) return(invisible(NULL))

  spid <- trimws(as.character(sub_process_id %||% ""))
  if (nzchar(spid)) {
    new_spid <- recode_id_cycle_prefix(spid, new_cycle_code)
    if (!identical(new_spid, spid)) {
      updateTextInput(session, "sub_process_id", value = new_spid)
    }
  }

  cid <- trimws(as.character(control_id %||% ""))
  if (nzchar(cid)) {
    new_cid <- recode_id_cycle_prefix(cid, new_cycle_code)
    if (!identical(new_cid, cid)) {
      updateTextInput(session, "control_id", value = new_cid)
    }
  }

  sub_val <- trimws(as.character(sub_process %||% ""))
  if (nzchar(sub_val)) {
    new_sub <- recode_sub_process_key(sub_val, new_cycle_code)
    if (!identical(new_sub, sub_val)) {
      updateSelectizeInput(session, "sub_process", selected = new_sub)
    }
  }
  invisible(NULL)
}

sub_process_key <- function(spid, spn) {
  sprintf("%s||%s", nzchar_trim(spid), nzchar_trim(spn))
}

parse_sub_process_key <- function(key) {
  parts <- strsplit(nzchar_trim(key), "\\|\\|", perl = TRUE)[[1]]
  list(id = parts[[1]] %||% "", name = if (length(parts) > 1) parts[[2]] else "")
}

# 子作業名稱 selectize：值可為 key（id||name）或自訂名稱
sub_process_filter_key <- function(spid, spn) {
  spn <- trimws(as.character(spn %||% ""))
  spid <- trimws(as.character(spid %||% ""))
  if (grepl("\\|\\|", spn, fixed = FALSE)) return(spn)
  key <- sub_process_key(spid, spn)
  if (nzchar(spid) || nzchar(spn)) key else ""
}

sub_process_name_from_value <- function(val) {
  val <- trimws(as.character(val %||% ""))
  if (!nzchar(val)) return("")
  if (grepl("\\|\\|", val, fixed = FALSE)) parse_sub_process_key(val)$name else val
}

sub_process_id_from_value <- function(val, fallback_id = "") {
  val <- trimws(as.character(val %||% ""))
  fb <- trimws(as.character(fallback_id %||% ""))
  if (grepl("\\|\\|", val, fixed = FALSE)) {
    sp <- parse_sub_process_key(val)
    if (nzchar(sp$id)) sp$id else fb
  } else {
    fb
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
  if (!is.null(risk_factor) && nzchar(risk_factor) && !identical(risk_factor, "__custom__")) {
    out <- Filter(function(r) {
      identical(r$risk_factor, risk_factor) || identical(r$risk_name, risk_factor)
    }, out)
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
  keys <- unique(vapply(rows, function(r) {
    if (!nzchar(r$sub_process_id) && !nzchar(r$sub_process)) return("")
    sub_process_key(r$sub_process_id, r$sub_process)
  }, character(1)))
  keys <- keys[nzchar(keys)]
  labels <- vapply(keys, sub_process_choice_label, character(1))
  # 選項僅顯示名稱；同名子作業保留第一筆
  keep <- !duplicated(labels)
  stats::setNames(keys[keep], labels[keep])
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
apply_ctrl_to_cascade <- function(session, ctrl) {
  ctrl <- as.list(ctrl)
  if (nzchar(ctrl$cycle %||% "")) {
    updateSelectInput(session, "cycle", selected = ctrl$cycle)
    updateTextInput(session, "cycle_code",
                    value = {
                      cc <- trimws(ctrl$cycle_code %||% "")
                      if (nzchar(cc)) cc else cycle_code_for(ctrl$cycle)
                    })
  }
  invisible(ctrl)
}

apply_supplement_from_ctrl <- function(session, ctrl, pbc_registry = NULL) {
  ctrl <- as.list(ctrl)
  updateTextInput(session, "control_id", value = ctrl$control_id %||% ctrl$library_id %||% "")
  updateTextInput(session, "cycle_code", value = {
    cc <- trimws(ctrl$cycle_code %||% "")
    if (nzchar(cc)) cc else cycle_code_for(ctrl$cycle %||% "")
  })
  updateTextInput(session, "sub_process_id", value = ctrl$sub_process_id %||% "")
  sp_key <- sub_process_key(ctrl$sub_process_id %||% "", ctrl$sub_process %||% "")
  sp_sel <- if (nzchar(sp_key) &&
               nzchar(trimws(ctrl$sub_process_id %||% "")) &&
               nzchar(trimws(ctrl$sub_process %||% ""))) {
    sp_key
  } else {
    trimws(ctrl$sub_process %||% "")
  }
  updateSelectizeInput(session, "sub_process", selected = sp_sel)
  rf_sel <- risk_factor_selection_from_ctrl(ctrl)
  updateSelectizeInput(session, "risk_factor", selected = rf_sel)
  updateTextAreaInput(session, "risk_description", value = ctrl$risk_description %||% "")
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
  updateTextInput(session, "related_policy", value = ctrl$related_policy %||% "")
  updateSelectizeInput(session, "related_law",
                       selected = {
                         raw <- trimws(as.character(ctrl$related_law %||% ""))
                         if (!nzchar(raw)) character(0) else trimws(unlist(strsplit(raw, "[;；|/]+")))
                       })
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
  updateTextInput(session, "responsible_unit", value = ctrl$responsible_unit %||% "")
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
  if (nzchar(det$risk_description)) {
    updateTextAreaInput(session, "risk_description", value = det$risk_description)
  }
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
  # value = canonical risk_factor; label = short tag（不含 []、不附描述）
  factors <- unique(vapply(rows, function(r) r$risk_factor, character(1)))
  factors <- factors[nzchar(factors)]
  tags <- vapply(factors, risk_factor_tag, character(1))
  labels <- tags
  dup <- unique(tags[duplicated(tags) | duplicated(tags, fromLast = TRUE)])
  if (length(dup)) {
    for (i in seq_along(factors)) {
      if (tags[i] %in% dup) {
        alt <- gsub("\\[|\\]", "", factors[i])
        labels[i] <- if (nchar(alt) > 28) paste0(substr(alt, 1, 27), "…") else alt
      }
    }
  }
  stats::setNames(factors, labels)
}

cascade_risk_detail <- function(rows, risk_factor) {
  hit <- Filter(function(r) {
    identical(r$risk_factor, risk_factor) || identical(r$risk_name, risk_factor)
  }, rows)
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
    extra <- unique(c(
      as.character(pbc_df$reviewed_name),
      as.character(pbc_df$client_pbc_name),
      as.character(pbc_df$iuc_or_system),
      if (exists("format_pbc_reviewed_label", mode = "function")) {
        vapply(seq_len(nrow(pbc_df)), function(i) {
          format_pbc_reviewed_label(pbc_df$reviewed_name[i], pbc_df$pbc_kind[i])
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
  req <- c("cycle", "sub_process_id", "risk_factor", "control_objective",
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
    nature = "控制類型（人工／自動）",
    approach = "控制活動類型（預防／偵測・單一）",
    frequency = "控制頻率",
    responsible_unit = "流程負責單位",
    iuc_or_system = "IUC",
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
      # also library_id JL-EC-101-01 → EC-101-01
      lid <- item$library_id %||% ""
      if (grepl("^JL-", lid)) cid <- c(cid, sub("^JL-", "", lid))
      ids <- c(ids, nzchar_trim(cid))
    }
  }
  unique(ids[nzchar(ids)])
}

next_rcm_control_id <- function(sub_process_id, existing_ids = character()) {
  sp <- nzchar_trim(sub_process_id)
  if (!nzchar(sp)) sp <- "SP-001"
  # Match {sp}-NN at end
  pat <- paste0("^", gsub("([.\\-])", "\\\\\\1", sp), "-(\\d+)$")
  nums <- integer()
  for (id in existing_ids) {
    m <- regmatches(id, regexec(pat, id, perl = TRUE))[[1]]
    if (length(m) >= 2) nums <- c(nums, as.integer(m[[2]]))
  }
  next_n <- if (length(nums)) max(nums) + 1L else 1L
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
