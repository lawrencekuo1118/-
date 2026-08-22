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
  iuc_or_system = "IUC／相關系統"
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
      iuc_or_system = nzchar_trim(c$related_system %||% c$iuc_or_system),
      related_document = nzchar_trim(c$related_document %||% c$outputs),
      company_status = nzchar_trim(c$company_status %||% c$detailed_description),
      control_id = nzchar_trim(c$control_id),
      significant_account = nzchar_trim(c$significant_account),
      design_gap_note = nzchar_trim(c$design_gap_note),
      related_policy = nzchar_trim(c$related_policy),
      related_law = nzchar_trim(c$related_law),
      raw = c
    )
  })
}

sub_process_key <- function(spid, spn) {
  sprintf("%s||%s", nzchar_trim(spid), nzchar_trim(spn))
}

parse_sub_process_key <- function(key) {
  parts <- strsplit(nzchar_trim(key), "\\|\\|", perl = TRUE)[[1]]
  list(id = parts[[1]] %||% "", name = if (length(parts) > 1) parts[[2]] else "")
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
  labels <- vapply(keys, function(k) {
    sp <- parse_sub_process_key(k)
    if (nzchar(sp$id) && nzchar(sp$name)) sprintf("%s｜%s", sp$id, sp$name)
    else if (nzchar(sp$id)) sp$id else sp$name
  }, character(1))
  stats::setNames(keys, labels)
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

# Apply library / RCM control into cascade wizard (single source for core design fields)
apply_ctrl_to_cascade <- function(session, ctrl) {
  ctrl <- as.list(ctrl)
  if (nzchar(ctrl$cycle %||% "")) {
    updateSelectInput(session, "cycle", selected = ctrl$cycle)
  }
  spid <- ctrl$sub_process_id %||% ""
  spn <- ctrl$sub_process %||% ""
  if (nzchar(spid) || nzchar(spn)) {
    updateSelectInput(session, "cascade_sub", selected = sub_process_key(spid, spn))
  }
  rf <- trimws(ctrl$risk_factor %||% ctrl$risk_name %||% "")
  if (nzchar(rf)) {
    updateSelectInput(session, "cascade_risk", selected = rf)
  }
  if (nzchar(ctrl$control_objective %||% "")) {
    updateSelectInput(session, "cascade_objective", selected = ctrl$control_objective)
  }
  act <- ctrl$control_activity %||% ""
  if (nzchar(act)) {
    updateSelectInput(
      session, "cascade_activity",
      selected = activity_key(act, ctrl$approach %||% ctrl$control_activity_type)
    )
  }
  iuc <- trimws(ctrl$iuc_or_system %||% ctrl$related_system %||% "")
  if (nzchar(iuc)) {
    updateSelectInput(session, "cascade_iuc", selected = iuc)
  }
  invisible(ctrl)
}

apply_supplement_from_ctrl <- function(session, ctrl) {
  ctrl <- as.list(ctrl)
  updateTextInput(session, "control_id", value = ctrl$control_id %||% ctrl$library_id %||% "")
  updateTextInput(session, "significant_account",
                  value = {
                    ac <- trimws(as.character(ctrl$significant_account %||% ""))
                    if (is_reporting_risk_category(ctrl$risk_category %||% "")) {
                      if (!nzchar(ac) || identical(toupper(ac), "NA")) "" else ac
                    } else {
                      ""
                    }
                  })
  updateTextInput(session, "related_policy", value = ctrl$related_policy %||% "")
  updateSelectizeInput(session, "related_law",
                       selected = {
                         raw <- trimws(as.character(ctrl$related_law %||% ""))
                         if (!nzchar(raw)) character(0) else trimws(unlist(strsplit(raw, "[;；|/]+")))
                       })
  updateTextInput(session, "related_document",
                  value = ctrl$related_document %||% ctrl$outputs %||% "")
  strip <- function(x) gsub("^\\[[^\\]]+\\]\\s*", "", trimws(as.character(x %||% "")))
  kind <- risk_attr_kind_from_ctrl(ctrl)
  detail <- risk_attr_detail_from_ctrl(ctrl)
  updateRadioButtons(session, "risk_attr_kind", selected = kind)
  updateTextAreaInput(session, "risk_attr_detail", value = detail)
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
  if (nzchar(det$risk_description)) {
    updateTextAreaInput(session, "risk_description", value = det$risk_description)
  }
  if (nzchar(det$risk_category)) {
    updateSelectInput(session, "risk_category", selected = det$risk_category)
  }
  updateTextInput(session, "risk_name", value = risk_factor_tag(risk_factor))
  r <- det$sample
  kind <- risk_attr_kind_from_category(det$risk_category)
  if (!nzchar(kind) && is.list(r) && length(r)) {
    kind <- risk_attr_kind_from_ctrl(r)
  }
  if (!nzchar(kind)) kind <- "operations"
  detail <- ""
  if (is.list(r) && length(r)) {
    strip <- function(x) gsub("^\\[[^\\]]+\\]\\s*", "", trimws(as.character(x %||% "")))
    detail <- switch(kind,
                     financial = strip(r$risk_attr_financial),
                     operations = strip(r$risk_attr_operations),
                     compliance = strip(r$risk_attr_compliance),
                     "")
    if (!nzchar(detail)) {
      for (f in c(r$risk_attr_financial, r$risk_attr_operations, r$risk_attr_compliance)) {
        d <- strip(f)
        if (nzchar(d)) { detail <- d; break }
      }
    }
  }
  updateRadioButtons(session, "risk_attr_kind", selected = kind)
  updateTextAreaInput(session, "risk_attr_detail", value = detail)
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
    iuc_or_system = "IUC／相關系統",
    control_activity = "控制活動"
  )
  for (f in names(labels)) {
    val <- if (identical(f, "iuc_or_system")) {
      nzchar_trim(ctrl$iuc_or_system %||% ctrl$related_system)
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
    sprintf("5. IUC／相關系統：%s\n", nzchar_or(ctrl$iuc_or_system %||% ctrl$related_system, "（缺）")),
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
    iuc_or_system = sel$iuc_or_system %||% "",
    related_system = sel$iuc_or_system %||% "",
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
