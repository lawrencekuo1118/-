# High-privilege access for editing 範本庫 / 參數庫 on disk.
# Override plaintext via env CONTROL_DESIGN_ADMIN_PASSWORD (shinyapps.io Settings → Vars).
# Default (when env unset): 1118 — do not show this in the UI.

hash_admin_password <- function(pw) {
  digest::digest(as.character(pw %||% ""), algo = "sha256", serialize = FALSE)
}

.DEFAULT_ADMIN_PASSWORD_SHA256 <- hash_admin_password("1118")

verify_admin_password <- function(pw) {
  pw <- trimws(as.character(pw %||% ""))
  if (!nzchar(pw)) return(FALSE)
  env_pw <- Sys.getenv("CONTROL_DESIGN_ADMIN_PASSWORD", unset = "")
  if (nzchar(env_pw)) {
    return(identical(pw, env_pw) ||
             identical(hash_admin_password(pw), hash_admin_password(env_pw)))
  }
  identical(hash_admin_password(pw), .DEFAULT_ADMIN_PASSWORD_SHA256)
}

#' Prompt high-privilege login modal (only when an edit is attempted).
show_admin_login_modal <- function(session = NULL) {
  if (is.null(session)) return(invisible(FALSE))
  showModal(
    modalDialog(
      title = "高權登入",
      easyClose = TRUE,
      footer = tagList(
        modalButton("取消"),
        actionButton("admin_login", "登入", class = "btn-primary btn-sm")
      ),
      p(class = "small text-muted mb-2",
        "修改範本庫或參數庫需高權。登入後此工作階段可繼續編輯。"),
      passwordInput("admin_password", NULL, placeholder = "高權密碼")
    ),
    session = session
  )
  invisible(FALSE)
}

require_admin <- function(is_admin, session = NULL) {
  if (isTRUE(is_admin)) return(TRUE)
  if (!is.null(session)) {
    show_admin_login_modal(session)
  }
  FALSE
}

# Enable/disable actionButton or downloadButton until prerequisites are met
set_action_button <- function(session, id, enabled, title = "") {
  if (is.null(session) || !nzchar(as.character(id %||% ""))) return(invisible(FALSE))
  session$sendCustomMessage("toggleButton", list(
    id = as.character(id),
    enabled = isTRUE(enabled),
    title = as.character(title %||% "")
  ))
  invisible(TRUE)
}
