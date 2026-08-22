# High-privilege access for editing 範本庫 / 參數庫 on disk.
# Override plaintext via env CONTROL_DESIGN_ADMIN_PASSWORD (shinyapps.io Settings → Vars).
# Default (when env unset): 尬電SOX#Admin — do not show this in the UI.

hash_admin_password <- function(pw) {
  digest::digest(as.character(pw %||% ""), algo = "sha256", serialize = FALSE)
}

.DEFAULT_ADMIN_PASSWORD_SHA256 <- hash_admin_password("尬電SOX#Admin")

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

require_admin <- function(is_admin, session = NULL) {
  if (isTRUE(is_admin)) return(TRUE)
  if (!is.null(session)) {
    showNotification("需高權登入後才能修改範本庫／參數庫", type = "error", duration = 5)
  }
  FALSE
}
