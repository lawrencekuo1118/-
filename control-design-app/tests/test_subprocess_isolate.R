# Regression: sub_process / sub_process_id edits must not re-trigger choice refresh
library(shiny)

`%||%` <- function(x, y) if (is.null(x)) y else x

n_refresh <- 0L
server_iso <- function(input, output, session) {
  refresh <- function() {
    n_refresh <<- n_refresh + 1L
    cy <- input$cycle %||% ""
    cur <- isolate(input$sub_process)
    spid <- isolate(input$sub_process_id)
    invisible(list(cy = cy, cur = cur, spid = spid))
  }
  observe({
    input$cycle
    refresh()
  })
}

testServer(server_iso, {
  session$setInputs(cycle = "資訊循環")
  after_cycle <- n_refresh
  session$setInputs(sub_process = "EC-101||存取管理")
  session$flushReact()
  after_name <- n_refresh
  session$setInputs(sub_process_id = "EC-101")
  session$flushReact()
  after_id <- n_refresh
  stopifnot(identical(after_name, after_cycle))
  stopifnot(identical(after_id, after_cycle))
  cat("OK: isolate prevents refresh on id/name edits\n")
})

n_refresh <- 0L
server_bug <- function(input, output, session) {
  refresh <- function() {
    n_refresh <<- n_refresh + 1L
    invisible(list(input$cycle, input$sub_process, input$sub_process_id))
  }
  observe({
    input$cycle
    refresh()
  })
}

testServer(server_bug, {
  session$setInputs(cycle = "資訊循環")
  base <- n_refresh
  session$setInputs(sub_process = "EC-101||x")
  session$flushReact()
  stopifnot(n_refresh > base)
  cat("OK: without isolate, name edit re-triggers refresh (bug pattern)\n")
})

cat("All subprocess isolate tests passed.\n")
