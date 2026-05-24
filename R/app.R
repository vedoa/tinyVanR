#' Run a tinyVanR App
#' @param app A tiny_app object
#' @param host Host string
#' @param port Port integer
#' @export
run_app <- function(app, host = "127.0.0.1", port = 8080) {
  sessions <- new.env(parent = emptyenv())

  # Robust path resolution for static assets
  tinyVanR_dir <- system.file("tinyVanR", package = "tinyVanR")
  if (tinyVanR_dir == "") {
    if (dir.exists("inst/tinyVanR")) tinyVanR_dir <- "inst/tinyVanR"
    else if (dir.exists("www")) tinyVanR_dir <- "www"
  }

  # Register Global WebSocket Handlers
  civetwebR::on_ws("open", function(req) {
    id <- as.character(req$id)
    sessions[[id]] <- create_session(id, app$ui, app$server)
    message(sprintf("Session opened: %s", id))
  })

  civetwebR::on_ws("message", function(req) {
    id <- as.character(req$id)
    sess <- sessions[[id]]
    if (!is.null(sess)) {
      dispatch_ws_message(sess, req$body)
    }
  })

  civetwebR::on_ws("close", function(req) {
    id <- as.character(req$id)
    if (exists(id, envir = sessions)) {
      rm(list = id, envir = sessions)
      message(sprintf("Session closed: %s", id))
    }
  })

  message(sprintf("Running tinyVanR at http://%s:%d", host, port))
  
  # Start the server
  civetwebR::serve(
    host = host,
    port = port,
    static = list(list(dir = tinyVanR_dir, prefix = "/")),
    timeout_ms = 10L
  )
}
