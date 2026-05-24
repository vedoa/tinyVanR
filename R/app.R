#' Run a tinyVanR App
#' @param app A tiny_app object
#' @param host Host string
#' @param port Port integer
#' @param quiet Logical. Suppress startup message.
#' @param log Function or NULL. Optional request logger.
#' @param static NULL or list. Static serving configuration(s).
#' @param timeout_ms Polling timeout in milliseconds. Default is 100ms.
#' @param num_threads Integer. Number of worker threads. Default is 50.
#' @param max_body_size Number. Maximum request body size in bytes. Default is 8MiB.
#' @param request_timeout_ms Integer. Socket receive timeout. Default is 30s.
#' @export
run_app <- function(
  app,
  host = "127.0.0.1",
  port = 8080L,
  quiet = FALSE,
  log = NULL,
  static = NULL,
  timeout_ms = 100L,
  num_threads = 50L,
  max_body_size = 8 * 1024 * 1024,
  request_timeout_ms = 30000L
) {

  
  sessions <- new.env(parent = emptyenv())

  # Robust path resolution for static assets
  tinyVanR_dir <- system.file("tinyVanR", package = "tinyVanR")
  if (tinyVanR_dir == "") {
    if (dir.exists("inst/tinyVanR")) {
      tinyVanR_dir <- "inst/tinyVanR"
    }
  }

  # --- Dynamic HTML Generation ---
  # Construct the full HTML document using vtag
  # This replaces the static index.html file
  html_doc <- vtag(
    "html",
    vtag(
      "head",
      vtag("title", app$title),
      vtag("script", type = "text/javascript", src = "/tinyvanr/van-1.6.0.min.js"),
      # Process custom CSS links
      lapply(app$css, function(item) {
        if (inherits(item, "tiny_ui")) return(item)
        vtag("link", rel = "stylesheet", href = item)
      }),
      # Process custom head elements
      app$head
    ),
    vtag(
      "body",
      vtag("div", id = "root"),
      vtag("script", src = "/tinyvanr/client.js")
    )
  )

  # Register a GET handler for the root path to serve the dynamically generated HTML
  civetwebR::handle("GET", "/", function(req) {
    list(
      status = 200L,
      headers = list("Content-Type" = "text/html"),
      body = as_html(html_doc)
    )
  })

  # Ensure our package's static assets are always served under /tinyvanr
  all_static <- c(static, list(list(dir = tinyVanR_dir, prefix = "/tinyvanr")))

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

  if (!quiet) {
    message(sprintf("Running tinyVanR at http://%s:%d", host, port))
  }

  # Start the server
  civetwebR::serve(
    port = port,
    host = host,
    quiet = quiet,
    log = log,
    static = all_static,
    timeout_ms = timeout_ms,
    num_threads = num_threads,
    max_body_size = max_body_size,
    request_timeout_ms = request_timeout_ms
  )
}
