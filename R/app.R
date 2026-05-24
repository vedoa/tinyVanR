#' Launch a tinyVanR Application
#'
#' Configures and starts the civetwebR web server, sets up WebSocket routes,
#' and handles initial HTML generation and static asset serving.
#'
#' @param app A \code{tinyvanr_app} object created via \code{tinyvanr_app()}.
#' @param host Character. Bind address for the server. Default is "127.0.0.1".
#' @param port Integer. Port to listen on. Default is 8080.
#' @param quiet Logical. If TRUE, suppresses the startup notification.
#' @param log Function or NULL. Optional request logging function.
#' @param static List. Static directory configurations for the web server.
#' @param timeout_ms Integer. Polling timeout in milliseconds.
#' @param num_threads Integer. Number of worker threads.
#' @param max_body_size Numeric. Maximum allowed request body size in bytes.
#' @param request_timeout_ms Integer. Socket receive timeout in milliseconds.
#' @param custom Logical. If TRUE, disables automatic injection of framework JS/CSS and index handler.
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
  request_timeout_ms = 30000L,
  custom = FALSE
) {
  if (!inherits(app, "tinyvanr_app")) {
    stop("The 'app' argument must be of class 'tinyvanr_app'.", call. = FALSE)
  }

  sessions <- new.env(parent = emptyenv())

  # Initialize static assets list
  all_static <- static

  if (!custom) {
    # --- Dynamic HTML Generation ---
    html_doc <- vtag(
      "html",
      vtag(
        "head",
        app$head$before,
        vtag("title", app$title),
        vtag(
          "script",
          type = "text/javascript",
          src = "/tinyvanr/van-1.6.0.min.js"
        ),
        app$head$after
      ),
      vtag(
        "body",
        app$body$before,
        vtag("div", id = "root"),
        vtag("script", src = "/tinyvanr/client.js"),
        app$body$after
      )
    )

    # Register default GET handler
    civetwebR::handle("GET", "/", function(req) {
      list(
        status = 200L,
        headers = list("Content-Type" = "text/html"),
        body = paste0("<!DOCTYPE html>", as_html(html_doc))
      )
    })

    # Robust path resolution for static assets
    tinyVanR_dir <- normalizePath(system.file("tinyVanR", package = "tinyVanR"), mustWork = FALSE)
    if (tinyVanR_dir == "") {
      if (dir.exists("inst/tinyVanR")) {
        tinyVanR_dir <- "inst/tinyVanR"
      }
    }
    
    # Inject framework static assets
    all_static <- c(
      static,
      list(list(dir = tinyVanR_dir, prefix = "/tinyvanr"))
    )
  }

  # Register Global WebSocket Handlers
  civetwebR::on_ws("open", function(req) {
    id <- as.character(req$id)
    sessions[[id]] <- create_session(id, app$frontend, app$backend)
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
