# Locate installed browser assets, with a source-checkout fallback for development.
.tinyvanr_asset_dir <- function() {
  tinyVanR_dir <- normalizePath(
    system.file("tinyVanR", package = "tinyVanR"),
    mustWork = FALSE
  )
  if (tinyVanR_dir == "" && dir.exists("inst/tinyVanR")) {
    tinyVanR_dir <- normalizePath("inst/tinyVanR", mustWork = TRUE)
  }
  tinyVanR_dir
}

# Build the shell document into which the client renders the app UI.
.tinyvanr_html_document <- function(app) {
  vtag(
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
}

# Convert the shell document into the HTTP response for the root route.
.tinyvanr_index_response <- function(app) {
  list(
    status = 200L,
    headers = c("Content-Type" = "text/html; charset=utf-8"),
    body = paste0("<!DOCTYPE html>", as_html(.tinyvanr_html_document(app)))
  )
}

# Create WebSocket lifecycle callbacks backed by per-client app sessions.
.tinyvanr_ws_callbacks <- function(app, sessions) {
  list(
    on_open = function(ws, req) {
      id <- as.character(ws$id)
      sessions[[id]] <- create_session(id, app$frontend, app$backend, ws)
      message(sprintf("Session opened: %s", id))
    },
    on_message = function(ws, data) {
      id <- as.character(ws$id)
      sess <- sessions[[id]]
      if (!is.null(sess) && length(data) > 0) {
        msg <- if (is.raw(data)) rawToChar(data) else as.character(data)
        dispatch_ws_message(sess, msg)
      }
    },
    on_close = function(ws) {
      id <- as.character(ws$id)
      if (exists(id, envir = sessions)) {
        rm(list = id, envir = sessions)
        message(sprintf("Session closed: %s", id))
      }
    }
  )
}

# Turn user static-mount configurations into nanonext handler objects.
.tinyvanr_static_handlers <- function(static, listing_dir) {
  handlers <- list()
  for (config in static) {
    if (is.list(config)) {
      static_handler <- .static_config_handler(config, listing_dir)
      if (!is.null(static_handler)) {
        handlers <- c(handlers, list(static_handler))
      }
    }
  }
  handlers
}

# Assemble static, WebSocket, and optional root handlers in dispatch order.
.tinyvanr_handlers <- function(app, sessions, static = NULL, custom = FALSE) {
  asset_dir <- .tinyvanr_asset_dir()
  static_configs <- static
  handlers <- list()

  if (!custom) {
    handlers <- list(handler("/", function(req) .tinyvanr_index_response(app)))
    # Static framework assets are injected only for the built-in app page.
    if (!is.null(asset_dir) && nzchar(asset_dir)) {
      static_configs <- c(
        static_configs,
        list(list(dir = asset_dir, prefix = "/tinyvanr"))
      )
    }
  }

  static_handlers <- .tinyvanr_static_handlers(static_configs, asset_dir)
  callbacks <- .tinyvanr_ws_callbacks(app, sessions)
  ws_handler <- handler_ws(
    "/ws",
    on_open = callbacks$on_open,
    on_message = callbacks$on_message,
    on_close = callbacks$on_close,
    textframes = TRUE
  )

  # Keep static files and the WebSocket endpoint ahead of the root catch-all.
  c(static_handlers, list(ws_handler), handlers)
}

#' Launch a tinyVanR Application
#'
#' Configures and starts the nanonext web server, sets up WebSocket routes,
#' and handles initial HTML generation and static asset serving.
#'
#' @param app A \code{tinyvanr_app} object created via \code{tinyvanr_app()}.
#' @param host Character. Bind address for the server. Default is "127.0.0.1".
#' @param port Integer. Port to listen on. Default is 8080.
#' @param quiet Logical. If TRUE, suppresses the startup notification.
#' @param static List. Static directory configurations. Each entry has `dir`
#'   and `prefix`, and may set `list_dirs = TRUE` to render a browsable listing
#'   when no index file exists. For listing-enabled mounts, `index` optionally
#'   sets the index filenames checked before showing the directory listing.
#' @param custom Logical. If TRUE, disables automatic injection of framework JS/CSS and index handler.
#' @export
run_app <- function(
  app,
  host = "127.0.0.1",
  port = 8080L,
  quiet = FALSE,
  static = NULL,
  custom = FALSE
) {
  if (!inherits(app, "tinyvanr_app")) {
    stop("The 'app' argument must be of class 'tinyvanr_app'.", call. = FALSE)
  }

  sessions <- new.env(parent = emptyenv())
  handlers <- .tinyvanr_handlers(app, sessions, static, custom)

  if (!quiet) {
    message(sprintf("Running tinyVanR at http://%s:%d", host, port))
  }

  # Start the server using nanonext
  server <- http_server(
    url = paste0("http://", host, ":", port),
    handlers = handlers
  )

  server$serve()
}
