#' Create a Virtual UI Element
#'
#' Constructs an HTML tag representation that can be rendered to static HTML
#' via \code{as_html} or serialized for dynamic Van.js rendering.
#'
#' @param tag Character. The HTML tag name (e.g., "div", "input").
#' @param ... Unnamed arguments are treated as children; named arguments are attributes.
#' @param id Character or NULL. Optional ID used for reactive server-side binding.
#'
#' @return An object of class \code{tiny_ui}.
#' @export
vtag <- function(tag, ..., id = NULL) {
  dots <- list(...)
  nms <- names(dots)

  # Split dots into props (named) and children (unnamed)
  if (is.null(nms)) {
    props <- list()
    children <- dots
  } else {
    props <- dots[nms != ""]
    # Filter out NULL properties (common when using dedicated tag helpers)
    props <- props[!vapply(props, is.null, logical(1))]
    children <- unname(dots[nms == ""])
  }

  structure(
    list(tag = tag, id = id, props = props, children = children),
    class = "tiny_ui"
  )
}

#' Convert tiny_ui Objects to HTML Strings
#'
#' Recursively renders a \code{tiny_ui} tree or a list of elements into a valid HTML character string.
#'
#' @param x A \code{tiny_ui} object, a list of elements, or a character string.
#'
#' @return A character string containing the HTML representation.
#' @export
as_html <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (is.character(x)) {
    return(x)
  }
  if (is.list(x) && !inherits(x, "tiny_ui")) {
    return(paste(vapply(x, as_html, character(1)), collapse = ""))
  }
  if (!inherits(x, "tiny_ui")) {
    return(as.character(x))
  } # Fallback for non-vtag objects

  id_attr <- if (!is.null(x$id)) sprintf(' id="%s"', x$id) else ""
  props_attrs <- ""
  if (length(x$props) > 0) {
    props_attrs <- paste(
      names(x$props),
      vapply(x$props, function(p) sprintf('"%s"', p), character(1)),
      sep = "=",
      collapse = " "
    )
    props_attrs <- paste0(" ", props_attrs)
  }

  inner_html <- paste(vapply(x$children, as_html, character(1)), collapse = "")
  sprintf("<%s%s%s>%s</%s>", x$tag, id_attr, props_attrs, inner_html, x$tag)
}

#' Serialize R Objects to JavaScript Literals
#'
#' Converts R lists and vectors into a string format compatible with JavaScript object literals.
#' This is used to transmit the initial UI structure over WebSockets.
#'
#' @param x The R object to serialize.
#'
#' @return A character string representing the JS object.
#' @keywords internal
serialize_ui <- function(x) {
  if (is.null(x)) {
    return("null")
  }
  if (is.logical(x)) {
    return(tolower(as.character(x)))
  }
  if (is.numeric(x)) {
    return(as.character(x))
  }
  if (is.character(x)) {
    return(paste0("'", gsub("'", "\\'", x, fixed = TRUE), "'"))
  }
  if (is.list(x) && !is.null(names(x))) {
    kv <- vapply(
      names(x),
      function(n) paste0(n, ":", serialize_ui(x[[n]])),
      character(1)
    )
    return(paste0("{", paste(kv, collapse = ","), "}"))
  }
  if (is.list(x)) {
    return(paste0(
      "[",
      paste(vapply(x, serialize_ui, character(1)), collapse = ","),
      "]"
    ))
  }
  return(paste0("'", as.character(x), "'"))
}

#' Constructor for a tinyVanR Application
#'
#' Bundles the frontend definition and backend logic into a single app object.
#'
#' @param frontend A \code{tiny_ui} object defining the initial layout.
#' @param backend A function(input, output, on) containing the reactive server logic.
#' @param title Character. The title displayed in the browser tab.
#' @param head List with \code{before} and \code{after} slots for the HTML head section.
#' @param body List with \code{before} and \code{after} slots for the HTML body section.
#'
#' @return An object of class \code{tinyvanr_app}.
#' @export
tinyvanr_app <- function(
  frontend,
  backend,
  title = "tinyVanR",
  head = list(before = list(), after = list()),
  body = list(before = list(), after = list())
) {
  # Normalize head inputs
  if (!is.list(head)) {
    head <- list()
  }
  if (is.null(head$before)) {
    head$before <- list()
  }
  if (is.null(head$after)) {
    head$after <- list()
  }
  if (!is.list(head$before)) {
    head$before <- list(head$before)
  }
  if (!is.list(head$after)) {
    head$after <- list(head$after)
  }

  # Normalize body inputs
  if (!is.list(body)) {
    body <- list()
  }
  if (is.null(body$before)) {
    body$before <- list()
  }
  if (is.null(body$after)) {
    body$after <- list()
  }
  if (!is.list(body$before)) {
    body$before <- list(body$before)
  }
  if (!is.list(body$after)) {
    body$after <- list(body$after)
  }

  structure(
    list(
      frontend = frontend,
      backend = backend,
      title = title,
      head = head,
      body = body
    ),
    class = "tinyvanr_app"
  )
}

#' Initialize a New Client Session
#'
#' Creates an isolated environment for a unique WebSocket connection, providing the input/output API.
#'
#' @param client_id Character. The unique connection ID from the web server.
#' @param app_frontend The \code{tiny_ui} tree for initial rendering.
#' @param app_backend The backend function provided in \code{tinyvanr_app}.
#'
#' @return An environment containing session state and reactive handlers.
#' @keywords internal
create_session <- function(client_id, app_frontend, app_backend) {
  sess <- new.env(parent = emptyenv())
  sess$id <- client_id
  sess$input_state <- new.env(parent = emptyenv())
  sess$handlers <- new.env(parent = emptyenv())

  # Input API
  input_api <- list(get = function(id) sess$input_state[[id]])

  # Output API
  output_api <- list(
    set = function(id, value, type = "text") {
      msg <- paste("OUT", id, type, as.character(value), sep = "\t")
      civetwebR::ws_send(as.integer(sess$id), msg)
    },
    # Helper to set element properties (e.g. disabled, style, class)
    set_prop = function(id, prop, value) {
      msg <- paste(
        "OUT",
        id,
        paste0("prop:", prop),
        as.character(value),
        sep = "\t"
      )
      civetwebR::ws_send(as.integer(sess$id), msg)
    }
  )

  # Event Registration API
  on_api <- function(id, handler) {
    sess$handlers[[id]] <- handler
  }

  # Send the initial UI Layout
  ui_msg <- paste("UI", serialize_ui(app_frontend), sep = "\t")
  civetwebR::ws_send(as.integer(sess$id), ui_msg)

  # Run user server code to register handlers and perform initial updates
  app_backend(input_api, output_api, on_api)

  sess
}

#' Route and Process Incoming WebSocket Messages
#'
#' Parses the tab-separated protocol messages and executes corresponding backend handlers.
#'
#' @param session The session environment created by \code{create_session}.
#' @param raw_msg Raw vector. The raw message bytes from the WebSocket.
#'
#' @keywords internal
dispatch_ws_message <- function(session, raw_msg) {
  msg_str <- rawToChar(raw_msg)
  parts <- strsplit(msg_str, "\t")[[1]]

  if (length(parts) < 4 || parts[1] != "IN") {
    return()
  }

  id <- parts[2]
  type <- parts[3]
  value_raw <- parts[4]

  # Basic Coercion
  parsed_val <- switch(
    type,
    "num" = as.numeric(value_raw),
    "bool" = as.logical(toupper(value_raw)),
    "click" = 1, # Counter or trigger
    value_raw
  )

  # Update session state
  session$input_state[[id]] <- parsed_val

  # Execute registered handler
  handler <- session$handlers[[id]]
  if (is.function(handler)) {
    tryCatch(
      {
        handler(parsed_val)
      },
      error = function(e) {
        err_msg <- paste("ERR", "Handler Error", "text", e$message, sep = "\t")
        civetwebR::ws_send(as.integer(session$id), err_msg)
      }
    )
  }
}
