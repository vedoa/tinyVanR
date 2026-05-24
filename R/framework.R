#' UI DSL Helpers
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
    children <- unname(dots[nms == ""])
  }

  structure(
    list(tag = tag, id = id, props = props, children = children),
    class = "tiny_ui"
  )
}

#' Convert tiny_ui to static HTML string
#' @export
as_html <- function(x) {
  if (is.null(x)) {
    return("")
  }
  if (is.character(x)) {
    return(x)
  }
  if (!inherits(x, "tiny_ui")) {
    return(as.character(x))
  } # Fallback for non-vtag objects

  id_attr <- if (!is.null(x$id)) sprintf(' id="%s"', x$id) else ""
  props_attrs <- ""
  if (length(x$props) > 0) {
    props_attrs <- paste(
      names(x$props),
      sapply(x$props, function(p) sprintf('"%s"', p)),
      sep = "=",
      collapse = " "
    )
    props_attrs <- paste0(" ", props_attrs)
  }

  inner_html <- paste(sapply(x$children, as_html), collapse = "")
  sprintf("<%s%s%s>%s</%s>", x$tag, id_attr, props_attrs, inner_html, x$tag)
}

#' Recursive Base R to JS Object Serializer (Zero Dependency)
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
    kv <- sapply(names(x), function(n) paste0(n, ":", serialize_ui(x[[n]])))
    return(paste0("{", paste(kv, collapse = ","), "}"))
  }
  if (is.list(x)) {
    return(paste0("[", paste(sapply(x, serialize_ui), collapse = ","), "]"))
  }
  return(paste0("'", as.character(x), "'"))
}

#' Constructor for tinyVanR Application
#' @export
tiny_app <- function(
  ui,
  server_func,
  title = "tinyVanR",
  css = list(),
  head = list()
) {
  # Ensure css and head are always lists of vtag objects or strings
  if (!is.list(css)) {
    css <- list(css)
  }
  if (!is.list(head)) {
    head <- list(head)
  }

  structure(
    list(
      ui = ui,
      server = server_func,
      title = title,
      css = css,
      head = head
    ),
    class = "tiny_app"
  )
}

#' Initialize a new session environment
create_session <- function(client_id, app_ui, app_server) {
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
  ui_msg <- paste("UI", serialize_ui(app_ui), sep = "\t")
  civetwebR::ws_send(as.integer(sess$id), ui_msg)

  # Run user server code to register handlers and perform initial updates
  app_server(input_api, output_api, on_api)

  sess
}

#' Route incoming WebSocket text messages
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
