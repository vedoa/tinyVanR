#' Constructor for tinyVanR Application
#' @export
tiny_app <- function(ui_file = "index.html", server_func) {
  structure(list(ui = ui_file, server = server_func), class = "tiny_app")
}

#' Initialize a new session environment
create_session <- function(client_id, app_server) {
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
    }
  )

  # Event Registration API
  on_api <- function(id, handler) { sess$handlers[[id]] <- handler }

  # Run user server code once to register handlers
  app_server(input_api, output_api, on_api)
  
  sess
}

#' Route incoming WebSocket text messages
dispatch_ws_message <- function(session, raw_msg) {
  msg_str <- rawToChar(raw_msg)
  parts <- strsplit(msg_str, "\t")[[1]]
  
  if (length(parts) < 4 || parts[1] != "IN") return()

  id <- parts[2]
  type <- parts[3]
  value_raw <- parts[4]

  # Basic Coercion
  parsed_val <- switch(type,
    "num"   = as.numeric(value_raw),
    "bool"  = as.logical(toupper(value_raw)),
    "click" = 1, # Counter or trigger
    value_raw
  )

  # Update session state
  session$input_state[[id]] <- parsed_val

  # Execute registered handler
  handler <- session$handlers[[id]]
  if (is.function(handler)) {
    tryCatch({
      handler(parsed_val)
    }, error = function(e) {
      err_msg <- paste("ERR", "Handler Error", "text", e$message, sep = "\t")
      civetwebR::ws_send(as.integer(session$id), err_msg)
    })
  }
}