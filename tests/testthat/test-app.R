test_that("tinyvanr_app normalizes head and body inputs", {
  # Test with single tags instead of lists
  app <- tinyvanr_app(
    frontend = vtag("div"),
    backend = function(i, o, n) {},
    head = vtag("meta", charset = "UTF-8"),
    body = list(before = vtag("header"))
  )

  expect_s3_class(app, "tinyvanr_app")

  # Head should be normalized to a list with before/after slots
  expect_true(is.list(app$head$before))
  expect_true(is.list(app$head$after))

  # Body should be normalized
  expect_true(is.list(app$body$before))
  expect_s3_class(app$body$before[[1]], "tinyvanr_ui")
})

test_that("tinyvanr_app fills omitted slots and wraps scalar head and body values", {
  app <- tinyvanr_app(
    frontend = vtag("main"),
    backend = function(input, output, on) {},
    head = list(after = vtag("meta", charset = "UTF-8")),
    body = "plain body text"
  )

  expect_length(app$head$before, 0L)
  expect_length(app$head$after, 1L)
  expect_s3_class(app$head$after[[1]], "tinyvanr_ui")
  expect_length(app$body$before, 1L)
  expect_equal(app$body$before[[1]], "plain body text")
  expect_length(app$body$after, 0L)
})

test_that("tinyvanr_app wraps scalar before/after slots into lists", {
  app <- tinyvanr_app(
    frontend = vtag("main"),
    backend = function(input, output, on) {},
    head = list(before = "head before", after = "head after"),
    body = list(before = vtag("header"), after = vtag("footer"))
  )

  expect_equal(app$head$before, list("head before"))
  expect_equal(app$head$after, list("head after"))
  expect_s3_class(app$body$before[[1]], "tinyvanr_ui")
  expect_s3_class(app$body$after[[1]], "tinyvanr_ui")
})

test_that("tinyvanr_app replaces explicit NULL slots with empty lists", {
  app <- tinyvanr_app(
    frontend = vtag("main"),
    backend = function(input, output, on) {},
    head = list(before = vtag("meta"), after = NULL),
    body = list(before = NULL, after = vtag("footer"))
  )

  expect_s3_class(app$head$before[[1]], "tinyvanr_ui")
  expect_length(app$head$after, 0L)
  expect_length(app$body$before, 0L)
  expect_s3_class(app$body$after[[1]], "tinyvanr_ui")
})

test_that("tinyvanr_app fails on wrong class", {
  expect_error(run_app(list()), "must be of class 'tinyvanr_app'")
})

test_that("default index response includes framework assets and app sections", {
  app <- tinyvanr_app(
    frontend = van$main(van$h1("App UI")),
    backend = function(input, output, on) {},
    title = "Route Test",
    head = list(after = list(van$meta(name = "description", content = "test"))),
    body = list(
      before = list(van$header("Header")),
      after = list(van$footer("Footer"))
    )
  )

  response <- .tinyvanr_index_response(app)

  expect_equal(response$status, 200L)
  expect_equal(response$headers[["Content-Type"]], "text/html; charset=utf-8")
  expect_match(response$body, "<!DOCTYPE html><html")
  expect_match(response$body, "Route Test")
  expect_match(response$body, "/tinyvanr/van-1.6.0.min.js")
  expect_match(response$body, "/tinyvanr/client.js")
  expect_match(response$body, 'id="root"')
  expect_match(response$body, "description")
  expect_match(response$body, "Header")
  expect_match(response$body, "Footer")
})

test_that("asset directory resolver finds the vendored browser files", {
  asset_dir <- .tinyvanr_asset_dir()
  expect_true(dir.exists(asset_dir))
  expect_true(file.exists(file.path(asset_dir, "client.js")))
  expect_true(file.exists(file.path(asset_dir, "van-1.6.0.min.js")))
})

test_that("handler assembly includes built-in routes unless custom mode is set", {
  app <- tinyvanr_app(van$div("UI"), function(input, output, on) {})
  sessions <- new.env(parent = emptyenv())
  user_static <- list(list(dir = tempdir(), prefix = "/user-files"))

  default_handlers <- .tinyvanr_handlers(
    app,
    sessions,
    user_static,
    custom = FALSE
  )
  custom_handlers <- .tinyvanr_handlers(
    app,
    sessions,
    user_static,
    custom = TRUE
  )
  custom_without_static <- .tinyvanr_handlers(
    app,
    sessions,
    NULL,
    custom = TRUE
  )

  # Default mode: user files + framework files + WebSocket + root route.
  expect_length(default_handlers, 4L)
  # Custom mode keeps caller routes and WebSocket but omits framework/root routes.
  expect_length(custom_handlers, 2L)
  expect_length(custom_without_static, 1L)
})

test_that("WebSocket callbacks create, dispatch to, and close per-client sessions", {
  sessions <- new.env(parent = emptyenv())
  received <- NULL
  app <- tinyvanr_app(
    frontend = van$div("Session UI"),
    backend = function(input, output, on) {
      on("field", function(value) {
        received <<- value
      })
    }
  )
  callbacks <- .tinyvanr_ws_callbacks(app, sessions)

  sent <- character()
  ws <- new.env(parent = emptyenv())
  ws$id <- 42L
  ws$send <- function(message) {
    sent <<- c(sent, message)
  }

  expect_message(callbacks$on_open(ws, list(uri = "/ws")), "Session opened: 42")
  expect_true(exists("42", envir = sessions, inherits = FALSE))
  expect_match(sent[[1]], "^UI\\t")

  callbacks$on_message(ws, charToRaw("IN\tfield\tstr\tfrom-raw"))
  expect_equal(received, "from-raw")
  callbacks$on_message(ws, "IN\tfield\tstr\tfrom-character")
  expect_equal(received, "from-character")

  expect_message(callbacks$on_close(ws), "Session closed: 42")
  expect_false(exists("42", envir = sessions, inherits = FALSE))
  expect_silent(callbacks$on_close(ws))
  callbacks$on_message(ws, "IN\tfield\tstr\tignored")
  expect_equal(received, "from-character")
})

test_that("run_app configures nanonext and returns the server result", {
  app <- tinyvanr_app(van$div("UI"), function(input, output, on) {})
  server_args <- NULL
  mock_server <- list(serve = function() "server-result")
  mockery::stub(run_app, "http_server", function(url, handlers) {
    server_args <<- list(url = url, handlers = handlers)
    mock_server
  })

  result <- run_app(
    app,
    host = "127.0.0.1",
    port = 9123L,
    quiet = TRUE,
    custom = TRUE,
    static = list(list(dir = tempdir(), prefix = "/files"))
  )

  expect_equal(result, "server-result")
  expect_equal(server_args$url, "http://127.0.0.1:9123")
  expect_length(server_args$handlers, 2L)
})

test_that("run_app reports its address when not quiet", {
  app <- tinyvanr_app(van$div("UI"), function(input, output, on) {})
  mockery::stub(run_app, "http_server", function(url, handlers) {
    list(serve = function() invisible(NULL))
  })

  expect_message(
    run_app(app, host = "localhost", port = 9001L, custom = TRUE),
    "Running tinyVanR at http://localhost:9001"
  )
})

test_that("run_app uses the built-in page and packaged static handler by default", {
  app <- tinyvanr_app(van$div("UI"), function(input, output, on) {})
  server_args <- NULL
  mockery::stub(run_app, "http_server", function(url, handlers) {
    server_args <<- list(url = url, handlers = handlers)
    list(serve = function() invisible(NULL))
  })

  expect_silent(run_app(app, port = 8124L, quiet = TRUE))

  expect_equal(server_args$url, "http://127.0.0.1:8124")
  # Framework static assets, WebSocket route, then the HTML root route.
  expect_length(server_args$handlers, 3L)
})

test_that("create_session sends initial UI", {
  # Mock ws$send to capture the message sent
  sent_msg <- NULL
  mock_ws <- new.env()
  mock_ws$send <- function(msg) {
    sent_msg <<- msg
  }

  ui <- van$div(id = "test", "Hello")
  backend <- function(input, output, on) {}

  sess <- create_session("123", ui, backend, mock_ws)

  expect_match(sent_msg, "^UI\t")
  expect_match(sent_msg, "'tag':'div'")
})
