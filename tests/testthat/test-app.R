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
  expect_s3_class(app$body$before[[1]], "tiny_ui")
})

test_that("tinyvanr_app fails on wrong class", {
  expect_error(run_app(list()), "must be of class 'tinyvanr_app'")
})

test_that("create_session sends initial UI", {
  # Mock ws_send to capture the message sent
  sent_msg <- NULL
  mockery::stub(create_session, "civetwebR::ws_send", function(id, msg) {
    sent_msg <<- msg
  })

  ui <- van$div(id = "test", "Hello")
  backend <- function(input, output, on) {}

  sess <- create_session("123", ui, backend)

  expect_match(sent_msg, "^UI\t")
  expect_match(sent_msg, "'tag':'div'")
})
