test_that("dispatch_ws_message coerces types correctly", {
  # Mock session environment
  sess <- new.env()
  sess$input_state <- new.env()
  sess$handlers <- new.env()

  # Mock ws$send for error handling
  sess$ws <- list(
    send = function(msg) NULL
  )

  # Test numeric coercion
  dispatch_ws_message(sess, "IN\tmyslider\tnum\t42.5")
  expect_equal(sess$input_state$myslider, 42.5)

  # Test boolean coercion
  dispatch_ws_message(sess, "IN\tmycheck\tbool\tTRUE")
  expect_true(sess$input_state$mycheck)

  # Test click trigger
  dispatch_ws_message(sess, "IN\tmybtn\tclick\t1")
  expect_equal(sess$input_state$mybtn, 1)
})

test_that("dispatch_ws_message handles malformed input", {
  sess <- new.env()
  # Missing parts or wrong prefix should return early without error
  expect_silent(dispatch_ws_message(sess, "OUT\tid\ttype\tval"))
  expect_silent(dispatch_ws_message(sess, "IN\tshort"))
  expect_silent(dispatch_ws_message(sess, ""))
})

test_that("dispatch_ws_message triggers handlers", {
  sess <- new.env()
  sess$input_state <- new.env()
  sess$handlers <- new.env()

  handler_called <- FALSE
  val_received <- NULL

  sess$handlers$btn <- function(val) {
    handler_called <<- TRUE
    val_received <<- val
  }

  dispatch_ws_message(sess, "IN\tbtn\tclick\t1")
  expect_true(handler_called)
  expect_equal(val_received, 1)
})

test_that("dispatch_ws_message handles errors gracefully", {
  # We need to mock ws$send to prevent actual socket calls
  # in a test environment
  sess <- new.env()
  sess$id <- "1"
  sess$input_state <- new.env()
  sess$handlers <- new.env()
  sess$ws <- list(
    send = function(msg) NULL
  )

  sess$handlers$err_btn <- function(val) stop("Boom!")

  # Should not crash the R session
  expect_silent(dispatch_ws_message(sess, "IN\terr_btn\tclick\t1"))
})

test_that("output API sends correct format", {
  sent_msg <- NULL
  mock_ws <- list(
    send = function(msg) {
      sent_msg <<- msg
    }
  )
  mock_ws <- new.env()
  mock_ws$send <- function(msg) {
    sent_msg <<- msg
  }

  sess <- create_session(
    "1",
    vtag("div"),
    function(i, o, n) {
      o$set("txt_id", "hello")
      expect_equal(sent_msg, "OUT\ttxt_id\ttext\thello")

      o$set_prop("btn_id", "disabled", TRUE)
      expect_equal(sent_msg, "OUT\tbtn_id\tprop:disabled\tTRUE")
    },
    mock_ws
  )
})
