test_that("vtag creates correct structure", {
  t <- vtag("div", class = "container", van$p("hello"), id = "test-id")

  expect_s3_class(t, "tiny_ui")
  expect_equal(t$tag, "div")
  expect_equal(t$id, "test-id")
  expect_equal(t$props$class, "container")
  expect_length(t$children, 1)
})

test_that("vtag filters NULL properties", {
  t <- vtag("input", value = NULL, type = "text")
  expect_null(t$props$value)
  expect_equal(t$props$type, "text")
})

test_that("vtag handles deeply nested children", {
  t <- van$div(van$span(van$b("inner")))
  html <- as_html(t)
  expect_equal(html, "<div><span><b>inner</b></span></div>")
})

test_that("as_html renders basic tags", {
  t <- vtag("span", "content", class = "bold")
  html <- as_html(t)
  expect_equal(html, '<span class="bold">content</span>')
})

test_that("as_html fallbacks to as.character for non-ui objects", {
  expect_equal(as_html(123), "123")
  expect_equal(as_html(TRUE), "TRUE")
})

test_that("as_html handles nested lists, nulls, and scalars", {
  tags <- list(vtag("br"), NULL, vtag("hr"))
  html <- as_html(tags)
  expect_equal(html, "<br></br><hr></hr>")

  mixed <- list("text", vtag("b", "bold"))
  expect_equal(as_html(mixed), "text<b>bold</b>")
})

test_that("serialize_ui handles basic types", {
  expect_equal(serialize_ui(NULL), "null")
  expect_equal(serialize_ui(TRUE), "true")
  expect_equal(serialize_ui(42), "42")
  expect_equal(serialize_ui("hello"), "'hello'")
})

test_that("serialize_ui quotes keys for JS safety", {
  # This is critical for reserved words like 'class'
  input <- list(class = "btn", `for` = "input-1")
  js <- serialize_ui(input)
  expect_match(js, "'class':'btn'")
  expect_match(js, "'for':'input-1'")
})

test_that("serialize_ui handles factors and empty lists", {
  expect_equal(serialize_ui(factor("a")), "'a'")
  expect_equal(serialize_ui(list()), "[]")
  expect_equal(serialize_ui(setNames(list(), character(0))), "[]")
})

test_that("serialize_ui handles nested lists", {
  input <- list(
    tag = "div",
    children = list("text", list(tag = "br"))
  )
  js <- serialize_ui(input)
  expect_match(js, "\\['text',\\{'tag':'br'")
})
