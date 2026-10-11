static_test_listing_dir <- function() {
  assets <- system.file("tinyVanR", package = "tinyVanR")
  if (
    !nzchar(assets) || !file.exists(file.path(assets, "directory-listing.html"))
  ) {
    assets <- testthat::test_path("..", "..", "inst", "tinyVanR")
  }
  normalizePath(assets, winslash = "/", mustWork = TRUE)
}

static_test_response <- function(
  uri,
  root,
  prefix = "/browse",
  index = "index.html",
  listing_dir = static_test_listing_dir()
) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  root_prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  normalized_prefix <- paste0("/", sub("^/+|/+$", "", prefix))
  if (normalized_prefix == "/") {
    normalized_prefix <- ""
  }

  .static_directory_response(
    req = list(uri = uri),
    root = root,
    root_prefix = root_prefix,
    prefix = normalized_prefix,
    index = index,
    listing_dir = listing_dir
  )
}

test_that("listing renders escaped rows, breadcrumbs, and sorting assets", {
  root <- tempfile("tinyvanr-static-listing-")
  dir.create(file.path(root, "nested"), recursive = TRUE)
  writeLines("file contents", file.path(root, "hello.txt"))
  writeLines("nested contents", file.path(root, "nested", "inside.txt"))
  writeLines("hidden", file.path(root, ".hidden"))
  writeBin(
    charToRaw("spaces are supported"),
    file.path(root, "hello world.txt")
  )
  writeBin(charToRaw("special name"), file.path(root, "research & notes.txt"))

  response <- static_test_response("/browse/", root = root)

  expect_equal(response$status, 200L)
  expect_equal(response$headers[["Content-Type"]], "text/html; charset=utf-8")
  expect_match(response$body, "Index of")
  expect_match(response$body, 'href="/browse/hello.txt"')
  expect_match(response$body, 'href="/browse/nested/"')
  expect_match(response$body, 'href="/browse/hello%20world.txt"')
  expect_match(response$body, 'href="/browse/research%20%26%20notes.txt"')
  expect_match(response$body, "research &amp; notes.txt")
  expect_false(grepl(".hidden", response$body, fixed = TRUE))
  expect_match(response$body, "\\.listing-shell")
  expect_match(response$body, "sortEntries")
  expect_match(response$body, 'href="/browse/">/</a>')

  encoded_file <- static_test_response("/browse/hello%20world.txt", root = root)
  expect_equal(rawToChar(encoded_file$body), "spaces are supported")

  nested <- static_test_response("/browse/nested/", root = root)
  expect_match(nested$body, 'href="/browse/nested/inside.txt"')
  expect_match(nested$body, 'href="/browse/">/</a>')
  expect_match(nested$body, 'href="/browse/nested/">nested</a>')
})

test_that("directory index files are served before generated listings", {
  root <- tempfile("tinyvanr-static-index-")
  dir.create(root)
  writeBin(charToRaw("default index"), file.path(root, "index.html"))
  writeBin(charToRaw("custom index"), file.path(root, "home.html"))

  default_response <- static_test_response("/browse/", root = root)
  expect_equal(default_response$status, 200L)
  expect_equal(rawToChar(default_response$body), "default index")
  expect_equal(
    default_response$headers[["Content-Type"]],
    "text/html; charset=utf-8"
  )

  custom_response <- static_test_response(
    "/browse/",
    root = root,
    index = c("missing.html", "home.html")
  )
  expect_equal(rawToChar(custom_response$body), "custom index")
  expect_false(grepl(
    "listing-shell",
    rawToChar(custom_response$body),
    fixed = TRUE
  ))

  # A symlinked index that resolves outside the mount must not be served.
  outside_index <- tempfile(fileext = ".html")
  writeBin(charToRaw("outside index"), outside_index)
  link_path <- file.path(root, "linked-index.html")
  link_created <- suppressWarnings(file.symlink(outside_index, link_path))
  if (isTRUE(link_created)) {
    linked_response <- static_test_response(
      "/browse/",
      root = root,
      index = "linked-index.html"
    )
    expect_match(linked_response$body, "Index of")
    expect_false(grepl("outside index", linked_response$body, fixed = TRUE))
  }
})

test_that("registered static callback serves requests through its captured mount", {
  root <- tempfile("tinyvanr-static-callback-")
  dir.create(root)
  writeBin(charToRaw("callback bytes"), file.path(root, "file.txt"))
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  root_prefix <- if (endsWith(root, "/")) root else paste0(root, "/")

  callback <- .static_directory_callback(
    root = root,
    root_prefix = root_prefix,
    prefix = "/mounted",
    index = "index.html",
    listing_dir = static_test_listing_dir()
  )
  response <- callback(list(uri = "/mounted/file.txt"))

  expect_equal(response$status, 200L)
  expect_equal(rawToChar(response$body), "callback bytes")
})

test_that("file requests return exact bytes, MIME types, and ignore query strings", {
  root <- tempfile("tinyvanr-static-files-")
  dir.create(root)
  writeBin(as.raw(c(0x00, 0x01, 0x7f, 0xff)), file.path(root, "binary.png"))

  response <- static_test_response("/browse/binary.png?cache=123", root = root)

  expect_equal(response$status, 200L)
  expect_identical(response$body, as.raw(c(0x00, 0x01, 0x7f, 0xff)))
  expect_equal(response$headers[["Content-Type"]], "image/png")
})

test_that("directories without an index redirect to a trailing slash", {
  root <- tempfile("tinyvanr-static-redirect-")
  dir.create(file.path(root, "nested"), recursive = TRUE)

  response <- static_test_response("/browse/nested", root = root)

  expect_equal(response$status, 301L)
  expect_equal(response$headers[["Location"]], "/browse/nested/")
})

test_that("missing resources and requests outside the mount are rejected", {
  root <- tempfile("tinyvanr-static-missing-")
  dir.create(root)

  expect_equal(
    static_test_response("/browse/no-such-file", root = root)$status,
    404L
  )
  expect_equal(
    static_test_response("/browse-other/file.txt", root = root)$status,
    404L
  )
  expect_equal(static_test_response("/browse/%00bad", root = root)$status, 404L)
  expect_equal(static_test_response(NULL, root = root)$status, 404L)
  expect_equal(static_test_response(NA_character_, root = root)$status, 404L)
  expect_equal(static_test_response(character(), root = root)$status, 404L)
  expect_equal(
    static_test_response(NULL, root = root, prefix = "")$status,
    200L
  )

  writeBin(charToRaw("relative path"), file.path(root, "relative.txt"))
  expect_equal(
    rawToChar(static_test_response("browse/relative.txt", root = root)$body),
    "relative path"
  )
})

test_that("path traversal is blocked, including URL-encoded traversal", {
  root <- tempfile("tinyvanr-static-root-")
  dir.create(root)
  outside <- paste0(root, "-outside.txt")
  writeLines("outside secret", outside)

  expect_equal(static_test_response("/browse/../", root = root)$status, 403L)
  expect_equal(
    static_test_response("/browse/%2e%2e/", root = root)$status,
    403L
  )
  expect_equal(
    static_test_response("/../", root = root, prefix = "")$status,
    403L
  )

  writeBin(charToRaw("inside root"), file.path(root, "safe.txt"))
  root_mount_response <- static_test_response(
    "/safe.txt",
    root = root,
    prefix = ""
  )
  expect_equal(rawToChar(root_mount_response$body), "inside root")
})

test_that("directory listings HTML-escape names and encode URL path segments", {
  expect_equal(
    .static_html_escape("<a & 'b'>\""),
    "&lt;a &amp; &#39;b&#39;&gt;&quot;"
  )
  expect_equal(
    .static_breadcrumb("/browse/a b/", "/browse"),
    paste0(
      '<a href="/browse/">/</a> / <a href="/browse/a%20b/">a b</a>'
    )
  )
  expect_equal(.static_breadcrumb("/", ""), '<a href="/">/</a>')
  expect_equal(
    .static_breadcrumb("/nested/deep/", ""),
    paste0(
      '<a href="/">/</a> / <a href="/nested/">nested</a>',
      ' / <a href="/nested/deep/">deep</a>'
    )
  )
})

test_that("listing falls back to plain HTML when package assets are unavailable", {
  root <- tempfile("tinyvanr-static-fallback-")
  dir.create(root)
  writeLines("listed", file.path(root, "plain.txt"))

  response <- static_test_response(
    "/browse/",
    root = root,
    listing_dir = ""
  )

  expect_equal(response$status, 200L)
  expect_match(response$body, "Index of")
  expect_match(response$body, 'href="/browse/plain.txt"')
  expect_false(grepl("directory-listing.css", response$body, fixed = TRUE))
})

test_that("empty listings and templates with missing optional assets render safely", {
  root <- tempfile("tinyvanr-static-empty-")
  dir.create(root)

  empty_response <- static_test_response("/browse/", root = root)
  expect_equal(empty_response$status, 200L)
  expect_match(empty_response$body, "Index of")
  expect_false(grepl('class="entry"', empty_response$body, fixed = TRUE))

  listing_dir <- tempfile("tinyvanr-listing-template-")
  dir.create(listing_dir)
  writeLines(
    "<html><style>{{styles}}</style><main>{{breadcrumb}}{{files}}</main><script>{{script}}</script></html>",
    file.path(listing_dir, "directory-listing.html")
  )
  writeLines("templated", file.path(root, "templated.txt"))
  response <- static_test_response(
    "/browse/",
    root = root,
    listing_dir = listing_dir
  )
  expect_match(response$body, "templated.txt")
  expect_false(grepl("{{styles}}", response$body, fixed = TRUE))
  expect_false(grepl("{{script}}", response$body, fixed = TRUE))
  expect_match(response$body, "<style></style>")
  expect_match(response$body, "<script></script>")
})

test_that("static metadata helpers map content types and format sizes", {
  paths <- c(
    "page.HTML",
    "app.js",
    "data.json",
    "style.css",
    "readme.txt",
    "table.csv",
    "page.htm",
    "vector.svg",
    "plot.PNG",
    "photo.jpeg",
    "photo.jpg",
    "animation.gif",
    "image.webp",
    "favicon.ico",
    "report.pdf",
    "module.wasm",
    "unknown.xyz",
    "extensionless"
  )
  expected_types <- c(
    "text/html; charset=utf-8",
    "application/javascript",
    "application/json",
    "text/css",
    "text/plain; charset=utf-8",
    "text/csv; charset=utf-8",
    "text/html; charset=utf-8",
    "image/svg+xml",
    "image/png",
    "image/jpeg",
    "image/jpeg",
    "image/gif",
    "image/webp",
    "image/x-icon",
    "application/pdf",
    "application/wasm",
    "application/octet-stream",
    "application/octet-stream"
  )
  expect_equal(
    unname(vapply(paths, .static_mime_type, character(1))),
    expected_types
  )

  expect_equal(.static_format_size(0), "0 B")
  expect_equal(.static_format_size(1023), "1023 B")
  expect_equal(.static_format_size(1024), "1.0 KB")
  expect_equal(.static_format_size(1024^2), "1.0 MB")
})

test_that("static handler validates its configuration", {
  listing_dir <- static_test_listing_dir()
  expect_error(
    .static_directory_handler(
      NA_character_,
      "/files",
      "index.html",
      listing_dir
    ),
    "dir"
  )
  expect_error(
    .static_directory_handler(
      "missing-directory",
      "/files",
      "index.html",
      listing_dir
    ),
    "does not exist"
  )
  expect_error(
    .static_directory_handler(
      tempdir(),
      NA_character_,
      "index.html",
      listing_dir
    ),
    "prefix"
  )
  expect_error(
    .static_directory_handler(
      tempdir(),
      character(),
      "index.html",
      listing_dir
    ),
    "prefix"
  )
  expect_error(
    .static_directory_handler(tempdir(), 1, "index.html", listing_dir),
    "prefix"
  )
  expect_error(
    .static_directory_handler(tempdir(), "/files", character(), listing_dir),
    "index"
  )
  expect_error(
    .static_directory_handler(tempdir(), "/files", 1, listing_dir),
    "index"
  )
  expect_error(
    .static_directory_handler(tempdir(), "/files", NA_character_, listing_dir),
    "index"
  )
  expect_error(
    .static_directory_handler(
      tempdir(),
      "/files",
      "../outside.html",
      listing_dir
    ),
    "simple file names"
  )
  expect_error(
    .static_directory_handler(tempdir(), "/files", "", listing_dir),
    "simple file names"
  )
  expect_error(
    .static_directory_handler(
      tempdir(),
      "/files",
      "sub/index.html",
      listing_dir
    ),
    "simple file names"
  )
  expect_error(
    .static_directory_handler(tempdir(), "/files", ".", listing_dir),
    "simple file names"
  )

  # Root prefixes are normalized to the root route and should construct cleanly.
  root_handler <- .static_directory_handler(
    tempdir(),
    "/",
    "index.html",
    listing_dir
  )
  expect_false(is.null(root_handler))
})

test_that("static mount configuration selects the native or listing handler", {
  listing_dir <- static_test_listing_dir()
  expect_null(.static_config_handler(list(prefix = "/files"), listing_dir))

  native_handler <- .static_config_handler(
    list(dir = tempdir(), prefix = "/native"),
    listing_dir
  )
  expect_false(is.null(native_handler))

  listing_handler <- .static_config_handler(
    list(dir = tempdir(), prefix = "/listing", list_dirs = TRUE),
    listing_dir
  )
  expect_false(is.null(listing_handler))

  expect_error(
    .static_config_handler(
      list(dir = tempdir(), prefix = "/bad", list_dirs = "yes"),
      listing_dir
    ),
    "list_dirs"
  )
})
