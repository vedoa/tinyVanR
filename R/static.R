# Choose nanonext's regular file handler or tinyVanR's optional listing handler.
.static_config_handler <- function(config, listing_dir) {
  dir <- config$dir
  prefix <- config$prefix
  if (is.null(dir) || is.null(prefix)) {
    return(NULL)
  }

  list_dirs <- config$list_dirs
  if (
    !is.null(list_dirs) &&
      (!is.logical(list_dirs) || length(list_dirs) != 1L || is.na(list_dirs))
  ) {
    stop("static list_dirs must be TRUE or FALSE", call. = FALSE)
  }

  if (isTRUE(list_dirs)) {
    index <- if (is.null(config$index)) "index.html" else config$index
    return(.static_directory_handler(dir, prefix, index, listing_dir))
  }

  handler_directory(prefix, dir)
}

# Validate a listing-enabled mount and wrap its request responder as a route.
.static_directory_handler <- function(dir, prefix, index, listing_dir) {
  # Validate configuration once at startup instead of failing on each request.
  if (!is.character(dir) || length(dir) != 1L || is.na(dir)) {
    stop("static dir must be a single directory path", call. = FALSE)
  }
  if (!dir.exists(dir)) {
    stop(sprintf("Static directory does not exist: %s", dir), call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("static prefix must be a single URL path", call. = FALSE)
  }
  if (
    !is.character(index) ||
      length(index) < 1L ||
      anyNA(index) ||
      any(!nzchar(index)) ||
      any(index %in% c(".", "..")) ||
      any(grepl("[/\\\\]", index))
  ) {
    stop("static index must contain simple file names", call. = FALSE)
  }

  root <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  root_prefix <- if (endsWith(root, "/")) root else paste0(root, "/")
  prefix <- paste0("/", sub("^/+|/+$", "", prefix))
  if (prefix == "/") {
    prefix <- ""
  }
  route <- if (prefix == "") "/" else prefix

  handler(
    route,
    .static_directory_callback(root, root_prefix, prefix, index, listing_dir),
    method = "GET",
    prefix = TRUE
  )
}

# Capture validated mount settings in the function nanonext calls per request.
.static_directory_callback <- function(
  root,
  root_prefix,
  prefix,
  index,
  listing_dir
) {
  function(req) {
    .static_directory_response(
      req = req,
      root = root,
      root_prefix = root_prefix,
      prefix = prefix,
      index = index,
      listing_dir = listing_dir
    )
  }
}

# Resolve one mounted URL securely, then serve a file, index, or directory page.
.static_directory_response <- function(
  req,
  root,
  root_prefix,
  prefix,
  index,
  listing_dir
) {
  path <- req$uri
  if (is.null(path) || !length(path) || is.na(path[[1]])) {
    path <- "/"
  } else {
    path <- as.character(path[[1]])
  }
  path <- sub("\\?.*$", "", path)
  if (!startsWith(path, "/")) {
    path <- paste0("/", path)
  }

  # Prefix matching must end at a path boundary; /assets-other is not /assets.
  if (
    prefix != "" && path != prefix && !startsWith(path, paste0(prefix, "/"))
  ) {
    return(list(status = 404L, body = "Not Found"))
  }

  relative <- if (prefix == "") {
    sub("^/+", "", path)
  } else {
    sub("^/+", "", substring(path, nchar(prefix) + 1L))
  }
  if (grepl("%00", relative, ignore.case = TRUE)) {
    return(list(status = 404L, body = "Not Found"))
  }
  relative <- tryCatch(utils::URLdecode(relative), error = function(e) relative)
  relative <- gsub("\\\\", "/", relative)
  requested <- file.path(root, relative)

  # Canonicalize before checking containment so traversal and symlinks cannot
  # escape the configured directory, including when the requested file is absent.
  resolved <- tryCatch(
    normalizePath(requested, winslash = "/", mustWork = FALSE),
    error = function(e) ""
  )
  if (!nzchar(resolved)) {
    return(list(status = 404L, body = "Not Found"))
  }
  if (!(identical(resolved, root) || startsWith(resolved, root_prefix))) {
    return(list(status = 403L, body = "Forbidden"))
  }

  if (dir.exists(resolved)) {
    # Serve a configured index page before falling back to the generated listing.
    for (index_file in index) {
      candidate <- file.path(resolved, index_file)
      if (file.exists(candidate) && !dir.exists(candidate)) {
        candidate_resolved <- normalizePath(
          candidate,
          winslash = "/",
          mustWork = FALSE
        )
        if (
          identical(candidate_resolved, root) ||
            startsWith(candidate_resolved, root_prefix)
        ) {
          return(.static_file_response(candidate_resolved))
        }
      }
    }

    if (!endsWith(path, "/")) {
      return(list(
        status = 301L,
        headers = c(Location = paste0(path, "/")),
        body = ""
      ))
    }

    return(.static_listing_response(
      directory = resolved,
      request_path = path,
      prefix = prefix,
      listing_dir = listing_dir
    ))
  }

  if (!file.exists(resolved)) {
    return(list(status = 404L, body = "Not Found"))
  }
  .static_file_response(resolved)
}

# Read a file as bytes and pair it with the inferred response content type.
.static_file_response <- function(path) {
  info <- file.info(path)
  body <- readBin(path, what = "raw", n = info$size)
  list(
    status = 200L,
    headers = c("Content-Type" = .static_mime_type(path)),
    body = body
  )
}

# Map common extensions to browser MIME types; unknown types are binary.
.static_mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    html = "text/html; charset=utf-8",
    htm = "text/html; charset=utf-8",
    css = "text/css",
    js = "application/javascript",
    json = "application/json",
    txt = "text/plain; charset=utf-8",
    csv = "text/csv; charset=utf-8",
    svg = "image/svg+xml",
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    webp = "image/webp",
    ico = "image/x-icon",
    pdf = "application/pdf",
    wasm = "application/wasm",
    "application/octet-stream"
  )
}

# Render one directory's entries using the bundled sortable listing template.
.static_listing_response <- function(
  directory,
  request_path,
  prefix,
  listing_dir
) {
  # Omit dotfiles, matching R's default list.files() behavior.
  files <- list.files(directory, all.files = FALSE, no.. = TRUE)
  if (length(files)) {
    full_paths <- file.path(directory, files)
    info <- file.info(full_paths)
    keep <- !is.na(info$isdir) | file.exists(full_paths)
    files <- files[keep]
    full_paths <- full_paths[keep]
    info <- info[keep, , drop = FALSE]
  } else {
    full_paths <- character()
    info <- file.info(full_paths)
  }

  base_url <- if (endsWith(request_path, "/")) {
    request_path
  } else {
    paste0(request_path, "/")
  }
  rows <- character(length(files))
  for (i in seq_along(files)) {
    name <- files[[i]]
    is_dir <- isTRUE(info$isdir[[i]])
    display_name <- if (is_dir) paste0(name, "/") else name
    escaped_name <- .static_html_escape(display_name)
    href_name <- utils::URLencode(name, reserved = TRUE)
    href <- paste0(base_url, href_name, if (is_dir) "/" else "")
    size <- if (is_dir || is.na(info$size[[i]])) {
      ""
    } else {
      .static_format_size(info$size[[i]])
    }
    mtime <- if (is.na(info$mtime[[i]])) {
      ""
    } else {
      format(info$mtime[[i]], "%Y-%m-%d %H:%M")
    }
    size_raw <- if (is_dir || is.na(info$size[[i]])) 0 else info$size[[i]]
    mtime_raw <- if (is.na(info$mtime[[i]])) 0 else as.numeric(info$mtime[[i]])
    kind <- if (is_dir) "dir" else "file"

    # Escape names/URLs before embedding filesystem entries into HTML.
    rows[[i]] <- paste0(
      '<div class="entry" data-type="',
      kind,
      '" data-name="',
      .static_html_escape(name),
      '" data-size="',
      size_raw,
      '" data-mtime="',
      mtime_raw,
      '">',
      '<a class="name" href="',
      .static_html_escape(href),
      '">',
      escaped_name,
      '</a>',
      '<span class="size">',
      .static_html_escape(size),
      '</span>',
      '<span class="time">',
      .static_html_escape(mtime),
      '</span></div>'
    )
  }

  breadcrumb <- .static_breadcrumb(request_path, prefix)
  has_listing_assets <- !is.null(listing_dir) &&
    length(listing_dir) == 1L &&
    !is.na(listing_dir) &&
    nzchar(listing_dir)
  template_path <- if (has_listing_assets) {
    file.path(listing_dir, "directory-listing.html")
  } else {
    ""
  }
  if (!nzchar(template_path) || !file.exists(template_path)) {
    body <- paste0(
      "<h1>Index of ",
      breadcrumb,
      "</h1>",
      paste(rows, collapse = "\n")
    )
  } else {
    # Inline trusted package assets so listings work even when custom mode has
    # deliberately disabled the framework's normal static-asset mount.
    body <- paste(
      readLines(template_path, warn = FALSE, encoding = "UTF-8"),
      collapse = "\n"
    )
    body <- sub("{{breadcrumb}}", breadcrumb, body, fixed = TRUE)
    body <- sub("{{files}}", paste(rows, collapse = "\n"), body, fixed = TRUE)
    styles_path <- file.path(listing_dir, "directory-listing.css")
    script_path <- file.path(listing_dir, "directory-listing.js")
    styles <- if (file.exists(styles_path)) {
      paste(
        readLines(styles_path, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
    } else {
      ""
    }
    script <- if (file.exists(script_path)) {
      paste(
        readLines(script_path, warn = FALSE, encoding = "UTF-8"),
        collapse = "\n"
      )
    } else {
      ""
    }
    body <- sub("{{styles}}", styles, body, fixed = TRUE)
    body <- sub("{{script}}", script, body, fixed = TRUE)
  }

  list(
    status = 200L,
    headers = c("Content-Type" = "text/html; charset=utf-8"),
    body = body
  )
}

# Build links from the mount root to the currently browsed subdirectory.
.static_breadcrumb <- function(path, prefix) {
  root_href <- if (prefix == "") "/" else paste0(prefix, "/")
  crumbs <- paste0('<a href="', .static_html_escape(root_href), '">/</a>')
  relative <- if (prefix == "") {
    sub("^/+|/+$", "", path)
  } else {
    sub("^/+|/+$", "", substring(path, nchar(prefix) + 1L))
  }
  if (!nzchar(relative)) {
    return(crumbs)
  }

  pieces <- strsplit(relative, "/", fixed = TRUE)[[1]]
  current <- root_href
  for (piece in pieces) {
    current <- paste0(current, utils::URLencode(piece, reserved = TRUE), "/")
    crumbs <- paste0(
      crumbs,
      " / <a href=\"",
      .static_html_escape(current),
      "\">",
      .static_html_escape(piece),
      "</a>"
    )
  }
  crumbs
}

# Escape filesystem-derived text before interpolating it into generated HTML.
.static_html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

# Format byte counts into concise values for the listing's size column.
.static_format_size <- function(size) {
  if (size < 1024) {
    return(paste0(size, " B"))
  }
  if (size < 1024^2) {
    return(sprintf("%.1f KB", size / 1024))
  }
  sprintf("%.1f MB", size / 1024^2)
}
