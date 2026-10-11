# Serve files from a directory, with an optional sortable directory listing.
# Run this example with tinyVanR installed, or after devtools::load_all().

library(tinyVanR)

# Prepare a small example folder. Replace this with your own existing folder
# when you want to serve real assets.
static_dir <- file.path(tempdir(), "tinyvanr_static_example")
images_dir <- file.path(static_dir, "images")
dir.create(images_dir, recursive = TRUE, showWarnings = FALSE)

writeLines(
  "Hello from a static text file.",
  file.path(static_dir, "hello.txt")
)
writeLines(
  "This file is in the nested images folder.",
  file.path(images_dir, "readme.txt")
)

# This app page links to the mounted directory. The listing shows files and
# subdirectories under static_dir; clicking a row opens that resource.
frontend <- van$div(
  van$h1("Static folder example"),
  van$p("Open the generated, sortable directory listing:"),
  van$a(href = "/files/", "Browse the static folder")
)

app <- tinyvanr_app(
  frontend = frontend,
  backend = function(input, output, on) {},
  title = "Static Files"
)

run_app(
  app,
  port = 8080,
  static = list(
    # Directory listings are disabled by default. Enable them for this mount.
    list(dir = static_dir, prefix = "/files", list_dirs = TRUE)
  )
)
