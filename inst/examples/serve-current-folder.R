# Serve the current R working directory as a browsable static folder.
# Run this only when the current directory contains files you are comfortable
# exposing. By default, run_app() binds to 127.0.0.1 (local machine only).

library(tinyVanR)

static_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

app <- tinyvanr_app(
  frontend = van$div(),
  backend = function(input, output, on) {},
  title = "Current Folder"
)

run_app(
  app,
  port = 8080,
  custom = TRUE,
  static = list(
    list(
      dir = static_dir,
      prefix = "/",
      list_dirs = TRUE
    )
  )
)

# Open http://127.0.0.1:8080/ to browse the current working directory.
# Set static_dir to a different folder if you do not want to expose getwd().
