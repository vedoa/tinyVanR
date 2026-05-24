source("../../R/framework.R")
source("../../R/app.R")
source("../../R/tags.R")

# Define UI entirely in R using the van helper object
frontend_ui <- van$div(
  van$h3("R-Defined UI (with van helper)"),
  van$div(
    "Adjust: ",
    van$input(id = "slider_1", type = "range", min = 0, max = 100)
  ),
  van$div(id = "val_display"),
  van$hr(),
  van$button(id = "btn_toggle", "Toggle Lock"),
  van$div(id = "status_msg"),
  van$hr(),
  van$p("This is a custom paragraph added to the UI."),
  van$a(href = "https://www.r-project.org/", "Visit R-Project")
)

#' A simple server-side application logic
simple_backend <- function(input, output, on) {
  slider_disabled <- FALSE

  # Set initial state
  output$set("val_display", "Value is: 0")
  output$set("status_msg", "Slider is now Active")
  output$set_prop("status_msg", "style", "color: green")
  output$set_prop("slider_1", "value", 0)

  # Sync slider value
  on("slider_1", function(val) {
    output$set("val_display", paste("Value is:", val))
  })

  # Toggle properties of other elements
  on("btn_toggle", function(val) {
    slider_disabled <<- !slider_disabled
    output$set_prop("slider_1", "disabled", slider_disabled)

    status <- if (slider_disabled) {
      "Slider is now Locked"
    } else {
      "Slider is now Active"
    }
    output$set("status_msg", status)
    output$set_prop(
      "status_msg",
      "style",
      if (slider_disabled) "color: red" else "color: green"
    )
  })
}

# Initialize and launch
app <- tinyvanr_app(
  frontend = frontend_ui,
  backend = simple_backend,
  title = "My Dynamic R App (van helper)",
  head = list(
    after = list(
      van$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/water.css@2/out/water.css"),
      van$link(rel = "stylesheet", href = "custom.css")
    )
  )
)
run_app(app, port = 8080, timeout_ms = 20L)