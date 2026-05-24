source("../../R/framework.R")
source("../../R/app.R")

# Define UI entirely in R
ui <- vtag(
  "div",
  vtag("h3", "R-Defined UI"),
  vtag("div", 
    "Adjust: ",
    vtag("input", id = "slider_1", type = "range", min = 0, max = 100)
  ),
  vtag("div", id = "val_display"),
  vtag("hr"),
  vtag("button", id = "btn_toggle", "Toggle Lock"),
  vtag("div", id = "status_msg")
)

#' A simple server-side application logic
simple_server <- function(input, output, on) {
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

  # Sync select value
  on("sel_1", function(val) {
    output$set("sel_out", paste("You chose:", val))
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
app <- tiny_app(ui = ui, server_func = simple_server)
run_app(app, port = 8080)
