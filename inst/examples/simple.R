# library(tinyVanR)

#' A simple server-side application logic
simple_server <- function(input, output, on) {
  message("Session Active.")

  # Update output when 'num_in' changes
  on("num_in", function(val) {
    # val is already coerced to numeric by the dispatcher
    doubled <- val * 2
    output$set("num_out", doubled)
  })

  # Handle button click
  on("btn_click", function(val) {
    current_time <- format(Sys.time(), "%H:%M:%S")
    output$set("btn_res", paste("Button pressed at:", current_time))
  })
}

# Initialize and launch
app <- tiny_app(server_func = simple_server)

run_app(app, port = 8888)