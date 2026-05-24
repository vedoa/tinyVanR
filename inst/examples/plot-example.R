# Simulated dataset
set.seed(42)
observations <- rnorm(1000)

# Create a temporary directory for session plots
plot_dir <- file.path(tempdir(), "tinyvanr_plots")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, showWarnings = FALSE)
}

# Define UI
frontend_ui <- van$div(
  class = "container",
  van$h2("Real-time R Plotting"),
  van$p(
    "This example shows how to bridge R's plotting device with the VanJS frontend."
  ),

  van$div(
    van$label(for_ = "bins", "Histogram Bins: "),
    van$input(
      id = "bins",
      type = "range",
      min = 1,
      max = 100,
      value = 30,
      style = "width: 100%;"
    )
  ),
  van$div(id = "bin_counter", style = "font-weight: bold; margin: 10px 0;"),
  van$br(),
  # The img tag will receive the Base64 data in its 'src' property
  van$img(
    id = "plot_output",
    style = "width: 100%; max-width: 600px; height: auto; border: 1px solid #ddd; background: #fff;"
  )
)

# Backend logic
plot_backend <- function(input, output, on) {
  # Generate a unique filename prefix for this session to avoid collisions
  session_prefix <- paste0(sample(c(letters, 0:9), 8), collapse = "")

  render_histogram <- function(bin_count) {
    # Update the text readout
    output$set("bin_counter", paste("Selected Bins:", bin_count))

    # 1. Save plot to the served static directory
    plot_filename <- sprintf("hist_%s.png", session_prefix)
    plot_path <- file.path(plot_dir, plot_filename)

    png(plot_path, width = 800, height = 500, res = 120)
    par(mar = c(4, 4, 2, 1))
    hist(
      observations,
      breaks = as.numeric(bin_count),
      col = "#4A90E2",
      border = "white",
      main = "Dynamic Distribution",
      xlab = "Value"
    )
    dev.off()

    # 2. Push the URL to the frontend.
    # We add a timestamp (?t=...) to force the browser to bypass the cache.
    output$set_prop(
      "plot_output",
      "src",
      sprintf("/plots/%s?t=%f", plot_filename, as.numeric(Sys.time()))
    )
  }

  # Initial Render
  render_histogram(30)

  # Reactive update on slider move
  on("bins", render_histogram)
}

app <- tinyvanr_app(frontend_ui, plot_backend, title = "tinyVanR Plotting")

# Launch with the plot directory registered as a static route
run_app(
  app,
  port = 8000,
  static = list(
    list(dir = plot_dir, prefix = "/plots")
  )
)
