# tinyVanR

**tinyVanR** is a small reactive web framework for R. It uses [nanonext](https://nanonext.r-lib.org/) for its HTTP and WebSocket server, and [VanJS](https://vanjs.org/) to render and update the browser UI.

## Installation

Install the released GitHub version with:

```r
devtools::install_github("vedoa/tinyVanR")
```

The package requires R and nanonext (version 1.8.2 or later). No JavaScript runtime is required to run an installed tinyVanR app; the browser assets are included in the R package.

## Quick start

```r
library(tinyVanR)

ui <- van$div(
  van$h1("Hello from R"),
  van$input(id = "name", type = "text", placeholder = "Your name"),
  van$p(id = "greeting")
)

server <- function(input, output, on) {
  on("name", function(value) {
    output$set("greeting", paste("Hello", value))
  })
}

app <- tinyvanr_app(ui, server, title = "Hello")
run_app(app, port = 8080)
```

Open <http://127.0.0.1:8080/> while the R process is running. Stop the server with Ctrl+C in the R console.

## UI construction

Use `van$...` helpers for common HTML elements:

```r
van$div(van$h2("A heading"), van$p("A paragraph"))
```

Or call `vtag()` directly if you need a tag without a helper:

```r
vtag("custom-element", "content", id = "custom")
```

Both produce the same `tinyvanr_ui` structures. Attributes are named arguments and child elements or text are positional arguments. An `id` makes an element addressable for server-side binding and updates.

## Server API

The `backend` function passed to `tinyvanr_app()` has the form `function(input, output, on)`:

- `input$get(id)` returns the latest input value recorded for an element.
- `on(id, callback)` registers a callback for an element's input or click events.
- `output$set(id, value)` updates the text content of a bound element.
- `output$set_prop(id, property, value)` updates a DOM property, such as an input's `value`, a button's `disabled` state, or an image's `src`.

For these APIs to target an element, give it an `id` in the UI and use that same ID in the backend. Input elements and buttons send their values to the server over the WebSocket connection.

## Static files

Pass static directories to `run_app()` as a list of `dir` and `prefix` entries:

```r
run_app(
  app,
  port = 8080,
  static = list(list(dir = "path/to/files", prefix = "/assets"))
)
```

A file at `path/to/files/logo.png` is served at `/assets/logo.png`. The directory URL `/assets/` is not a file listing; it returns an index file only if the directory contains `index.html`.

## Examples

Runnable examples are included in `inst/examples/`:

- `simple.R` builds the UI with `vtag()`.
- `simple-with-van.R` builds the UI with the `van$...` helpers.
- `plot-example.R` serves generated histogram images from a static route.

When running an example from a source checkout, load the package first (for example, with `devtools::load_all()`) and then source the example. Each example starts a server and occupies the R process until it is stopped.

## License

MIT. See [LICENSE.md](LICENSE.md).
