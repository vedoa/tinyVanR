# tinyVanR

tinyVanR is a reactive web framework for R. It uses the CivetWeb embedded C server for the backend and VanJS for the frontend. Communication is handled via WebSockets.

## Core Components

*   **Backend**: R session running `civetwebR`.
*   **Frontend**: VanJS, a minimal DOM-reactive library.
*   **Interface**: A pure R DSL for defining HTML structures.

## Installation

```r
devtools::install_github("vedoa/tinyVanR")
```

## License

MIT