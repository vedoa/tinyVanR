#' VanJS Tag Helpers
#'
#' A collection of functions to create HTML elements using the \code{vtag} system.
#' This mimics the VanJS \code{van.tags} API, providing a clean syntax for UI definition.
#'
#' @examples
#' van$div(
#'   van$h1("Hello"),
#'   van$p("This is a paragraph", class = "text-muted"),
#'   id = "main-container"
#' )
#'
#' @export
van <- new.env(parent = emptyenv())

# Common tags with explicit definitions for better Intellisense
van$a <- function(..., href = NULL, target = NULL, class = NULL, id = NULL) {
  vtag("a", href = href, target = target, class = class, ..., id = id)
}
van$abbr <- function(..., class = NULL, id = NULL) {
  vtag("abbr", class = class, ..., id = id)
}
van$address <- function(..., class = NULL, id = NULL) {
  vtag("address", class = class, ..., id = id)
}
van$area <- function(..., class = NULL, id = NULL) {
  vtag("area", class = class, ..., id = id)
}
van$article <- function(..., class = NULL, id = NULL) {
  vtag("article", class = class, ..., id = id)
}
van$aside <- function(..., class = NULL, id = NULL) {
  vtag("aside", class = class, ..., id = id)
}
van$audio <- function(
  ...,
  src = NULL,
  autoplay = NULL,
  controls = NULL,
  loop = NULL,
  class = NULL,
  id = NULL
) {
  vtag(
    "audio",
    src = src,
    autoplay = autoplay,
    controls = controls,
    loop = loop,
    class = class,
    ...,
    id = id
  )
}
van$b <- function(..., class = NULL, id = NULL) {
  vtag("b", class = class, ..., id = id)
}
van$base <- function(..., class = NULL, id = NULL) {
  vtag("base", class = class, ..., id = id)
}
van$bdi <- function(..., class = NULL, id = NULL) {
  vtag("bdi", class = class, ..., id = id)
}
van$bdo <- function(..., class = NULL, id = NULL) {
  vtag("bdo", class = class, ..., id = id)
}
van$blockquote <- function(..., class = NULL, id = NULL) {
  vtag("blockquote", class = class, ..., id = id)
}
van$body <- function(..., class = NULL, id = NULL) {
  vtag("body", class = class, ..., id = id)
}
van$br <- function(..., class = NULL, id = NULL) {
  vtag("br", class = class, ..., id = id)
}
van$button <- function(
  ...,
  type = NULL,
  disabled = NULL,
  class = NULL,
  id = NULL
) {
  vtag("button", type = type, disabled = disabled, class = class, ..., id = id)
}
van$canvas <- function(
  ...,
  width = NULL,
  height = NULL,
  class = NULL,
  id = NULL
) {
  vtag("canvas", width = width, height = height, class = class, ..., id = id)
}
van$caption <- function(..., class = NULL, id = NULL) {
  vtag("caption", class = class, ..., id = id)
}
van$cite <- function(..., class = NULL, id = NULL) {
  vtag("cite", class = class, ..., id = id)
}
van$code <- function(..., class = NULL, id = NULL) {
  vtag("code", class = class, ..., id = id)
}
van$col <- function(..., class = NULL, id = NULL) {
  vtag("col", class = class, ..., id = id)
}
van$colgroup <- function(..., class = NULL, id = NULL) {
  vtag("colgroup", class = class, ..., id = id)
}
van$data <- function(..., class = NULL, id = NULL) {
  vtag("data", class = class, ..., id = id)
}
van$datalist <- function(..., class = NULL, id = NULL) {
  vtag("datalist", class = class, ..., id = id)
}
van$dd <- function(..., class = NULL, id = NULL) {
  vtag("dd", class = class, ..., id = id)
}
van$del <- function(..., class = NULL, id = NULL) {
  vtag("del", class = class, ..., id = id)
}
van$details <- function(..., class = NULL, id = NULL) {
  vtag("details", class = class, ..., id = id)
}
van$dfn <- function(..., class = NULL, id = NULL) {
  vtag("dfn", class = class, ..., id = id)
}
van$dialog <- function(..., class = NULL, id = NULL) {
  vtag("dialog", class = class, ..., id = id)
}
van$div <- function(..., class = NULL, id = NULL) {
  vtag("div", class = class, ..., id = id)
}
van$dl <- function(..., class = NULL, id = NULL) {
  vtag("dl", class = class, ..., id = id)
}
van$dt <- function(..., class = NULL, id = NULL) {
  vtag("dt", class = class, ..., id = id)
}
van$em <- function(..., class = NULL, id = NULL) {
  vtag("em", class = class, ..., id = id)
}
van$embed <- function(..., class = NULL, id = NULL) {
  vtag("embed", class = class, ..., id = id)
}
van$fieldset <- function(..., class = NULL, id = NULL) {
  vtag("fieldset", class = class, ..., id = id)
}
van$figcaption <- function(..., class = NULL, id = NULL) {
  vtag("figcaption", class = class, ..., id = id)
}
van$figure <- function(..., class = NULL, id = NULL) {
  vtag("figure", class = class, ..., id = id)
}
van$footer <- function(..., class = NULL, id = NULL) {
  vtag("footer", class = class, ..., id = id)
}
van$form <- function(
  ...,
  action = NULL,
  method = NULL,
  class = NULL,
  id = NULL
) {
  vtag("form", action = action, method = method, class = class, ..., id = id)
}
van$h1 <- function(..., class = NULL, id = NULL) {
  vtag("h1", class = class, ..., id = id)
}
van$h2 <- function(..., class = NULL, id = NULL) {
  vtag("h2", class = class, ..., id = id)
}
van$h3 <- function(..., class = NULL, id = NULL) {
  vtag("h3", class = class, ..., id = id)
}
van$h4 <- function(..., class = NULL, id = NULL) {
  vtag("h4", class = class, ..., id = id)
}
van$h5 <- function(..., class = NULL, id = NULL) {
  vtag("h5", class = class, ..., id = id)
}
van$h6 <- function(..., class = NULL, id = NULL) {
  vtag("h6", class = class, ..., id = id)
}
van$head <- function(..., class = NULL, id = NULL) {
  vtag("head", class = class, ..., id = id)
}
van$header <- function(..., class = NULL, id = NULL) {
  vtag("header", class = class, ..., id = id)
}
van$hgroup <- function(..., class = NULL, id = NULL) {
  vtag("hgroup", class = class, ..., id = id)
}
van$hr <- function(..., class = NULL, id = NULL) {
  vtag("hr", class = class, ..., id = id)
}
van$html <- function(..., class = NULL, id = NULL) {
  vtag("html", class = class, ..., id = id)
}
van$i <- function(..., class = NULL, id = NULL) {
  vtag("i", class = class, ..., id = id)
}
van$iframe <- function(
  ...,
  src = NULL,
  width = NULL,
  height = NULL,
  class = NULL,
  id = NULL
) {
  vtag(
    "iframe",
    src = src,
    width = width,
    height = height,
    class = class,
    ...,
    id = id
  )
}
van$img <- function(
  ...,
  src = NULL,
  alt = NULL,
  width = NULL,
  height = NULL,
  class = NULL,
  id = NULL
) {
  vtag(
    "img",
    src = src,
    alt = alt,
    width = width,
    height = height,
    class = class,
    ...,
    id = id
  )
}
van$input <- function(
  ...,
  type = NULL,
  value = NULL,
  placeholder = NULL,
  checked = NULL,
  class = NULL,
  id = NULL
) {
  vtag(
    "input",
    type = type,
    value = value,
    placeholder = placeholder,
    checked = checked,
    class = class,
    ...,
    id = id
  )
}
van$ins <- function(..., class = NULL, id = NULL) {
  vtag("ins", class = class, ..., id = id)
}
van$kbd <- function(..., class = NULL, id = NULL) {
  vtag("kbd", class = class, ..., id = id)
}
van$label <- function(..., for_ = NULL, class = NULL, id = NULL) {
  vtag("label", `for` = for_, class = class, ..., id = id)
}
van$legend <- function(..., class = NULL, id = NULL) {
  vtag("legend", class = class, ..., id = id)
}
van$li <- function(..., class = NULL, id = NULL) {
  vtag("li", class = class, ..., id = id)
}
van$link <- function(..., rel = NULL, href = NULL, class = NULL, id = NULL) {
  vtag("link", rel = rel, href = href, class = class, ..., id = id)
}
van$main <- function(..., class = NULL, id = NULL) {
  vtag("main", class = class, ..., id = id)
}
van$map <- function(..., class = NULL, id = NULL) {
  vtag("map", class = class, ..., id = id)
}
van$mark <- function(..., class = NULL, id = NULL) {
  vtag("mark", class = class, ..., id = id)
}
van$menu <- function(..., class = NULL, id = NULL) {
  vtag("menu", class = class, ..., id = id)
}
van$meta <- function(
  ...,
  name = NULL,
  content = NULL,
  class = NULL,
  id = NULL
) {
  vtag("meta", name = name, content = content, class = class, ..., id = id)
}
van$meter <- function(..., class = NULL, id = NULL) {
  vtag("meter", class = class, ..., id = id)
}
van$nav <- function(..., class = NULL, id = NULL) {
  vtag("nav", class = class, ..., id = id)
}
van$noscript <- function(..., class = NULL, id = NULL) {
  vtag("noscript", class = class, ..., id = id)
}
van$object <- function(..., class = NULL, id = NULL) {
  vtag("object", class = class, ..., id = id)
}
van$ol <- function(..., class = NULL, id = NULL) {
  vtag("ol", class = class, ..., id = id)
}
van$optgroup <- function(..., class = NULL, id = NULL) {
  vtag("optgroup", class = class, ..., id = id)
}
van$option <- function(
  ...,
  value = NULL,
  selected = NULL,
  disabled = NULL,
  class = NULL,
  id = NULL
) {
  vtag(
    "option",
    value = value,
    selected = selected,
    disabled = disabled,
    class = class,
    ...,
    id = id
  )
}
van$output <- function(..., class = NULL, id = NULL) {
  vtag("output", class = class, ..., id = id)
}
van$p <- function(..., class = NULL, id = NULL) {
  vtag("p", class = class, ..., id = id)
}
van$picture <- function(..., class = NULL, id = NULL) {
  vtag("picture", class = class, ..., id = id)
}
van$pre <- function(..., class = NULL, id = NULL) {
  vtag("pre", class = class, ..., id = id)
}
van$progress <- function(..., class = NULL, id = NULL) {
  vtag("progress", class = class, ..., id = id)
}
van$q <- function(..., class = NULL, id = NULL) {
  vtag("q", class = class, ..., id = id)
}
van$rp <- function(..., class = NULL, id = NULL) {
  vtag("rp", class = class, ..., id = id)
}
van$rt <- function(..., class = NULL, id = NULL) {
  vtag("rt", class = class, ..., id = id)
}
van$ruby <- function(..., class = NULL, id = NULL) {
  vtag("ruby", class = class, ..., id = id)
}
van$s <- function(..., class = NULL, id = NULL) {
  vtag("s", class = class, ..., id = id)
}
van$samp <- function(..., class = NULL, id = NULL) {
  vtag("samp", class = class, ..., id = id)
}
van$script <- function(..., src = NULL, type = NULL, class = NULL, id = NULL) {
  vtag("script", src = src, type = type, class = class, ..., id = id)
}
van$section <- function(..., class = NULL, id = NULL) {
  vtag("section", class = class, ..., id = id)
}
van$select <- function(
  ...,
  name = NULL,
  disabled = NULL,
  class = NULL,
  id = NULL
) {
  vtag("select", name = name, disabled = disabled, class = class, ..., id = id)
}
van$slot <- function(..., class = NULL, id = NULL) {
  vtag("slot", class = class, ..., id = id)
}
van$small <- function(..., class = NULL, id = NULL) {
  vtag("small", class = class, ..., id = id)
}
van$source <- function(..., class = NULL, id = NULL) {
  vtag("source", class = class, ..., id = id)
}
van$span <- function(..., class = NULL, id = NULL) {
  vtag("span", class = class, ..., id = id)
}
van$strong <- function(..., class = NULL, id = NULL) {
  vtag("strong", class = class, ..., id = id)
}
van$style <- function(..., class = NULL, id = NULL) {
  vtag("style", class = class, ..., id = id)
}
van$sub <- function(..., class = NULL, id = NULL) {
  vtag("sub", class = class, ..., id = id)
}
van$summary <- function(..., class = NULL, id = NULL) {
  vtag("summary", class = class, ..., id = id)
}
van$sup <- function(..., class = NULL, id = NULL) {
  vtag("sup", class = class, ..., id = id)
}
van$table <- function(..., class = NULL, id = NULL) {
  vtag("table", class = class, ..., id = id)
}
van$tbody <- function(..., class = NULL, id = NULL) {
  vtag("tbody", class = class, ..., id = id)
}
van$td <- function(..., class = NULL, id = NULL) {
  vtag("td", class = class, ..., id = id)
}
van$template <- function(..., class = NULL, id = NULL) {
  vtag("template", class = class, ..., id = id)
}
van$textarea <- function(
  ...,
  placeholder = NULL,
  rows = NULL,
  cols = NULL,
  class = NULL,
  id = NULL
) {
  vtag(
    "textarea",
    placeholder = placeholder,
    rows = rows,
    cols = cols,
    class = class,
    ...,
    id = id
  )
}
van$tfoot <- function(..., class = NULL, id = NULL) {
  vtag("tfoot", class = class, ..., id = id)
}
van$th <- function(..., class = NULL, id = NULL) {
  vtag("th", class = class, ..., id = id)
}
van$thead <- function(..., class = NULL, id = NULL) {
  vtag("thead", class = class, ..., id = id)
}
van$time <- function(..., class = NULL, id = NULL) {
  vtag("time", class = class, ..., id = id)
}
van$title <- function(..., class = NULL, id = NULL) {
  vtag("title", class = class, ..., id = id)
}
van$tr <- function(..., class = NULL, id = NULL) {
  vtag("tr", class = class, ..., id = id)
}
van$track <- function(..., class = NULL, id = NULL) {
  vtag("track", class = class, ..., id = id)
}
van$u <- function(..., class = NULL, id = NULL) {
  vtag("u", class = class, ..., id = id)
}
van$ul <- function(..., class = NULL, id = NULL) {
  vtag("ul", class = class, ..., id = id)
}
van$var <- function(..., class = NULL, id = NULL) {
  vtag("var", class = class, ..., id = id)
}
van$video <- function(
  ...,
  src = NULL,
  autoplay = NULL,
  controls = NULL,
  loop = NULL,
  class = NULL,
  id = NULL
) {
  vtag(
    "video",
    src = src,
    autoplay = autoplay,
    controls = controls,
    loop = loop,
    class = class,
    ...,
    id = id
  )
}
van$wbr <- function(..., class = NULL, id = NULL) {
  vtag("wbr", class = class, ..., id = id)
}

# Convert to list to ensure easy exporting and Intellisense in RStudio
van <- as.list(van)
