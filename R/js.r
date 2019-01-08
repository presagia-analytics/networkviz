#' @export
js <- function(g, main, bg, width, height, ... ) {
  UseMethod("js", g)
}

#' @importFrom threejs graphjs
#' @importFrom tidygraph %E>% %N>%
#' @export
js.tbl_graph <- function(g, main = "", bg = "white",
  width = NULL, height = NULL, ... ) {

  et <- g %E>% as_tibble
  vt <- g %N>% as_tibble
  args <- list(g = g)
  if ("size" %in% names(vt)) {
    args$vertex.size <- vt$size
  } else {
    args$vertex.size <- 0.4
  }
  if ("color" %in% names(vt)) {
    args$vertex.color <- vt$color
  }
  if ("shape" %in% names(vt)) {
    args$vertex.shape <- vt$shape
  }
  if ("name" %in% names(vt)) {
    args$vertex.label <- vt$name
  }
  if ("color" %in% names(et)) {
    args$edge.color <- et$color
  } else {
    args$edge.color <- "black"
  }
  if ("width" %in% names(et)) {
    args$edge.width <- et$width
  }
  if ("alpha" %in% names(et)) {
    args$edge.alpha <- et$alpha
  }
  args <- append(args, list(...))
  do.call(graphjs, args)
}

