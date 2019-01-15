#' @title Create a threejs graph from a tibble graph
#' 
#' @description Supply a graph to be rendered as a threejs graph.
#' @param g the graph to visualize. Note that graph and vertex properties
#' corresponding to threejs arguments will be used when creating the graph.
#' See the threejs documentation for all possible arguments.
#' @param ... other arguments. Currently not used.
#' @importFrom threejs graphjs
#' @importFrom tidygraph %E>% %N>%
#' @examples
#' library(tidygraph)
#' library(dplyr)
#' rstat_nodes <- tibble(name = c("Rick", "Mike", "Brian", "Yuan"))
#' rstat_edges <- tibble(from = c(1, 1, 2, 2, 3, 3),
#'                         to = c(2, 3, 3, 4, 1, 4))
#' 
#' Create a graph. Mouse over a vertex to see the name.
#' tbl_graph(nodes = rstat_nodes, edges = rstat_edges) %E>%
#'   mutate(color = factor(c(1, 1, 1, 1, 1, 2))) %>%
#'   js()
#' @export
js <- function(g, ... ) {
  UseMethod("js", g)
}

#' @export
js.tbl_graph <- function(g, ... ) {

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

