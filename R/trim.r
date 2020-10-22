#' @title Trim the low-weight edges from a graph
#' 
#' @description Remove edges in a graph whose weights are small. This function
#' is meant to visualize full-connected (or almost fully connected graphs) 
#' where many of the edges may be small.
#' @param x a subgroup object.
#' @param k parameter for the trim method. Currently, it can be either
#' "top_k", which keeps the edges with the k largest weights or 
#' "cutoff" where edges whose weights are less than the cutoff are removed.
#' @param method the method for trimming edges. Current options are the
#' default, "top_k" which retains the edges with the k highest values or
#' "cutoff" which is value a weight must be above to remain in the graph.
#' @export
trim_edges <- function(x, k, method = "top_k") {
  UseMethod("trim_edges")
}

#' @importFrom igraph V E
#' @export
trim_edges.igraph <- function(g, k = 2*length(V(g)), method = "top_k") {
  trim_edges(as_tbl_graph(g), k, method)
}

#' @importFrom igraph E
#' @importFrom tidygraph activate filter
#' @export
trim_edges.tbl_graph <- function(g, k = 2*length(V(g)), method = "top_k") {
  k <- min(k, length(E(g)))
  if (method == "top_k") {
    ws <- order(E(g)$weight, decreasing = TRUE)
    cutoff <- E(g)$weight[ws][min(length(E(g)), k)]
  } else if (method == "cutoff") {
    cutoff <- k
  }
  g %E>% filter(weight > cutoff)
}

