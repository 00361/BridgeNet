#' Identify bridge hub genes
#'
#' @param edges Edge list from build_bridge.
#' @param core_genes Character vector of core genes.
#' @param top_n Integer.
#' @param method "fused", "degree", or "betweenness".
#' @param verbose Logical.
#' @return List with bridge, centrality, graph.
#' @export
find_bridge_hub <- function(edges, core_genes, top_n = 10,
                            method = "fused", verbose = TRUE) {
  node_cols <- .find_node_columns(edges)
  if (is.null(node_cols)) {
    if (all(c("node1", "node2") %in% colnames(edges))) {
      node_cols <- c("node1", "node2")
    } else {
      stop("Cannot identify node columns.")
    }
  }
  g <- igraph::graph_from_data_frame(edges[, node_cols], directed = FALSE)
  if (verbose) message("[BridgeNet] Network: ", igraph::vcount(g),
                       " nodes, ", igraph::ecount(g), " edges.")
  centrality <- data.frame(
    gene = igraph::V(g)$name,
    Degree = igraph::degree(g),
    Betweenness = igraph::betweenness(g),
    Closeness = igraph::closeness(g),
    Eigenvector = igraph::eigen_centrality(g)$vector,
    stringsAsFactors = FALSE
  )
  centrality <- centrality[order(-centrality$Degree), ]
  all_neighbors <- unique(unlist(lapply(core_genes, function(x) {
    if (x %in% igraph::V(g)$name) igraph::neighbors(g, x)$name else NULL
  })))
  if (length(all_neighbors) == 0) warning("Core genes not in network.")
  bridge <- centrality[centrality$gene %in% all_neighbors, ]
  bridge <- utils::head(bridge, top_n)
  rownames(bridge) <- NULL
  list(bridge = bridge, centrality = centrality, graph = g)
}
