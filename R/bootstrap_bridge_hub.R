#' Bootstrap-based bridge hub identification
#'
#' @param edges Edge list with columns node1, node2 (or STRING format).
#' @param core_genes Character vector of core genes.
#' @param n_boot Integer. Number of bootstrap resamplings (default 500).
#' @param top_pct Numeric. Top percentage threshold (default 0.1).
#' @param weights Named numeric vector of centrality weights.
#' @param verbose Logical. Print progress.
#' @return Data frame with gene, prob_bridge, Degree.
#' @export
bootstrap_bridge_hub <- function(edges, core_genes,
                                 n_boot = 500, top_pct = 0.1,
                                 weights = c(Degree = 0.4, Betweenness = 0.3,
                                             Closeness = 0.15, Eigenvector = 0.15),
                                 verbose = TRUE) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required.")
  }
  node_cols <- .find_node_columns(edges)
  if (is.null(node_cols)) {
    if (all(c("node1", "node2") %in% colnames(edges))) {
      node_cols <- c("node1", "node2")
    } else {
      stop("Cannot identify node columns.")
    }
  }
  edge_df <- data.frame(
    from = edges[[node_cols[1]]],
    to = edges[[node_cols[2]]],
    stringsAsFactors = FALSE
  )
  g <- igraph::graph_from_data_frame(edge_df, directed = FALSE)

  compute_fused <- function(g) {
    deg <- igraph::degree(g)
    bet <- igraph::betweenness(g)
    clo <- igraph::closeness(g)
    eig <- igraph::eigen_centrality(g)$vector
    norm <- function(x) {
      x[is.na(x)] <- 0
      rng <- range(x)
      if (rng[1] == rng[2]) return(rep(0, length(x)))
      (x - rng[1]) / (rng[2] - rng[1])
    }
    fused <- norm(deg) * weights["Degree"] +
      norm(bet) * weights["Betweenness"] +
      norm(clo) * weights["Closeness"] +
      norm(eig) * weights["Eigenvector"]
    names(fused) <- igraph::V(g)$name
    fused
  }

  all_nodes <- igraph::V(g)$name
  count_top <- setNames(rep(0, length(all_nodes)), all_nodes)

  if (verbose) message("[PhytoBridge] Bootstrap resampling (n = ", n_boot, ")...")

  set.seed(123)
  for (i in 1:n_boot) {
    idx <- sample(nrow(edge_df), replace = TRUE)
    g_boot <- igraph::graph_from_data_frame(
      edge_df[idx, ],
      vertices = data.frame(name = all_nodes),
      directed = FALSE
    )
    fused_boot <- compute_fused(g_boot)
    fused_boot[is.na(fused_boot)] <- 0
    threshold <- stats::quantile(fused_boot, 1 - top_pct, na.rm = TRUE)
    top_nodes <- names(fused_boot)[fused_boot >= threshold]
    count_top[top_nodes] <- count_top[top_nodes] + 1
  }

  prob_bridge <- count_top / n_boot
  result <- data.frame(
    gene = names(prob_bridge),
    prob_bridge = round(prob_bridge, 3),
    Degree = igraph::degree(g)[names(prob_bridge)],
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$prob_bridge, -result$Degree), ]
  rownames(result) <- NULL
  result
}
