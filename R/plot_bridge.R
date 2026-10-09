#' Plot bridge network
#'
#' @param graph igraph object.
#' @param highlight Character vector of nodes to highlight.
#' @param title Character.
#' @param layout "fr", "kk", or "circle".
#' @return ggplot object.
#' @export
plot_bridge <- function(graph, highlight = NULL, title = "Bridge network",
                        layout = "fr") {
  set.seed(123)
  layout_fn <- switch(layout,
                      "fr" = igraph::layout_with_fr,
                      "kk" = igraph::layout_with_kk,
                      "circle" = igraph::layout_in_circle,
                      igraph::layout_with_fr
  )
  coords <- layout_fn(graph)
  nodes_df <- data.frame(
    name = igraph::V(graph)$name,
    x = coords[, 1], y = coords[, 2],
    degree = igraph::degree(graph),
    stringsAsFactors = FALSE
  )
  nodes_df$type <- ifelse(nodes_df$name %in% highlight, "Highlight", "Other")
  edges_df <- igraph::as_data_frame(graph, what = "edges")
  edges_df$x1 <- nodes_df$x[match(edges_df$from, nodes_df$name)]
  edges_df$y1 <- nodes_df$y[match(edges_df$from, nodes_df$name)]
  edges_df$x2 <- nodes_df$x[match(edges_df$to, nodes_df$name)]
  edges_df$y2 <- nodes_df$y[match(edges_df$to, nodes_df$name)]
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges_df,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = "grey80", alpha = 0.4
    ) +
    ggplot2::geom_point(
      data = nodes_df,
      ggplot2::aes(x = x, y = y, color = type, size = degree)
    ) +
    ggplot2::scale_color_manual(
      values = c("Highlight" = "#E64B35", "Other" = "#4DBBD5")
    ) +
    ggplot2::scale_size_continuous(range = c(3, 10)) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(title = title, color = "Node type", size = "Degree")
}
