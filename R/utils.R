# 内部函数：自动识别节点列名
.find_node_columns <- function(df) {
  candidates <- list(
    c("preferredName_A", "preferredName_B"),
    c("#node1", "node2"),
    c("node1", "node2"),
    c("stringId_A", "stringId_B"),
    c("from", "to"),
    c("Source", "Target")
  )
  for (pair in candidates) {
    if (all(pair %in% colnames(df))) return(pair)
  }
  NULL
}

# 内部函数：标准化边列表
.standardize_edges <- function(df) {
  node_cols <- .find_node_columns(df)
  if (is.null(node_cols)) {
    stop("Cannot identify node columns. Available: ",
         paste(colnames(df), collapse = ", "))
  }
  score_col <- if ("score" %in% colnames(df)) "score" else
    if ("combined_score" %in% colnames(df)) "combined_score" else NULL
  result <- data.frame(
    node1 = df[[node_cols[1]]],
    node2 = df[[node_cols[2]]],
    stringsAsFactors = FALSE
  )
  if (!is.null(score_col)) result$score <- df[[score_col]]
  result
}
