#' Functional enrichment of bridge hub genes
#'
#' @param genes Character vector.
#' @param ont "BP", "MF", or "CC".
#' @param pvalueCutoff Numeric.
#' @param verbose Logical.
#' @return enrichResult object.
#' @keywords internal
enrich_bridge <- function(genes, ont = "BP", pvalueCutoff = 0.05,
                          verbose = TRUE) {
  if (length(genes) < 3) stop("Need at least 3 genes.")
  if (verbose) message("[BridgeNet] Running GO enrichment...")
  clusterProfiler::enrichGO(
    gene = genes,
    OrgDb = org.Hs.eg.db::org.Hs.eg.db,
    keyType = "SYMBOL",
    ont = ont,
    pvalueCutoff = pvalueCutoff,
    qvalueCutoff = pvalueCutoff
  )
}
