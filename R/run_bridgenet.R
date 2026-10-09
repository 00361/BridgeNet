#' Run complete BridgeNet pipeline
#'
#' @param targets Drug target input.
#' @param core_genes Core gene vector.
#' @param source "STRING" or "custom".
#' @param score STRING score.
#' @param top_n Integer.
#' @param enrich Logical.
#' @param verbose Logical.
#' @return List of all results.
#' @export
run_bridgenet <- function(targets, core_genes, source = "STRING",
                          score = 400, top_n = 10, enrich = TRUE,
                          verbose = TRUE) {
  target_genes <- read_targets(targets)
  edges <- build_bridge(target_genes, core_genes, source = source,
                        score = score, verbose = verbose)
  hub_result <- find_bridge_hub(edges, core_genes, top_n = top_n,
                                verbose = verbose)
  enrichment <- NULL
  if (enrich && nrow(hub_result$bridge) >= 3) {
    enrichment <- tryCatch(
      enrich_bridge(hub_result$bridge$gene, verbose = verbose),
      error = function(e) { warning("Enrichment failed: ", e$message); NULL }
    )
  }
  list(
    targets = target_genes,
    edges = edges,
    bridge = hub_result$bridge,
    centrality = hub_result$centrality,
    graph = hub_result$graph,
    enrichment = enrichment
  )
}
