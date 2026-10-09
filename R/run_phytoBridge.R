#' Run complete PhytoBridge pipeline
#'
#' @param phyto_targets Character vector or CSV path of natural product targets.
#' @param disease_genes Character vector of disease causal genes (from MR).
#' @param core_genes Character vector of core genes.
#' @param n_boot Integer. Bootstrap resamplings (default 500).
#' @param verbose Logical.
#' @return A list with phyto targets, bridge network, bootstrap result, and BPS.
#' @export
run_phytoBridge <- function(phyto_targets, disease_genes, core_genes,
                            n_boot = 500, verbose = TRUE) {
  targets <- read_targets(phyto_targets)
  if (verbose) message("[PhytoBridge] Loaded ", length(targets), " targets.")
  edges <- build_bridge(targets, disease_genes, verbose = verbose)
  bridge_boot <- bootstrap_bridge_hub(
    edges, core_genes = core_genes,
    n_boot = n_boot, verbose = verbose
  )
  bps <- data.frame(
    gene = bridge_boot$gene,
    BPS = bridge_boot$prob_bridge,
    Degree = bridge_boot$Degree,
    stringsAsFactors = FALSE
  )
  bps <- bps[order(-bps$BPS), ]
  rownames(bps) <- NULL
  list(
    phyto_targets = targets,
    edges = edges,
    bridge_bootstrap = bridge_boot,
    BPS = bps
  )
}
