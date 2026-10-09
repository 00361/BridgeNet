#' Build bridge PPI network
#'
#' @param targets Character vector of drug targets.
#' @param core_genes Character vector of disease core genes.
#' @param source "STRING" or "custom".
#' @param score Integer. STRING score threshold.
#' @param species Integer. NCBI taxonomy ID.
#' @param custom_edges Data frame (if source = "custom").
#' @param verbose Logical.
#' @return Standardized edge list (node1, node2, score).
#' @export
build_bridge <- function(targets, core_genes, source = "STRING",
                         score = 400, species = 9606,
                         custom_edges = NULL, verbose = TRUE) {
  if (source == "custom") {
    if (is.null(custom_edges)) stop("Please provide custom_edges.")
    if (verbose) message("[BridgeNet] Using custom edge list...")
    edges <- .standardize_edges(custom_edges)
  } else {
    all_genes <- unique(c(targets, core_genes))
    if (length(all_genes) < 2) stop("Need at least 2 genes.")
    if (verbose) message("[BridgeNet] Querying STRING for ",
                         length(all_genes), " genes...")
    gene_string <- paste(all_genes, collapse = "%0d")
    url <- paste0("https://string-db.org/api/tsv/network?identifiers=",
                  gene_string, "&species=", species,
                  "&required_score=", score)
    res <- httr::GET(url)
    if (httr::status_code(res) != 200) {
      stop("STRING API failed: ", httr::status_code(res))
    }
    raw <- utils::read.delim(text = httr::content(res, "text"), header = TRUE)
    if (nrow(raw) == 0) stop("No interactions found.")
    edges <- .standardize_edges(raw)
    if (verbose) message("[BridgeNet] Retrieved ", nrow(edges),
                         " interactions.")
  }
  edges
}
