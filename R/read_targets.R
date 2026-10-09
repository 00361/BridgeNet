#' Read drug targets from multiple sources
#'
#' @param input Character path to CSV, character vector, or data frame.
#' @param format One of "auto", "swisstarget", "simple".
#' @param prob_cutoff Numeric. Probability cutoff.
#' @param gene_col Character. Gene column name (for data frame).
#' @return A character vector of gene symbols.
#' @export
read_targets <- function(input, format = "auto", prob_cutoff = 0,
                         gene_col = NULL) {
  if (is.data.frame(input)) {
    if (is.null(gene_col)) {
      candidates <- c("Common_name", "Common.name", "Target", "gene", "symbol")
      gene_col <- candidates[candidates %in% colnames(input)][1]
      if (is.na(gene_col)) stop("Cannot identify gene column.")
    }
    genes <- trimws(input[[gene_col]])
  } else if (is.character(input) && length(input) == 1 && file.exists(input)) {
    df <- utils::read.csv(input, stringsAsFactors = FALSE)
    if (format == "auto" && ncol(df) == 7) format <- "swisstarget"
    if (format == "swisstarget") {
      colnames(df) <- c("Target", "Common_name", "Uniprot_ID", "ChEMBL_ID",
                        "Target_Class", "Probability", "Known_actives")
      df <- df[df$Probability > prob_cutoff, ]
      genes <- trimws(df$Common_name)
    } else {
      genes <- trimws(df[[1]])
    }
  } else if (is.character(input)) {
    genes <- trimws(input)
  } else {
    stop("Unsupported input type.")
  }
  genes <- genes[!is.na(genes) & genes != "" & genes != "N/A"]
  unique(genes)
}
