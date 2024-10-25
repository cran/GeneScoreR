#' Calculate Z-Scores from Count Tables
#'
#' This function computes a Z-score sum for each sample in the given "scored" count table, based on
#' the means and SDs of the genes in the control table.
#'
#' @importFrom stats sd
#' @param scored_table Data frame of samples to be scored (genes as rows, samples as columns). All columns must be numeric.
#' @param control_table Data frame of control samples (genes as rows, samples as columns). All columns must be numeric.
#' @return A data frame with the sum of Z-scores per sample and the sample IDs.
#' @examples
#' # Example data to be scored
#' scored_table <- data.frame(
#'   sample1 = c(1, 2, 3),
#'   sample2 = c(4, 5, 6),
#'   sample3 = c(7, 8, 9)
#' )
#' rownames(scored_table) <- c("gene1", "gene2", "gene3")
#'
#' # Example control data
#' control_table <- data.frame(
#'   control1 = c(1, 1, 1),
#'   control2 = c(2, 2, 2),
#'   control3 = c(3, 3, 3)
#' )
#' rownames(control_table) <- c("gene1", "gene2", "gene3")
#'
#' # Calculate Z-score for each sample of the scored_table
#' zscore(scored_table, control_table)

#' @export
zscore <- function(scored_table, control_table) {

  # Stop if inputs are not data frames with matching gene names as rows
  if (!is.data.frame(scored_table) || !is.data.frame(control_table)) {
    stop("Both scored_table and control_table should be data frames.")
  }

  if (!all(rownames(scored_table) == rownames(control_table))) {
    stop("The row names in the scored_table and control_table must match.")
  }

  control_means <- apply(control_table, 1, mean)
  control_sds <- apply(control_table, 1, sd)

  standardize_row <- function(row) {
    (row - control_means) / control_sds
  }

  scored_standardized <- t(apply(t(scored_table), 1, standardize_row))

  row_sums <- rowSums(scored_standardized)

  score_frame <- data.frame(`Z-score sum` = row_sums)
  score_frame$ID <- colnames(scored_table)

  return(score_frame)
}
