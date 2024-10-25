#' Calculate Geometric Means from Count Tables
#'
#' This function computes the geometric mean for each sample in the given count table.
#'
#' @param count_table A data frame of gene count data (genes as rows, samples as columns). All columns must be numeric.
#' @return A data frame with the geometric means per sample and the sample IDs.
#' @examples
#' # Example data to be scored
#' count_table <- data.frame(
#'   sample1 = c(1, 10, 100),
#'   sample2 = c(2, 20, 200),
#'   sample3 = c(3, 30, 300)
#' )
#' rownames(count_table) <- c("gene1", "gene2", "gene3")
#'
#' # Calculate Geometric Mean per sample in the count_table
#' geomean(count_table)
#' @export
geomean <- function(count_table) {

  # Stop if input is not a data frame
  if (!is.data.frame(count_table)) {
    stop("count_table should be a data frame.")
  }

  geometric_mean <- function(x) {
    exp(mean(log(x[x > 0])))
  }

  geomeans <- apply(count_table, 2, geometric_mean)

  score_frame <- data.frame(`Geometric Mean` = geomeans)
  score_frame$ID <- colnames(count_table)

  return(score_frame)
}
