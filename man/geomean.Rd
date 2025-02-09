% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geomean_calculator.R
\name{geomean}
\alias{geomean}
\title{Calculate Geometric Means from Count Tables}
\usage{
geomean(count_table)
}
\arguments{
\item{count_table}{A data frame of gene count data (genes as rows, samples as columns). All columns must be numeric.}
}
\value{
A data frame with the geometric means per sample and the sample IDs.
}
\description{
This function computes the geometric mean for each sample in the given count table.
}
\examples{
# Example data to be scored
count_table <- data.frame(
  sample1 = c(1, 10, 100),
  sample2 = c(2, 20, 200),
  sample3 = c(3, 30, 300)
)
rownames(count_table) <- c("gene1", "gene2", "gene3")

# Calculate Geometric Mean per sample in the count_table
geomean(count_table)
}
