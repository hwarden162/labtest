#' Calculate Gene Entropies in scRNA-seq Data
#'
#' Describe how your function works here.
#'
#' @param counts A matrix of count data, with genes as columns.
#'
#' @return A vector of entropies
#' @export
#'
#' @examples
#'
#' mat <- matrix(1:12, ncol = 3)
#' calc_entropy(mat)
#'
calc_entropy <- function(counts) {

  if (missing(counts)) {
    stop("\n \u2716 Counts matrix not supplied")
  }

  norm_counts <- apply(
    counts,
    2,
    \(x){x/sum(x)}
  )

  gene_ents <- apply(
    norm_counts,
    2,
    \(x){entropy::entropy(x)}
  )

  gene_ents
}
