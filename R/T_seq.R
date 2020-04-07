#' T_seq
#' T_seq performs the selection of the T sequences from dataset.internal function not exported.
#'
#' @param order integer
#' @return Required Turyn sequences of order of x
#' @details Create T sequences of given order from the internal dataset T_sequences
#' @details T-sequences are available for length of seq(1,73,2) and 83, 101 and 107 in the internal table.
#' @seealso
#' \code{\link{had_goethals_T}} for Goethals-Seidel construction method using T-sequences.
#'
#' @source
#' The Turyn sequences were obtained from \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}.
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#'


T_seq<-function(order){
  dat <- subset(T_sequences, T_sequences $Order==order)
  return(dat)
}
