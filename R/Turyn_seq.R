#' Turyn_seq
#' Turyn_seq performs the selection of the Turyn sequences from dataset.
#' It is internal function not exported.
#' @param order integer
#' @return Required Turyn sequences of order of x
#' @details Create Turyn sequences of given order from the internal dataset T_sequences
#' @details
#' Turyn type-sequences are available for 28,30,34,36 in the internal table.
#' @seealso
#' \code{\link{had_goethals_Turyn}} for Goethals-Seidel construction method using Turyn sequences.
#'
#' #'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#'


Turyn_seq <- function(order){
  dat <- subset(Turyn_sequences,Turyn_sequences$Turynorder ==order)
  return(dat)
}
