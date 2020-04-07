#' seq_williamson
#'
#' seq_williamson performs the selection of Williamson sequences from dataset
#'
#'
#' @param order integer
#' @return Required Williamson sequences of order
#' @export
#' @details Create williamson sequences of given order from the internal dataset williamson_sequences
#' @details Williamson sequences are available for length of seq(1,63, 2) except 15, 35, 47, 53, 59 in the internal table.
#' @seealso
#' \code{\link{had_williamson}} for Williamson construction method using Williamson sequences.
#' @source
#' The Base sequences are obtained The Base sequences were obtained from
#' \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#' and London (2013).
#'
#' @references Williamson, J. (1944). Hadamard determinant theorem and the sum of four squares. Duke. Math. J., 11, 65-81.
#' @references Williamson, J. (1947). Note on Hadamard's determnant theorem. Bull. Amer. Math. Soc., 53, 608-613.
#' @references
#' London, S. 2013. Constructing New Turyn Type Sequences, T-Sequences and Hadamard Matrices. PhD Thesis, University of Illinois at Chicago, Chicago.

seq_williamson <- function(order){
  dat <- subset(williamson_sequences,williamson_sequences$WOrder ==order)
  return(dat)
}
