#' cdn_williamson
#'
#' @description
#' Checks Hadamard Matrix can be constructed using available Williamson sequences.
#' @param order  integer
#' @return 5 or NULL
#' @export
#' @details This function checks whether the Hadamard matrix of given order can be constructed using williamson sequences.
#' If Williamson sequences of length n,n,n,n are available, Hadamard matrix of order 4n can be constructed.
#' Returns the value 5, if it is possible otherwise NULL is returned.
#' @details Williamson sequences are available for length of seq(1,63, 2) except 15, 35, 47, 53, 59 in the internal table.
#' @seealso
#' \code{\link{had_williamson}} for Williamson construction method using Williamson sequences.
#' @source
#' The Williamson sequences were obtained from \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos} and London (2013).
#'
#' @examples
#' cdn_williamson(20)
#' #5
#' @examples
#' cdn_goethals_T(24)
#' #NULL
#'
#' @references Williamson, J. (1944). Hadamard determinant theorem and the sum of four squares. Duke. Math. J., 11, 65-81.
#' @references Williamson, J. (1947). Note on Hadamard's determnant theorem. Bull. Amer. Math. Soc., 53, 608-613.
#' @references
#' London, S. 2013. Constructing New Turyn Type Sequences, T-Sequences and Hadamard Matrices. PhD Thesis, University of Illinois at Chicago, Chicago.
cdn_williamson<-function(order){
  x<-order/4
  n<-unique(williamson_sequences$WOrder)
  if(x %in% n)
    return(ret_value=5)
  else
    return(NULL)
}
