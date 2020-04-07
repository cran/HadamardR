#' cdn_goethals_T
#'
#' @description
#' Checks Hadamard Matrix can be constructed using available T-sequences.
#' @param order  integer
#' @return 13 or NULL
#' @export
#' @details This function checks whether the Hadamard matrix of given order can be constructed using T sequences.
#' If T sequences of length n,n,n,n are available, Hadamard matrix of order 4n can be constructed.
#' Returns the value 13, if it is possible otherwise NULL is returned.
#' @details T-sequences are available for length of seq(1,73,2) and for 83, 101 and 107 in the internal table.
#' @seealso
#' \code{\link{had_goethals_T}} for Goethals-Seidel construction method using T-sequences.
#'
#' @examples
#' cdn_goethals_T(28)
#' #T-seqeunce of length 7 exists.
#' #13
#' @examples
#' cdn_goethals_T(24)
#' #T-sequence of length 6 does not exist.
#' #NULL

cdn_goethals_T<-function(order){
  x<-order/4
  n<-unique(T_sequences$Order)
  if(x %in% n)
    return(ret_value=13)
  else
    return(NULL)
}
