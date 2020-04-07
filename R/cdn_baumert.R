#' cdn_baumert
#'
#' @description
#' Checks Hadamard Matrix can be constructed using Baumert-Hall arrays of order 12.
#' @param order  integer, order of Hadamard matrix to be checked.
#' @return 6 or NULL
#' @export
#' @details Baumert-Hall array is a generalization of Williamson Array.
#' In case, Williamson matrices are available for order/12, the method
#' return 6 otherwise it returns NULL.
#' @details The availabile Williamson sequences in the internal data sets is
#' seq(1,63, 2) except 15, 35, 47, 53, 59 in the internal table.
#' @seealso
#' \code{\link{had_baumert}} for Baumert-Hall's construction method.
#' @examples
#' cdn_baumert(36)
#' #6
#' @examples
#' cdn_baumert(72)
#' #NULL
#' @references
#' Hedayat, A. and Wallis, W. D.(1978).  Hadamard Matrices and Their Applications. Ann. Stat. 6: 1184-1238.
cdn_baumert<-function(order){
  x<-order/12
  n<-unique(williamson_sequences$WOrder)
  if(x %in% n)
    return(ret_value=6)
  else
    return(NULL)
}
