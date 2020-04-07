#' cdn_goethals_Turyn
#'
#' @description
#' Checks Hadamard Matrix can be constructed using available Turyn Type sequences.
#' @param order  integer
#' @return 8 or NULL
#' @export
#' @details This function checks whether the Hadamard matrix of given order can be constructed using Turyn sequences.
#' If Turyn sequences of (order+4)/12 is available then Hadamard matrix of order exists.
#' Returns the value 8, if it is possible otherwise NULL is returned.
#' @details Turyn type-sequences are available for 28,30,34,36 in the internal table.
#' @seealso
#' \code{\link{had_goethals_Turyn}} for Goethals-Seidel construction method using Turyn sequences.
#'
#' @examples cdn_goethals_Turyn(356)
#' #8
#' @examples cdn_goethals_Turyn(40)
#' #NULL
cdn_goethals_Turyn<-function(order){
  x<-order/4
  m<-(x+1)/3
  n<-unique(Turyn_sequences$Turynorder)
  if(m %in% n)
    return(ret_value=8)
  else
    return(NULL)
}
