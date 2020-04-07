#' cdn_miyamoto
#'
#' Checks Hadamard Matrix can be constructed using Ehlich's method.
#'
#' @param order  integer
#' @return 9 or NULL
#' @export
#'
#' @details In Miyamoto construction, if q= n/4 and q is a prime or prime power
#' and q=1 (mod 4),then there exists an Hadamard Matrix of order n.
#'
#' @seealso
#' \code{\link{had_miyamoto}} for Miyamoto's construction method.
#' @references
#' Miyamoto, M. (1991). A Construction of Hadamard matrices. J. Math. Phy., 12, 311-320.
#' @examples
#' cdn_miyamoto(20)
#' #q=5, is a prime number and q=1(mod 4).
#' #9
#' cdn_miyamoto(16)
#' #NULL


cdn_miyamoto<-function(order){
  q<-order/4
  if(numbers::mod(q,4)!=1){
    return(NULL)
  }
  if(is.prime(q)==F){
    return(NULL)
  }
  if(is.integer(Had_method(q-1)==F)){
    return(NULL)
  }
  else
    return(ret_value=9)
}
