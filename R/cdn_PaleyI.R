#' cdn_PaleyI
#' Checks Hadamard Matrix can be constructed using Paley I method.
#'
#' @param order  integer
#' @return 2 or NULL
#' @export
#'
#' @details
#' In Paley I method,  if q=order-1 and q is prime number and q=3 (mod 4) then the function retuns 2 otherwise NULL.
#'
#' @seealso
#' \code{\link{PaleyI}} for Paley I construction method.
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#' @examples
#' cdn_PaleyI(8)
#' #2
#' @examples
#' cdn_PaleyI(16)
#' #NULL

cdn_PaleyI<-function(order){
  q  <- (order-1)
  if(numbers::mod(q,4)==3 & is.prime(q)==TRUE){
    return(ret_value=2)
  }else
    return(NULL)
}
