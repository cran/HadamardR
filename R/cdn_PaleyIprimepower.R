#' cdn_PaleyIprimepower
#' checks Hadamard Matrix can be constructed using Paley I method.
#'
#' @param order  integer
#' @return 14 or NULL
#' @export
#'
#' @details
#' In Paley I method,  If q=order-1 and q is prime power and q=3 (mod 4) then
#' it retuns 14 otherwise NULL.
#'
#' @seealso
#' \code{\link{PaleyIPrimePower}} for Paley I construction method.
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#' @examples
#' cdn_PaleyI(28)
#' #14
#' @examples
#' cdn_PaleyI(16)
#' #NULL

cdn_PaleyIprimepower<-function(order){
  if(numbers::mod(order,4)!=0){
    return(NULL)
  }
  q<- order-1
  m<- is.primepower(q)
  p<- m[1]
  r<- m[2]
  if(is.null(p)==TRUE){
    return(NULL)
  }else
    return(ret_value=14)
}
