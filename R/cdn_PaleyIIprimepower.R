#' cdn_PaleyIIprimepower
#' checks Hadamard Matrix can be constructed using Paley II  method.
#'
#' @param order  integer
#' @return 15 or NULL
#' @export
#'
#' @details
#' In Paley II method,  q=order/2-1 and q is prime power and q=1 (mod 4) then
#' it retuns 15 otherwise NULL.
#'
#' @seealso
#' \code{\link{PaleyIIPrimePower}} for Paley construction method.
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#' @examples
#' cdn_PaleyIIprimepower(340)
#' #15
#' @examples
#' cdn_PaleyIIprimepower(64)
#' #NULL


cdn_PaleyIIprimepower<-function(order){
  if(numbers::mod(order,4)!=0){
    return(NULL)
  }
  cardin<-order/2-1
  m<- is.primepower(cardin)
  p<- m[1]
  r<- m[2]
  if(is.null(m)){
    return(NULL)
  }else
    return(ret_value=15)
}
