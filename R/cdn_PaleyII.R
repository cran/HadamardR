#' cdn_PaleyII
#' Checks Hadamard Matrix can be constructed using Paley II method.
#'
#' @param order  integer
#' @return 3 or NULL
#' @export
#'
#' @details
#' In Paley II method,  If q=order/2-1 or q=order/4-1 and q is prime number and
#' q=1 (mod 4) then this function retuns 3 otherwise NULL.
#'
#' @seealso
#' \code{\link{PaleyII}} for Paley II construction method.
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#' @examples
#' cdn_PaleyII(24)
#' #3
#' @examples
#' cdn_PaleyII(16)
#' #NULL
cdn_PaleyII<-function(order){
  q<- (order/2)-1
  if(is.prime(q)==T & numbers::mod(q,4)==1 ){
    return(ret_value=3)
  }
  q<- (order/4)-1
  if(is.prime(q)==T & numbers::mod(q,4)==1 ){
    return(ret_value=3)
  }else
    return(NULL)
}
