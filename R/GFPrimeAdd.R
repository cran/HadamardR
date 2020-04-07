#' GFPrimeAdd
#'
#' GFPrimeAdd creates the addition Table for GF(p), where p is a prime number
#' @param p integer
#' @return Addition Table of GF(p) in the form of matrix of order p x p.
#' @export
#' @details
#' If the elements of GF(p) are 0,1,..,p-1 then the (i,j)th element of matrix
#' returned is addition of (i-1)th and (j-1)th elements. The additions are
#' subjected to modulo p.
#'
#' @examples
#' GFPrimeAdd(5)
#'      #[,1] [,2] [,3] [,4] [,5]
#'#[1,]    0    1    2    3    4
#'#[2,]    1    2    3    4    0
#'#[3,]    2    3    4    0    1
#'#[4,]    3    4    0    1    2
#'#[5,]    4    0    1    2    3
#'
#'
#'
#'

GFPrimeAdd<-function (p){
  if (is.prime(p)) {
    GFAdd=matrix(c(rep(0,p*p)),nrow=p,ncol=p)
    for (i in 1:p){
      for (j in 1:p) {
        GFAdd[i,j]<-(i+j-2)%%p

      }
    }
    return (GFAdd)
  }
  return (FALSE)
}
