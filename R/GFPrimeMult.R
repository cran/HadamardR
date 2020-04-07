#' GFPrimeMult
#' GFPrimeMult creates Multiplication Table for GF(p), where p is a prime number
#' @param p integer
#' @return Multiplication Table of GF(p) in the form of matrix of order p x p.
#' @export
#' @details
#' If the elements of GF(p) are 0,1,..,p-1 then the (i,j)th element of matrix
#'  returned is multiplication of (i-1)th and (j-1)th elements. The multiplications
#'  are subjected to modulo p.
#' @examples
#' GFPrimeMult(5)
#'      #[,1] [,2] [,3] [,4] [,5]
#'#[1,]    0    0    0    0    0
#'#[2,]    0    1    2    3    4
#'#[3,]    0    2    4    1    3
#'#[4,]    0    3    1    4    2
#'#[5,]    0    4    3    2    1

GFPrimeMult<-function (p){
  if (is.prime(p)) {
    GFMult=matrix(c(rep(0,p*p)),nrow=p,ncol=p)
    for (i in 1:p){
      for (j in 1:p) {
        GFMult[i,j]<- ((i-1)*(j-1))%%p

      }
    }
    return (GFMult)
  }
  return (FALSE)
}
