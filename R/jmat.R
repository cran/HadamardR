#' Jmat
#'
#' Jmat peforms the generation of unit matrix.
#'
#' @param n  integer
#' @return  square matrix of order n
#' @export
#'
#' @details
#' An J matrix is a square matrix where all the entries are one.
#'
#' @examples
#' Jmat(4)
#'#      [,1] [,2] [,3] [,4]
#'#[1,]    1    1    1    1
#'#[2,]    1    1    1    1
#'#[3,]    1    1    1    1
#'#[4,]    1    1    1    1

Jmat<-function(n){
  jm<-matrix(c(rep(1,n)),nrow=n,ncol=n)
  return(jm)
}
