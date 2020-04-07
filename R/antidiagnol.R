#' antidiagnol
#'
#' @description
#' antidiagnol performs the creation of Back diagnol matrix.
#' @param n  integer
#' @return Antidiagnol matrix of order n.
#' @export
#'
#' @details An anti-diagonal matrix is a square matrix where all the entries are
#' zero except those on the diagonal going from the lower left corner to the upper right
#' corner entries are equal to 1.
#' @details In the first row, the last column will be 1 and all other entries are 0.
#' @details In second row, last but one column is 1 and others are 0 and so on.
#'
#'
#' @examples
#' antidiagnol(4)
#' #0    0    0    1
#' #0    0    1    0
#' #0    1    0    0
#' #1    0    0    0


antidiagnol <- function (n){
  r<-matrix(rep(0,n*n),nrow=n,ncol=n)
  for (i in 1:n){
    for (j in 1:n){
      if (i+j==n+1)
        r[i,j]=1
    }
  }
  return (r)
}
