#' Normrow
#' Normcol performs the Normalisation of row the given matrix.
#' @param m  Matrix
#' @return Normalised matrix
#' @export
#' @details For the given matrix of the first row of the all the -1 elements converting +1
#' without alter the property of the matrix.
#' @examples
#' PaleyII(12)
#'#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#'# [1,]    1    1    1    1    1    1    1   -1   -1    -1    -1    -1
#'# [2,]    1    1    1   -1   -1    1   -1    1   -1     1     1    -1
#'# [3,]    1    1    1    1   -1   -1   -1   -1    1    -1     1     1
#'# [4,]    1   -1    1    1    1   -1   -1    1   -1     1    -1     1
#'# [5,]    1   -1   -1    1    1    1   -1    1    1    -1     1    -1
#'# [6,]    1    1   -1   -1    1    1   -1   -1    1     1    -1     1
#'# [7,]    1   -1   -1   -1   -1   -1   -1   -1   -1    -1    -1    -1
#'# [8,]   -1    1   -1    1    1   -1   -1   -1   -1     1     1    -1
#'# [9,]   -1   -1    1   -1    1    1   -1   -1   -1    -1     1     1
#'#[10,]   -1    1   -1    1   -1    1   -1    1   -1    -1    -1     1
#'#[11,]   -1    1    1   -1    1   -1   -1    1    1    -1    -1    -1
#'#[12,]   -1   -1    1    1   -1    1   -1   -1    1     1    -1    -1
#' @examples
#' Normrow(PaleyII(12))
#'#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#'# [1,]    1    1    1    1    1    1    1    1    1     1     1     1
#'# [2,]    1    1    1   -1   -1    1   -1   -1    1    -1    -1     1
#'# [3,]    1    1    1    1   -1   -1   -1    1   -1     1    -1    -1
#'# [4,]    1   -1    1    1    1   -1   -1   -1    1    -1     1    -1
#'# [5,]    1   -1   -1    1    1    1   -1   -1   -1     1    -1     1
#'# [6,]    1    1   -1   -1    1    1   -1    1   -1    -1     1    -1
#'# [7,]    1   -1   -1   -1   -1   -1   -1    1    1     1     1     1
#'# [8,]   -1    1   -1    1    1   -1   -1    1    1    -1    -1     1
#'# [9,]   -1   -1    1   -1    1    1   -1    1    1     1    -1    -1
#'#[10,]   -1    1   -1    1   -1    1   -1   -1    1     1     1    -1
#'#[11,]   -1    1    1   -1    1   -1   -1   -1   -1     1     1     1
#'#[12,]   -1   -1    1    1   -1    1   -1    1   -1    -1     1     1

Normrow<-function(m){
  nrows<-nrow(m)
  ncols<-ncol(m)
  m1<-matrix(1,nrow=nrows,ncol = ncols)
  for (j in 1:ncols){
    if (m[1,j]==-1){
      for (i in 1:nrows){
        m1[i,j]=-m[i,j]
      }
    }
    else {
      for (i in 1:nrows){
        m1[i,j]=m[i,j]
      }
    }
  }
  return(m1)
}
