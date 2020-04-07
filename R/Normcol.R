#' Normcol
#' Normcol performs the Normalisation of column the given matrix.
#'
#' @param m  Matrix
#' @return Normalised matrix
#' @export
#' @details For the given matrix of the first column of the all the -1 elements converting +1
#' without alter the property of the matrix.
#' @examples
#' PaleyI(8)
#' #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#' #[1,]    1    1    1    1    1    1    1    1
#' #[2,]   -1    1   -1   -1    1   -1    1    1
#' #[3,]   -1    1    1   -1   -1    1   -1    1
#' #[4,]   -1    1    1    1   -1   -1    1   -1
#' #[5,]   -1   -1    1    1    1   -1   -1    1
#' #[6,]   -1    1   -1    1    1    1   -1   -1
#' #[7,]   -1   -1    1   -1    1    1    1   -1
#' #[8,]   -1   -1   -1    1   -1    1    1    1
#' @examples
#' Normcol(PaleyI(8))
#' #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#' #[1,]    1    1    1    1    1    1    1    1
#' #[2,]    1   -1    1    1   -1    1   -1   -1
#' #[3,]    1   -1   -1    1    1   -1    1   -1
#' #[4,]    1   -1   -1   -1    1    1   -1    1
#' #[5,]    1    1   -1   -1   -1    1    1   -1
#' #[6,]    1   -1    1   -1   -1   -1    1    1
#' #[7,]    1    1   -1    1   -1   -1   -1    1
#' #[8,]    1    1    1   -1    1   -1   -1   -1
#'

Normcol<-function(m){
  nrows<-nrow(m)
  ncols<-ncol(m)
  m1<-matrix(1,nrow=nrows,ncol = ncols)
  for (i in 1:nrows){
    if (m[i,1]==-1){
      for (j in 1:ncols){
        m1[i,j]=-m[i,j]
      }
    }
    else {
      for (j in 1:ncols){
        m1[i,j]=m[i,j]
      }
    }
  }
  return(m1)
}
