#'  circulant_mat
#'
#'  circulant_mat performs construction of circulant matrices.
#'
#'
#' @param x  a vector to be used as intial row.
#' @return circulant matrix of order length of input vector.
#' @export
#' @description A matrix is said to be circulant if (i+1, j+1)th entry is equal to the
#' (i, j)th entry. Thus, for such matrices, the initial row determines the complex matrix. Whenever i+1,j+1 exceeds the order,
#' modulus operation is carried out.
#'
#' @references
#' Hedayat, A. and Wallis, W.D. (1978). Hadamard Matrices and Their Application.Ann. Stat., 6, 1184-1238.
#'
#' @examples
#' circulant_mat(c(1,1,-1,0))
#' #      [,1] [,2] [,3] [,4]
#' #[1,]    1    1   -1    0
#' #[2,]    0    1    1   -1
#' #[3,]   -1    0    1    1
#' #[4,]    1   -1    0    1
#' @examples
#' circulant_mat(c(5,9,-7,-2))
#' #      [,1] [,2] [,3] [,4]
#' #[1,]    5    9   -7   -2
#' #[2,]   -2    5    9   -7
#' #[3,]   -7   -2    5    9
#' #[4,]    9   -7   -2    5


circulant_mat<- function(x=NA){
  x<- as.vector(x)
  n <- length(x)
  mat <- matrix(x, n, n)
  for (i in 1:n) {
    mat[i, ] <- c(x[-(1:(n+1-i))], x[1:(n+1-i)])
  }
  return(mat)
}
