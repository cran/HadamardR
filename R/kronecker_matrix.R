#' kronecker_matrix
#'
#' @param n  integer (order of the matrix)
#' @return Hadamard matrix of order "n"
#' @export
#'
#' @details
#' This function construct Hadamard matrix by multiple of 2 Hadamard matrix.
#' It Returns the Hadamard Matrix, if it is not possible NULL is returned.
#'
#' @references
#' Sylvester, J.J. (1967). Thoughts on orthogonal matrices, simultaneous sign-succession
#' and Tessellated pavements in two or more colours, with applications to Newton's rule,
#' ornamental Tie-work, and the theory of numbers. Phil. Mag.,34, 461-475.
#' @references
#' Sylvester, J.J. (1968). Problem 2511. Math. Questions and solutions, 10, 74.
#' @references
#' Hedayat, A. and Wallis, W.D. (1978). Hadamard Matrices and Their Application.Ann. Stat., 6, 1184-1238.
#'
#' @examples
#' kronecker_matrix(8)
#' #      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#' #[1,]    1    1    1    1    1    1    1    1
#' #[2,]    1   -1    1   -1    1   -1    1   -1
#' #[3,]    1    1   -1   -1    1    1   -1   -1
#' #[4,]    1   -1   -1    1    1   -1   -1    1
#' #[5,]    1    1    1    1   -1   -1   -1   -1
#' #[6,]    1   -1    1   -1   -1    1   -1    1
#' #[7,]    1    1   -1   -1   -1   -1    1    1
#' #[8,]    1   -1   -1    1   -1    1    1   -1
#' @examples
#' kronecker_matrix(12)
#' #NULL


kronecker_matrix<-function(n){
  s1<-kro_method(n)
  if(s1==0){
    return(NULL)
  }
    if(s1!=0){
      k1<-Hadamard_Matrix(s1)
      s2<-n/s1
      k2<-Hadamard_Matrix(s2)
    }
    return(k1%x%k2)
  }

