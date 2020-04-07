#' goethals_seidel_array
#'
#' goethals_seidel_array  performs the construction of Hadamard
#' matrix by Goethals-Seidel method
#'
#' @param A    matrix
#' @param B    matrix
#' @param C    matrix
#' @param D    matrix
#' @return goethals_seidel matrix of order 4n
#' @export
#'
#' @details
#' For this function requrires the four matrices, all the four matrix are Circulant matrices
#' same order.R is an antidiagonal matrix of the same order With which it should satisfy
#' the AA'+ BB'+ CC'+ DD'=4nI, where I is the identity matrix of order n.
#' This function returns matrix of order 4n where n is the order of the given matrices.
#'
#' @references Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero
#' diagnol. Canad. J. Math., 19, 259-264.
#'

goethals_seidel_array<-function(A=NA,B=NA,C=NA,D=NA){
  if (is.matrix(A)==T)    {
    if(is.matrix(B)==T)     {
      if (is.matrix(C)==T)    {
        if (is.matrix(D)==T)      {
          R<-antidiagnol(ncol(A))
          return(rbind(cbind(A, B%*%R, C%*%R, D%*%R),
                       cbind(-B%*%R, A, t(D)%*%R, -t(C)%*%R),
                       cbind(-C%*%R, -t(D)%*%R, A, t(B)%*%R),
                       cbind(-D%*%R, t(C)%*%R, -t(B)%*%R, A)))
        }
      }
    } #A, B, C, D must be a circulant matrix
  }
}
