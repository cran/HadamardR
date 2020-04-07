#'check_hadamard
#'
#'check_hadamard tests whether the input matrix  is an Hadamard matrix or not.
#' @param x   matrix
#' @return
#' returns a text message
#' @export
#' @details This function can be used to check whether a given matrix is an Hadamard Matrix or not.
#' To ensure that generated matrix is indeed an Hadamard matrix, this function can be used. In case,
#' if the given matrix is an Hadamard matrix, a text message, Given matrix is an Hadamard Matrix of order is printed on the console.
#'
#' @details This function checks (i)Input is a matrix; (ii)a square matrix; (iii)Order of the matrix is an Hadamard number;
#' (iv) All elements are either +1 or -1; (v) HH'=nI, where n is the order of the input matrix H and H' is transpose of H.
#'
#' @references
#' Hedayat, A. and Wallis, W.D. (1978). Hadamard Matrices and Their Application.Ann. Stat., 6, 1184-1238.
#'
#' @examples
#' #Example 1:
#' h<-matrix(c(1,1,1,-1),nrow=2,ncol=2)
#' check_hadamard(h)
#' # Given matrix is an Hadamard Matrix of order 2
#' #Example 2:
#' h<-matrix(c(1,-1,1,-1),nrow=2,ncol=2)
#' check_hadamard(h)
#' #Not an Hadamard matrix
#' #Example 3:
#' h<-Hadamard_Matrix(36)
#' check_hadamard(h)
#' #"Given matrix is an Hadamard Matrix of order 36"

check_hadamard<-function(x){
  if (is.matrix(x)==FALSE){
    return(sprintf("Input Matrix is required"))
  }
  nr<-nrow(x)
  nc<-nrow(x)
  if (nr!=nc){
    return(sprintf("Not a Square Matrix, number of rows =%d and number of columns %d",nr,nc))
  }
  if ( numbers::mod(nr,4)!=0 & numbers::mod(nr,2)!=0){
    return(sprintf("%d is not a multiple of 4",nr))
  }
  for (i in 1:nr){
    for (j in 1:nr){
      ret<-  ifelse(x[i,j]==1 | x[i,j]==-1,TRUE,FALSE)
      if(ret<-FALSE){
        return(sprintf("Not an Hadamard Matrix, %.0f,%.0f element of H is not 1 or -1:%.0f"),i,j,x[i,j])
      }
    }

  }
  hth<-x%*%t(x)
  diag<-nr*diag(nr)
  for (i in 1:nr){
    for(j in 1:nr){
      if (hth[i,j]!=diag[i,j]){
        return(sprintf("Not an Hadamard Matrix, %.0f,%.0f element of H*t(H) is not correct, %.0f !=%.0f",i,j,hth[i,j],diag[i,j]))
      }
    }
  }

  return(sprintf("Given matrix is an Hadamard Matrix of order %d",nr))
}
