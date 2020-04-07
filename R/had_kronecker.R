#' had_kronecker
#'
#' had_kronecker performs the construction of an Hadamard matrix by kronecker product.
#'
#' @param n  an integer (Expected to be Hadamard Number)
#' @param exponent  an integer
#' @return Hadamard Matrix of order n, if n is power of 2, otherwise NULL.
#' @export
#' @details This function only applicable when n is the power of 2 and multiple of 4.
#' @details If n<-2, returns Hadamard matrix of order 2; if n is not Hadamard number, return NULL.
#' @details By default exponent=FALSE; when exponent is unknown it is computed.
#' @details If exponent is given use the same
#'
#'
#' @references
#' Hedayat, A. and Wallis, W.D. (1978). Hadamard Matrices and Their Application.Ann. Stat., 6, 1184-1238.
#' @references
#' Sylvester, J.J. (1968). Problem 2511. Math. Questions and solutions, 10, 74.
#'
#' @examples
#' had_kronecker(4)
#'#      [,1] [,2] [,3] [,4]
#'#[1,]    1    1    1    1
#'#[2,]    1   -1    1   -1
#'#[3,]    1    1   -1   -1
#'#[4,]    1   -1   -1    1
#' had_kronecker(8,3)
#' #     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#'#[1,]    1    1    1    1    1    1    1    1
#'#[2,]    1   -1    1   -1    1   -1    1   -1
#'#[3,]    1    1   -1   -1    1    1   -1   -1
#'#[4,]    1   -1   -1    1    1   -1   -1    1
#'#[5,]    1    1    1    1   -1   -1   -1   -1
#'#[6,]    1   -1    1   -1   -1    1   -1    1
#'#[7,]    1    1   -1   -1   -1   -1    1    1
#'#[8,]    1   -1   -1    1   -1    1    1   -1
#' had_kronecker(9)
#'  # NULL


had_kronecker<- function(n,exponent=NULL){
  h2 <- matrix(c(1,1,1,-1),nrow=2,ncol=2)
  if (n==2){
    return(h2)
      }
  if (is.null(exponent)){
    exponent<-pow(n)
    }
  if (is.null(exponent)){
    return(NULL)
  }
  if (n>4){
    while(numbers::mod(n,4)!=0)
      return(NULL)
      }
  if (n<4){
    return(NULL)
    }
  had<-h2
  if (exponent >= 2){
    for( i in 2 : exponent){
      had<- had %x% h2
    }
    return(had)
  }
}


