#' PaleyI
#'
#'This function performs constructing the Hadamard matrix by Paley method.
#'
#' @param n  integer (order of the matrix)
#' @return hadamard matrix of n
#' @export
#' @details
#' let q = n-1 ,  and q = 3 (mod 4), q is the prime number, then obtained the Hadamard
#' matrix of order q+1.if input satisfies these condition it retuns Hadamard Matrix; otherwise
#' returns NULL.
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#'
#' @examples
#' PaleyI(8)
#' #' PaleyI(8)
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
#' PaleyI(16)
#' #NULL


PaleyI<-function(n){
  q  <- (n-1)
  if(numbers::mod(q,4)==3 & is.prime(q)==TRUE){
  A  <- qhad2(q)
  et  <- matrix(c(rep(1,q)),nrow=1,ncol=q)
  e <- matrix(c(rep(-1,q)),nrow=q,ncol=1)
  S1 <- cbind(0,et)
  S2 <- cbind(e,A)
  S  <- rbind(S1,S2)
  H  <- S+diag(n)
  return (H)
  }else
    return(NULL)
}
