#' PaleyIPrimePower
#'
#' @param n  integer
#' @return
#' Hadamard matrix
#' @export
#' @details
#' let q = n-1 ,  and q = 3 (mod 4), q is the prime power, then obtained the Hadamard
#' matrix of order q+1.if input satisfies these condition it retuns Hadamard Matrix; otherwise
#' returns NULL.
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#' @examples
#' PaleyIPrimePower(28)
#' @examples
#' PaleyIPrimePower(28)
#' #NULL


PaleyIPrimePower <- function(n){
  cardin<-n-1
  d<-is.primepower(cardin)
  if(is.null(d)){
    return(NULL)
  }
  p<-d[1]
  r<-d[2]
  Q<-QPrimePower(cardin)
  S<-matrix(rep(0,n*n),nrow = n,ncol = n)
  for (j in 2:n){
    S[1,j]=1
    S[j,1]=-1
  }
  for (i in 2:n){
    for(j in 2:n)
      S[i,j]=Q[i-1,j-1]
  }
  I<-diag(n)
  H<-S+I
  return(H)
}
