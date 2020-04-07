#' PaleyIIPrimePower
#'
#' @param order  integer
#' @return
#' Hadamard matrix of the given order.
#' @export
#' @details
#' q=n/2-1, If there is an Hadamard matrix of order h>1, and q = 1 (mod 4) is a prime power,
#' then there exists an Hadamard matrix of order nh.
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#' @examples
#' PaleyIIPrimePower(20)
#' @examples
#' PaleyIIPrimePower(24)



PaleyIIPrimePower <- function(order){
  cardin<-order/2-1
  a<-is.primepower(cardin)
  if(is.null(a)){
    return(NULL)
  }
  p<-a[1]
  r<-a[2]
  Q<-QPrimePower(cardin)
  n<-order/2
  S<-matrix(rep(0,n*n),nrow = n,ncol = n)
  for (j in 2:n){
    S[1,j]=1
    S[j,1]=1
  }
  for (i in 2:n){
    for(j in 2:n)
      S[i,j]=Q[i-1,j-1]
  }
  m1<-matrix(c(1,1,1,-1),nrow=2,ncol=2,byrow = TRUE)
  m2<-matrix(c(1,-1,-1,-1),nrow=2,ncol=2,byrow = TRUE)
  I<-diag(n)
  H<-S%x%m1+I%x%m2
  return(H)
}
