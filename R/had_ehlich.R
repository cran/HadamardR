#' had_ehlich
#'
#' had_ehlich performs the construction of Hadamard matrix by Ehlich method
#' @param x  Integer (order of the Hadamard matrix)
#' @return Hadamard matrix of order x
#' @export
#' @details
#' Ehlich (1965)'s construction method requires order of the Hadamard matrix must
#' be a of the form (n-1)^2. Conditions are (i)Order=(n-1)^2; (ii) n is a prime
#' or prime power and 3(mod 4); (iii) (n-2) must be a prime or prime power. In
#' case, if all the three conditions are satisfied,then function will return Hadamard
#' matrix of order x otherwise NULL.
#'
#' @references
#' Ehlich, H. (1965). Neue Hadamard-matrizen. Arch. Math., 16, 34-36.
#'
#' @examples
#' had_ehlich(36)
#' @examples
#' had_ehlich(20)
#' #NULL


had_ehlich<- function(x){
  if(sqrt(x)%%1!=0){
    return(NULL)
  }
  n<- sqrt(x)+1
  m<- n-2
  if(numbers::mod(n,4)!= 3)
    if(is.prime(n)!=TRUE){
      return(NULL)
    }
  if (numbers::mod(m,4)!=1){
    return(NULL)
  }
  if (is.prime(m)){
    A<-qhad2(m)
  }
  else {
    tem<-is.primepower(m)
    cardin<-m
    p<-tem[1]
    r<-tem[2]
    A<-QPrimePower(cardin)
  }
  if (is.prime(n)){
    G<-qhad2(n)
  }
  else {
    tem1<-is.primepower(n)
    cardin<-n
    p<-tem1[1]
    r<-tem1[2]
    G<-QPrimePower(cardin)
  }
  r1<-A%x%G
  r2<-diag(m)%x% (diag(n)-Jmat(n))
  r3<-Jmat(m)%x%diag(n)
  K<-r1+r2+r3
  et<-matrix(c(rep(1,m*n)),nrow=1,ncol=m*n)
  e<-matrix(c(rep(1,m*n)),nrow=m*n,ncol=1)
  c1<-cbind(1,et)
  c2<-cbind(e,K)
  H<-rbind(c1,c2)
  return(H)
}
