#' ply1
#'
#' ply1 -internal function; not exported.
#'
#' @param q  integer
#' @return Hadamard matrix of order 2(q+1)
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.
#'

ply1<-function(q){
  A<- qhad2(q)
  et  <- matrix(c(rep(1,q)),nrow=1,ncol=q)
  e <- matrix(c(rep(1,q)),nrow=q,ncol=1)
  S1 <- cbind(0,et)
  S2 <- cbind(e,A)
  S  <- rbind(S1,S2)
  a<-matrix(c(0,-1,1,0),ncol = 2,nrow = 2)
  h<-2
  Id<-diag(h/2)
  Uh<- Id %x% a
  Ah<-had_kronecker(h)
  Bh<- Uh %*%Ah
  H1<- Ah %x% diag(q+1)
  H2<- Bh %x% S
  H<- H1+H2
  return(H)
}
