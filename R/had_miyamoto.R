#' had_miyamoto
#'
#' had_miyamoto function perform the construction of the Hadamard matrix by using the
#' Miyamoto method
#'
#' @param n  integer (order of the matrix)
#' @return Hadamard matrix of n
#' @export
#' @details
#' If the q=n/4, and q be a prime power and q=1 (mod 4). If there is a exists of
#' Hadamard matrix of order q-1, then there exists an Hadamard matrix of order 4q.
#' If given order is not satisfied it returns NULL.
#'
#' @references
#' Miyamoto, M. (1991). A Construction of Hadamard matrices. J. Math. Phy., 12, 311-320.
#'
#' @examples
#' had_miyamoto(20)
#' @examples
#' had_miyamoto(24) #NULL
#'
had_miyamoto<-function(n){
  p<-n/4
  m<- (p-1)/2
  A<-miyamotoC(n)
  if(is.null(A)){
    return(NULL)
  }
  C1<- -A[2:(m+1),2:(m+1)]
  C2<- A[2:(m+1),(m+2):p]
  C2t<- C2
  C4<- A[(m+2):p,(m+2):p]

  K<-Hadamard_Matrix(p-1)
  if(is.matrix(K)==F){
    return(NULL)
  }
  K1<- K[1:((p-1)/2),1:((p-1)/2)]
  K2<- K[1:((p-1)/2),(((p-1)/2)+1):(p-1)]
  K3<- -K[(((p-1)/2)+1):(p-1), 1:((p-1)/2)]
  K4<- K[(((p-1)/2)+1):(p-1), (((p-1)/2)+1):(p-1)]

  zero <-matrix(rep(0,m*m),nrow=m,ncol=m)
  u1<- cbind(C1,C2,zero,zero)
  u2<-cbind(-C2t,C4,zero,zero)
  u3<-cbind(zero,zero,C1,C2)
  u4<- cbind(zero,zero,-C2t,C4)
  U<-rbind(u1,u2,u3,u4)
  I<-diag(m)
  v1<- cbind(I,zero,K1,K2)
  v2<- cbind(zero,I,K3,-K4)
  v3<- cbind(-t(K1),-t(K3), I,zero)
  v4<- cbind(-t(K2),t(K4),zero,I)
  V<-rbind(v1,v2,v3,v4)
  s1<- matrix(rep(1),nrow = 2,ncol = 2)
  s2<- matrix(c(1,-1,-1,1),nrow = 2,ncol = 2)

  T11<- (C1 %x% s1) + (I %x% s2)
  T12<- (C2 %x% s1) + (zero %x% s2)
  T13<- (zero %x% s1) +  ( K1 %x% s2)
  T14<- (zero %x% s1) +  ( K2 %x% s2)
  T21<- (t(C2) %x% s1) +  ( zero %x% s2)
  T22<- (C4 %x% s1) +  ( I %x% s2)
  T23<- (zero %x% s1) +  ( K3 %x% s2)
  T24<- (zero %x% s1) +  ( K4 %x% s2)
  T31<- (zero %x% s1) +  ( t(K1) %x% s2)
  T32<-(zero %x% s1) +  ( t(K3) %x% s2)
  T33<-(C1 %x% s1) +  ( I %x% s2)
  T34<- (C2 %x% s1) +  ( zero %x% s2)
  T41<- (zero %x% s1) +  ( t(K2) %x% s2)
  T42<- (zero %x% s1) +  ( t(K4) %x% s2)
  T43<- (t(C2) %x% s1) +  ( zero %x% s2)
  T44<-(C4 %x% s1) +  ( I %x% s2)

  et<-matrix(rep(1),ncol = 2*m,nrow = 1)
  e<-matrix(rep(1),ncol = 1,nrow = 2*m)

  a1<-cbind(1,et)
  a12<- cbind(e,T12)
  X12<-rbind(a1,a12)

  a13<- cbind(e,T13)
  X13<-rbind(a1,a13)

  a14<- cbind(e,T14)
  X14<-rbind(a1,a14)

  a21<- cbind(e,T21)
  X21<-rbind(a1,a21)

  a23<- cbind(e,T23)
  X23<-rbind(a1,a23)

  a24<- cbind(e,T24)
  X24<-rbind(a1,a24)

  a31<- cbind(e,T31)
  X31<-rbind(a1,a31)

  a32<- cbind(e,T32)
  X32<-rbind(a1,a32)

  a34<- cbind(e,T34)
  X34<-rbind(a1,a34)

  a41<- cbind(e,T41)
  X41<-rbind(a1,a41)

  a42<- cbind(e,T42)
  X42<-rbind(a1,a42)

  a43<- cbind(e,T43)
  X43<-rbind(a1,a43)

  b1<- cbind(1,-et)
  b11<- cbind(-e,T11)
  X11<-rbind(b1,b11)

  b22<- cbind(-e,T22)
  X22<-rbind(b1,b22)

  b33<- cbind(-e,T33)
  X33<-rbind(b1,b33)

  b44<- cbind(-e,T44)
  X44<-rbind(b1,b44)


  h1<- cbind(X11,X12,X13,X14)
  h2<- cbind(-X21,X22,X23,-X24)
  h3<- cbind(-X31,-X32,X33,X34)
  h4<- cbind(-X41,X42,-X43,X44)
  H<-rbind(h1,h2,h3,h4)
  return(H)

}
