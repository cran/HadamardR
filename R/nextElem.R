#' nextElem
#'
#' nextElem Generate next element of GF.
#' @param p1  integer
#' @param MF integer
#' @param p integer
#' @param r integer
#' @return A vector of order r, the coefficients of elements.
#' @export
#'


nextElem <- function(p1,MF,p,r){
  x<-c(0,1)
  x1<-c(rep(0,r+1))
  for (i in 1:r){
    for (j in 1:2){
      x1[i+j-1]<- x1[i+j-1]+p1[i]*x[j]
    }
  }
  if (x1[r+1]!=0)
    for(j in 1:r){
      x1[j]<-x1[j]+x1[r+1]*MF[j]
    }
  for (j in 1:r){
    if(x1[j]>=p) {
      x1[j] <- x1[j]%%p
    }
  }
  return(x1[1:r])
}
