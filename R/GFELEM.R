#' GFELEM
#' @description
#' Elements of Galois Field, GF(P^r)
#' @param p integer (a prime number)
#' @param r integer (a positive integer)
#' @param MF Integer Array containing Minimum function
#' @details
#' This function returns Elements of Galois field of order p^r. To use this function, Minimum function is required.
#' Minimum functions are available in internal dataset. To use the Minimum function from the internal, use HadamardR:::
#' @return A vector of size p^r
#' @export
#' @examples
#' library(HadamardR)
#' p<-3
#' r<-2
#' cardin=9
#' mf<-subset(HadamardR:::minimumfunction,HadamardR:::minimumfunction$s==cardin)
#' MF<-mf$coeff
#' GFElem<-GFELEM(p,r,MF)
#' GFElem
##       [,1] [,2]
##  [1,]    0    0 – Identity Element
##  [2,]    0    1 - x
##  [3,]    1    2 – 2x+1
##  [4,]    2    2 – 2x+2
##  [5,]    2    0 - 2
##  [6,]    0    2 – 2x
##  [7,]    2    1 – x+2
##  [8,]    1    1 – x+1
##  [9,]    1    0 - 1



GFELEM <- function(p, r, MF){
  cardin<- p^r
  GFElem<-matrix(c(rep(0,cardin*r)),nrow=cardin,ncol=r)
  for(j in 1:r)
  {
    GFElem[1,j]<-0;
    for(i in 2:r){
      if(i==j)
        GFElem[i,j]<-1
      else
        GFElem[i,j]<-0
    }

    GFElem[r+1,j]<-MF[j]

  }

  for(i in (r+2):cardin){
    x <-nextElem(GFElem[i-1,],MF,p,r)
    for (j in 1:r){
      GFElem[i,j]<-x[j]
    }
  }

  return(GFElem)
}

