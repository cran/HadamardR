#' GFADD
#' @description
#' Addition table of GF(P^r)
#' @param GFElem  integer (Can be obtained by calling GFELEM function)
#' @param p integer (a prime number)
#' @param r integer (a positive integer)
#' @details
#' This function returns addition table of Galois field of order p^r. To use this function, Minimum function, elements of GF are required.
#' Minimum functions are available in internal dataset. Elements can be generated using GFELEM function.
#' @return A matrix of size p^r x p^r
#' @export
#' @examples
#'
#' p<-3
#' r<-2
#' cardin<-p^2
#' mf<-subset(HadamardR:::minimumfunction,HadamardR:::minimumfunction$s==cardin)
#' MF<-mf$coeff
#' GFElem<-GFELEM(p,r,MF)
#' GFADD(GFElem,p,r)
#' #Addition Table of GF(9)
#'

GFADD<-function(GFElem,p,r){
  cardin<-p^r
  GFAdd<-matrix(c(rep(0,cardin*cardin)),nrow=cardin,ncol=cardin)
  add<-c(rep(0,r))
  for (i in 1:cardin) {
    for (j in 1:cardin){
      for (k in 1:r) {
        add[k]<- (GFElem[i,k]+GFElem[j,k])%%p
      } #k loop
      GFAdd[i,j]<-GFCheck(GFElem,r,cardin,add)
    } # j loop
  } # i loop
  return(GFAdd)
}



