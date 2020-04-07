#' GFM
#' GFM Generate Multiplication table of GF(p^r), where p is a prime power.
#'
#' @param cardin  integer
#' @return Multiplication table of GF(p^r)
#' @export
#' @details This function returns Multiplication table of Galois field of order p^r. To use this function, Minimum function, elements of GF are required.
#' Minimum functions are available in internal dataset. Elements can be generated using GFELEM function.
#'
#' @examples
#' GFM(9)
#' ##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#'##  [1,]    1    1    1    1    1    1    1    1    1
#'##  [2,]    1    3    4    5    6    7    8    9    2
#'##  [3,]    1    4    5    6    7    8    9    2    3
#'##  [4,]    1    5    6    7    8    9    2    3    4
#'##  [5,]    1    6    7    8    9    2    3    4    5
#'##  [6,]    1    7    8    9    2    3    4    5    6
#'##  [7,]    1    8    9    2    3    4    5    6    7
#'##  [8,]    1    9    2    3    4    5    6    7    8
#'##  [9,]    1    2    3    4    5    6    7    8    9



GFM<-function(cardin){
  GFMult<-matrix(c(rep(0,cardin*cardin)),nrow=cardin,ncol=cardin)
  #browser()
  for (i in 1:cardin){
    GFMult[1,i]<-1
    GFMult[i,1]<-1
  }
  for (i in 1:(cardin-1)){
    for(j in 1:(cardin-1)){
      if (i+j<cardin){
        GFMult[(i+1),(j+1)]<- (i+j)+1
      }
      else {
        GFMult[(i+1),(j+1)] <-(i+j)%%cardin+2
      }
    }
  }
  return(GFMult)
}
