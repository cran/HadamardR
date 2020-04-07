#' GFMult
#' GFMult Generate Multiplication table of GF(p^r), where p is a prime power.
#'
#' @param cardin  integer
#' @return Multiplication table of GF(p^r)
#' @export
#' @details This function returns Multiplication table of Galois field of order p^r. To use this function, Minimum function, elements of GF are required.
#' Minimum functions are available in internal dataset. Elements can be generated using GFELEM function.
#'
#' @examples
#' GFMult(9)
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



GFMult<-function(cardin){

  gfmult<-matrix(c(rep(0,cardin*cardin)),nrow=cardin,ncol=cardin)
  for (i in 1:cardin){
    gfmult[1,i]<-1
    gfmult[i,1]<-1
  }
  for (i in 2:cardin){
    for(j in i:cardin){
      if(i+j<=(cardin+1)){
        gfmult[i,j]=(i+j)-1
        gfmult[j,i]=(i+j)-1
      }
      else{
        k<-(i+j)%%cardin
        if(k==0){
          k<-cardin
        }
        gfmult[i,j]=k
        gfmult[j,i]=k
      }
    }
  }
  return(gfmult)
}


