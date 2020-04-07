#' QPrimePower
#' QPrimePower creats the Quadratic residues of the prime number.
#' @param cardin  integer
#' @return matrix of cardin x cardin
#' @export
#' @details
#' The given input is prime power it retuns the matrix of order cardin.
#' if the input is not prime number then it returns NULL.
#' @examples
#' QPrimePower(9)
#'#      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#'#[1,]    0    1   -1    1   -1    1   -1    1   -1
#'#[2,]    1    0    1   -1    1    1   -1   -1   -1
#'#[3,]   -1    1    0   -1    1   -1   -1    1    1
#'#[4,]    1   -1   -1    0    1   -1    1    1   -1
#'#[5,]   -1    1    1    1    0   -1    1   -1   -1
#'#[6,]    1    1   -1   -1   -1    0    1   -1    1
#'#[7,]   -1   -1   -1    1    1    1    0   -1    1
#'#[8,]    1   -1    1    1   -1   -1   -1    0    1
#'#[9,]   -1   -1    1   -1   -1    1    1    1    0
#' @examples
#' QPrimePower(36)
#' #NULL


QPrimePower<-function (cardin){
  d<-is.primepower(cardin)
  if(is.null(is.primepower(cardin))){
    return(NULL)
  }
  p<-d[1]
  r<-d[2]
   mf<-subset(minimumfunction,minimumfunction$s==cardin)
  if(nrow(mf)==0)
    return(NULL)
  MF<-c(mf$coeff)
  s<-cardin
  gfelem<-GFELEM(p,r,MF)
  gfadd<-GFADD(gfelem,p,r)
  Q<-matrix(rep(0,s*s),nrow = s,ncol = s)
  for (i in 1:s){
    for(j in 1:s){
      if((i-j)==0){
        Q[i,j]=0
      }
      else {
        for(k in 1:s){
          if (gfadd[k,j]==i){
            temp <- k
          }
        }
        if ((temp%%2)==0 && temp>0){
          Q[i,j]=1
        }
        else if (temp>0){
          Q[i,j]=-1
        }
        else{
          Q[i,j]=0
        }
      }
    }
  }
  return(Q)
}
