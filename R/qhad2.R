#' qhad2
#'
#' qhad2 creats the Quadratic residues of the prime number.
#' @param p is the integer
#' @return  matrix of order p
#' @export
#' @details
#' The given input is prime number it retuns the matrix of order p.
#' if the input is not prime number it returns NULL.
#'
#' @examples
#' qhad2(7)
#'#      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
#'#[1,]    0   -1   -1    1   -1    1    1
#'#[2,]    1    0   -1   -1    1   -1    1
#'#[3,]    1    1    0   -1   -1    1   -1
#'#[4,]   -1    1    1    0   -1   -1    1
#'#[5,]    1   -1    1    1    0   -1   -1
#'#[6,]   -1    1   -1    1    1    0   -1
#'#[7,]   -1   -1    1   -1    1    1    0

qhad2<-function(p){
  if(is.prime(p)==F){
    return(NULL)
  }
  qu<-quadprime(p)
  q <-matrix(c(rep(0,p)),nrow=p,ncol=p)
  for (i in 1:p){
    for(j in 1:p){
      flag<-0
      if((i-j)==0){
        q[i,j]<-i-j
        flag<-1
      }
      else if ((i-j)>0) {
        temp<-i-j
      }
      else {
        temp<-i-j+p
      }
      if(!flag){
        for(k in 1:((p-1)/2)) {
          if (temp==qu[k]){
            flag<-1
            break
          }
        }
        if (flag==1)
          q[i,j]<-1
        else
          q[i,j]<--1
      }
    }
  }
  return (q)
}
