#' miyamotoC
#'
#'
#' @param n  integer (order of the matrix)
#' @return q matrix
#' @export
#' @references
#' Miyamoto, M. (1991). A Construction of Hadamard matrices. J. Math. Phy., 12, 311-320.
#'
#' @examples
#' miyamotoC(20)
#'#      [,1] [,2] [,3] [,4] [,5]
#'#[1,]    0    1    1   -1   -1
#'#[2,]    1    0   -1   -1    1
#'#[3,]    1   -1    0    1   -1
#'#[4,]   -1   -1    1    0    1
#'#[5,]   -1    1   -1    1    0

miyamotoC<- function(n){
  if(numbers::mod(n,4)!=0){
    return(NULL)
  }
  p<-n/4
  if(is.prime(p)==F){
    return(NULL)
  }
  if(is.prime(p)==T){
  q<- qhad2(p)
  }
  s<-is.primepower(p)
  if(is.null(s)==FALSE){
    q<-QPrimePower(p)
  }
  m<- (p-1)/2
  v1<-c(1:m)
  v2<- c(1:m)
  count1<-1
  count2<-1
  for(j in 2:p){
    if((q[1,j]==1) & (j>(m+1))){
      v1[count1]=j
      count1<- count1+1
    }
    else if((q[1,j]==-1) & (j<=(m+1))){
      v2[count2]=j
      count2<- count2+1
    }
  }
  for(k in 1:(count1-1)){
    for(j in 1:p){
      if(j==v1[k])
        for(i in 1:p){
          temp<-q[i,j]
          q[i,j]<- q[i,v2[k]]
          q[i,v2[k]]<-temp
        }
    }
  }
  for(k in 1:(count2-1)){
    for(i in 1:p){
      if(i==v2[k])
        for(j in 1:p){
          temp<-q[i,j]
          q[i,j]<-q[v1[k],j]
          q[v1[k],j]<- temp
        }
    }
  }
  return(q)
}
