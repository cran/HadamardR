#' quadprime
#'
#' quadprime is a internal function not exported.
#' @param p  integer
#' @return squres
#' @details
#' this function obtain Quadratic residues of GF. It returns squares of odd elements of GF

quadprime <- function(p){
  quad<- c(rep(0,(p-1)/2))
  count=1
  for (i in seq(1,p-1,by=2)){
    quad[count]<- (i*i)%%p
    count<- count+1
  }
  return(quad)
}
