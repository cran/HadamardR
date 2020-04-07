#' method1_paleyII
#'
#' method1_paleyII is internal function not exported.
#'
#' @param n  integer
#' @return 0 or (n/2)-1
#'
#' @details
#' this function checks q<- (n/2)-1, q is prime number and q = 1(mod 4). if it satisfy it returns q;
#' otherwise returns NULL.
#'
#' @references
#' Paley, R.E.A.C. (1933). On Orthogonal matrices. J. Combin. Theory, A 57(1), 86-108.




method1_paleyII<-function(n){
  q<- (n/2)-1
  if(is.prime(q)==T & numbers::mod(q,4)==1 )
    return(q)
  else{
    return(0)
  }
}
