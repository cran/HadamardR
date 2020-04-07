#' kro_method
#'
#' kro_method internal function. Not exported.
#' @param r  integer (order of the matrix)
#' @return r/2 or NULL.
#' @export
#' @references
#' Sylvester, J.J. (1967). Thoughts on orthogonal matrices, simultaneous sign-succession
#' and Tessellated pavements in two or more colours, with applications to Newton's rule,
#' ornamental Tie-work, and the theory of numbers. Phil. Mag.,34, 461-475.
#' @references
#' Sylvester, J.J. (1968). Problem 2511. Math. Questions and solutions, 10, 74.
#' @references
#' Hedayat, A. and Wallis, W.D. (1978). Hadamard Matrices and Their Application.Ann. Stat., 6, 1184-1238.


kro_method<-function(r){
  if(numbers::mod(r,2)==0){
    if(numbers::mod(r/2,4)==0)
    return(r/2)
  }
  if(numbers::mod(r,2)==0){
    if(numbers::mod(r,8)==0){
    if(numbers::mod((r/8),4)==0)
      return(r/8)
    }
  }
  if(numbers::mod(r,4)==0){
  h1<-r/4
  if(numbers::mod(h1,4)==0){
    return(h1)
   }
  }
  return(0)
}
