#' cdn_kronecker_matrix
#'
#' @description
#' Checks Hadamard Matrix can be constructed by multiplying 2 existing Hadamard matrix.
#' @param r  integer
#' @return 12 or NULL
#' @export
#'
#' @details This function checks whether the Hadamard matrix  can be constructed as multiple of 2 Hadamard matrix.
#' Returns the value 12, if it is possible otherwise NULL is returned.
#' @examples
#' cdn_kronecker_matrix(8)
#' #12
#' @examples
#' cdn_kronecker_matrix(12)
#' #NULL
#'

cdn_kronecker_matrix<-function(r){
  if(numbers::mod(r,2)==0){
    if(numbers::mod(r/2,4)==0)
      return(ret_value=12)
  }
  if(numbers::mod(r,4)==0){
    h1<-r/4
    if(numbers::mod(h1,4)==0){
      return(ret_value=12)
    }else
      return(NULL)
  }
}
