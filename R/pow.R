#' pow
#'
#' pow functions finds the exponent of 2.
#'
#' @param n integer
#' @return power of 2
#' @export
#'
#' @details
#' This function checks the given number is the power of 2 or not
#' If the given number is power of 2 it returns the exponent value; otherwise NULL is returned.
#'
#' @examples
#' pow(4)
#' # 2
#' @examples
#' pow(5)
#' #NULL
#' @examples
#' pow(6)
#' #NULL


pow<-function(n){
  exponent=0
    if(n>0){
      while(numbers::mod(n,2) == 0){
        n=n/2
          exponent =exponent+1
            }
          if(n == 1){
            return(exponent)
       }
      }
    if(n == 0 || n != 1)
  {
      return(NULL)
  }
}



