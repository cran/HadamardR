#' is_divisible
#'
#' is_divisible is internal function. Not exported.
#'
#' @param num  integer
#' @param divisor integer
#' @return num/divisor
#' @details it returns num/divisor value.

is_divisible<-function(num,divisor){
  ret<-NULL
  if(numbers::mod(num,divisor)==0){
    ret<-num/divisor
  }
  return(ret)
}
