#' Had_method
#'
#' Had_method  performs the give order of the matrix is constructed by which method.
#'
#' @param order  integer (order of the Hadamard matrix)
#' @return Method number
#' @details If the method number retuns, if it
#' @details  1      kronecker method (power of 2 only)
#' @details  2      PaleyI
#' @details  3      PaleyII
#' @details  4      Ehlich method
#' @details  5      Williamson method
#' @details  6      Baumert-Hall method
#' @details  7      Goethals-Seidel by using Base sequences
#' @details  8      Goethals-Seidel by using Turyn sequences
#' @details  9      Miyamoto method
#' @details  10     Suplimentary Difference Sets
#' @details  11     Cooper-Wallis method
#' @details  12     Kronecker product method
#' @details  13     Goethals-Seidel by using T sequences
#' @details  14     Paley I Prime Power
#' @details  15     Paley II Prime Power
#' @export
#'
#' @examples
#' Had_method(92)   # "5"
#' @examples
#' Had_method(324)  # "4"

Had_method<-function(order){
  ret_value<-c(NULL,NULL)
  if(numbers::mod(order,4)!=0){
    return("order should be multiple of 4")
  }
  if(order==668){
    return(NULL)
  }
  if(order==716){
    return(NULL)
  }
  if(order==892){
    return(NULL)
  }
  #method= had_kronecker
  exponent=pow(order)
  if(is.null(exponent)!=TRUE){
    ret_value[1]<-1
    ret_value[2]<-exponent
    return(ret_value)
  }
  ##method=paleyI
  m<-cdn_PaleyI(order)
  if(is.null(m)==F){
    ret_value[1]<-2
    return(ret_value)
  }
  ##method=paleyII
  m<-cdn_PaleyII(order)
  if(is.null(m)==F){
    ret_value[1]<-3
    return(ret_value)
  }
  ##method=Ehlich
  m<-cdn_ehlich(order)
  if(is.null(m)==F){
    ret_value[1]<-4
    return(ret_value)
  }
  ##method=Williamson
  m<-cdn_williamson(order)
  if(is.null(m)==F){
    ret_value[1]<-5
    return(ret_value)
  }
  ##method=Baumert-Hall
  m<-cdn_baumert(order)
  if(is.null(m)==F){
    ret_value[1]<-6
    return(ret_value)
  }
  ##method=Goethals-Seidel (Base sequences)
  m<-cdn_goethals_base(order)
  if(is.null(m)==F){
    ret_value[1]<-7
    return(ret_value)
  }
  ##method=Goethals-Seidel (Turyn sequences)
  m<-cdn_goethals_Turyn(order)
  if(is.null(m)==F){
    ret_value[1]<-8
    return(ret_value)
  }
  ##method=SDS
  m<-cdn_sds(order)
  if(is.null(m)==F){
    ret_value[1]<-10
    return(ret_value)
  }
  ##method=Cooper Wallis
  x<-get_cooper(order)
  if(is.null(x$m)==F){
    if(is.null(x$n)==F)
      ret_value[1]<-11
    return(ret_value)
  }
  ##method=KPM
  m<-cdn_kronecker_matrix(order)
  if(is.null(m)==F){
    ret_value[1]<-12
    return(ret_value)
  }
  ##method=Goethals-Seidel (Turyn sequences)
  m<-cdn_goethals_T(order)
  if(is.null(m)==F){
    ret_value[1]<-13
    return(ret_value)
  }
  ##method=miyamoto
  m<-cdn_miyamoto(order)
  if(is.null(m)==F){
    ret_value[1]<-9
    return(ret_value)
  }
  #PaleyIPrimepower
  m<-cdn_PaleyIprimepower(order)
  if(is.null(m)==F){
    ret_value<-14
    return(ret_value)
  }
  #PaleyIIPrimePower
  m<-cdn_PaleyIIprimepower(order)
  if(is.null(m)==F){
    ret_value<-15
    return(ret_value)
  }
}
