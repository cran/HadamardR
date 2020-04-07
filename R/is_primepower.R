#' is.primepower
#'
#' @description
#' Checks whether given number is a prime power or not. Note that for a prime number, it would return NULL.
#' @param p  integer
#' @return a and b where p=a^b and a is a prime number. Otherwise NULL
#' @export
#'
#' @details Returns a and b where p=a^b, otherwise NULL. Uses primeFactors() function of numbers package.
#'
#'
#' @examples is.primepower(2048)
#' #2 11
#' @examples is.primepower(7)
#' #NULL
#' @examples is.primepower(100)
#' #NULL
is.primepower<-function(p){
  primefacts<-numbers::primeFactors(p)
  uniprimes<-unique(primefacts)
  if (length(uniprimes)==1 && length(primefacts)>1 ){
    basepower<-c(uniprimes,length(primefacts))
    return(basepower)
  }
  return(NULL)
}

