#' cdn_ehlich
#'
#' Checks Hadamard Matrix can be constructed using Ehlich's method.
#'
#' @param order  integer
#' @return 4 or NULL
#' @export
#' @details Ehlich (1965)'s construction method requires order of the Hadamard
#' matrix must be a of the form (n-1)^2. Conditions are (i)Order=(n-1)^2;
#' (ii) n is a prime or prime power and 3(mod 4). (iii) (n-2) must be a
#' prime or prime power. In case, if all the three conditions are satisfied,
#' function will return 4 or NULL.
#' @seealso
#' \code{\link{had_ehlich}} for Ehlich's construction method.
#' @references
#' Ehlich, H. (1965). Neue Hadamard-matrizen. Arch. Math., 16, 34-36.
#' @examples
#' cdn_ehlich(36)
#' #Condition 1:(n-1)^2 = 36 = 6^2
#' #Condition 2: n=7 (prime)and n=3(mod 4)
#' #Condition 3: n-2=5 (prime)
#' #Return
#' #4
#' cdn_ehlich(64)
#' #Condition 1:(n-1)^2 = 64 = 8^2
#' #Condition 2: n=9 (prime power) but n=1(mod 4).
#' #Condition 2 fails
#' #Return
#' #NULL


cdn_ehlich<-function(order){
 #browser()
  if(sqrt(order)%%1!=0){
    return(NULL)
  }
  n<-sqrt(order)+1
  m<-n-2
  if(numbers::mod(m,4)!=1){
    return(NULL)
  }
    if(is.prime(n)==TRUE){
      if(is.prime(m)==T & numbers::mod(m,4)==1)
      return(4)
    }
        if(is.null(is.primepower(n))){
          if(is.null(is.primepower(m))){
          return(NULL)
              }else
                return(4)
        }
}
