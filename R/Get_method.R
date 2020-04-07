#' Get_method
#'
#' Get_method helps finding the given order of the matrix is constructed by which method.
#'
#' @param order  integer
#' @return Method name of the given order.
#' @export
#'
#' @examples
#' Get_method(92) # Williamson method
#' @examples Get_method(24)
#' # Paley I




Get_method<-function(order){
  if(order==340){
    return("PaleyII prime power")
  }
  if(order==580){
    return("PaleyII prime power")
  }
  ret<-Had_method(order)
  if(ret[1]==1){
    return("Kronecker Method  (power of 2)")
  }
  if(ret[1]==2){
    return("Paley I")
  }
  if(ret[1]==3){
    return("Paley II")
  }
  if(ret[1]==4){
    return("Ehlich Method")
  }
  if(ret[1]==5){
    return("Williamson Method")
  }
  if(ret[1]==6){
    return("Baumert-Hall Method")
  }
  if(ret[1]==7){
    return("Goethals-Seidel Method by using Base sequences")
  }
  if(ret[1]==8){
    return("Goethals-Seidel Method by using Turyn sequences")
  }
  if(ret[1]==9){
    return("Miyamoto Method")
  }
  if(ret[1]==10){
    return("Supplementary Difference Sets")
  }
  if(ret[1]==11){
    return("Cooper-Wallis Method")
  }
  if(ret[1]==12){
    return("Kronecker Product Method")
  }
  if(order==668){
    return("No Construction Method Available")
  }
  if(order==716){
    return("No Construction Method Available")
  }
  if(order==892){
    return("No Construction Method Available")
  }
  if(ret[1]==13){
    return("Cooper-Wallis Method")
  }
  if(ret[1]==14){
    return("Cooper-Wallis Method")
  }
}
