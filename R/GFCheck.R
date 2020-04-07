#' GFCheck
#' @description This is an internal function to return the position of argument add in elements of GF(cardin)
#'
#' @param GFElem  integer array
#' @param cardin integer
#' @param add integer array
#' @param r integer
#' @return i integer
#' The position of the element checked in GFElem
#' @details
#' This function is not exported. Used for checking the result of addition or multiplication of GFElements.




GFCheck<-function(GFElem,r,cardin,add){
  for (i in 1:cardin){
    flag<-1
    for (j in 1:r){
      if (GFElem[i,j]!=add[j]){
        flag<-0
        break
      }
    }
    if(flag)
      return(i)
  }
  return(NULL)
}

