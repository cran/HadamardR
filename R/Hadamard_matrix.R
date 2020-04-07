#' Hadamard_Matrix
#'
#' Hadamard_Matrix is generic function for construction of Hadamard matrix.
#'
#' @param order  integer
#' @return
#' Hadamard Matrix of given Order
#' @export
#'
#' @details
#' function Hadamard_matrix was created which does not require known of
#' construction methods. Hadamard_matrix() takes an integer as input and returns
#' Hadamard matrix if it is available. In case, it is not possible to construct,
#' NULL value is returned.
#'
#' @examples
#' Hadamard_Matrix(1)
#' #1
#' Hadamard_Matrix(2)
#' #      [,1] [,2]
#' # [1,]    1    1
#' # [2,]    1   -1
#' Hadamard_Matrix(8)
#'       # [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#' # [1,]    1    1    1    1    1    1    1    1
#' # [2,]    1   -1    1   -1    1   -1    1   -1
#' # [3,]    1    1   -1   -1    1    1   -1   -1
#' # [4,]    1   -1   -1    1    1   -1   -1    1
#' # [5,]    1    1    1    1   -1   -1   -1   -1
#' # [6,]    1   -1    1   -1   -1    1   -1    1
#' # [7,]    1    1   -1   -1   -1   -1    1    1
#' # [8,]    1   -1   -1    1   -1    1    1   -1
#' Hadamard_Matrix(10)
#' #"Order is not a Hadamard number"
#' Hadamard_Matrix(668)
#' #"Not possible to construct or order is not a multiple of 4"


Hadamard_Matrix<-function(order){
  if (order==1){
    return(1)
  }
  if (order==2){
    h<-had_kronecker(n=order)
    return(h)
  }
  if (numbers::mod(order,4)!=0){
    return("Order is not a Hadamard number")
  }
  ret<-Had_method(order)
  if(is.null(ret)){
    return("Not possible to construct or order is not a multiple of 4")
  }
  if(ret[1]==1){
    exponent<-ret[2]
    h<-had_kronecker(n=order,exponent)
    return(h)
    }
  if(ret[1]==2){
    h<-PaleyI(order)
    return(h)
    }
  if(ret[1]==3){
    h<-PaleyII(order)
    return(h)
    }
  if(ret[1]==4){
    h<-had_ehlich(order)
    return(h)
    }
  if(ret[1]==5){
    h<-had_williamson(order)
    return(h)
    }
  if(ret[1]==6){
    h<-had_baumert(order)
    return(h)
    }
  if(ret[1]==7){
    h<-had_goethals_base(order)
    return(h)
    }
  if(ret[1]==8){
    h<-had_goethals_Turyn(order)
    return(h)
    }
  if(ret[1]==9){
    h<-had_miyamoto(order)
    return(h)
    }
  if(ret[1]==10){
    h<-had_SDS(order)
    return(h)
    }
  if(ret[1]==11){
    h<-had_cooper(order)
    return(h)
    }
  if(ret[1]==12){
    h<-kronecker_matrix(order)
    return(h)
    }
  if(ret[1]==13){
    h<-had_goethals_T(order)
    return(h)
  }
  if(ret[1]==14){
    h<-PaleyIPrimePower(order)
    return(h)
  }
  if(ret[1]==15){
    h<-PaleyIIPrimePower(order)
    return(h)
  }
}
