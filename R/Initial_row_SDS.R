#' Initial_row_SDS
#' Initial_row_SDS is an internal function.Not Exported.
#'
#' @param i is the numeric vectors
#' @param j is the numeric vectors
#' @param v is the numeric vectors
#' @param n is the numeric vectors
#' @param r is the numeric vectors
#' @return Intial rows of Matrix.
#' @details All inputs are numeric vectors of same length.
#' This function used in the COnstruction of Hadamard matrix by Supplementary Differences Sets
#' It converts the SDS sets into binary forms (+1 or -1).
#'
#' @references Djokovic, D. Z. (1992a). Skew Hadamard matrices of order 4x37 and 4x39. J. Combin. Theory, A 61, 319-321.
#' @references Djokovic, D. Z. (1992b). Construction of some new Hadamard matrices. Bull. Austral. math. Soc., 45, 327-332.
#' @references Djokovic, D. Z. (1992c). Ten new Hadamard matrices of skew type. Publ.Electrotechnickog Fak., Ser. Mathematika, Univ. of Belgrade, 3, 47-59.
#' @references Djokovic, D. Z. (1992d). Ten Hadamard matrices of order 1852 of Goethals-Seidel type. Europ. J. Combinatorics, 13, 245-248.
#' @references Djokovic, D. Z. (1994a). Two Hadamard matrices of order 956 of Goethals-Seidel type. Combinatorica, 14(3), 375-377.
#' @references Djokovic, D. Z. (1994b). Five new Hadamard matrices of order skew type. Austral. J. Combinatorics, 10, 259-264.
#'



Initial_row_SDS<-function(i,j,v,n,r){
  val<-j-i
  if(val < 0){
    val<-r+(j-i)
  }
  for(k in 1:n){
    if(v[k]==val){
      return(-1)
    }
  }
  return(1)
}
