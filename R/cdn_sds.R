#' cdn_sds
#'
#' @description
#' Checks Hadamard Matrix can be constructed using available Suplementary Difference Sets.
#' @param order  integer
#' @return 10 or NULL
#' @export
#' @details This function checks whether the Hadamard matrix of given order can be constructed using Suplementary Diffrence sets.
#' If SDS is available it Returns the value 10 otherwise NULL.
#' @details SDS are available for 103,127,151,163,181,191,239,251,463,631 in the internal table.
#'
#' @seealso
#' \code{\link{had_SDS}} for SDS construction method.
#'
#' @source
#' SDS sets are availble from Djokovic (1992a,b,c,d and 1994a,1994b).
#'
#' @references Djokovic, D. Z. (1992a). Skew Hadamard matrices of order 4x37 and 4x39. J. Combin. Theory, A 61, 319-321.
#' @references Djokovic, D. Z. (1992b). Construction of some new Hadamard matrices. Bull. Austral. math. Soc., 45, 327-332.
#' @references Djokovic, D. Z. (1992c). Ten new Hadamard matrices of skew type. Publ.Electrotechnickog Fak., Ser. Mathematika, Univ. of Belgrade, 3, 47-59.
#' @references Djokovic, D. Z. (1992d). Ten Hadamard matrices of order 1852 of Goethals-Seidel type. Europ. J. Combinatorics, 13, 245-248.
#' @references Djokovic, D. Z. (1994a). Two Hadamard matrices of order 956 of Goethals-Seidel type. Combinatorica, 14(3), 375-377.
#' @references Djokovic, D. Z. (1994b). Five new Hadamard matrices of order skew type. Austral. J. Combinatorics, 10, 259-264.
#'
#'
#' @examples
#' cdn_sds(412)
#' #10
#' @examples
#' cdn_sds(428)
#' #NULL
#'
#'
cdn_sds<-function(order){
  x<-order/4
  n<-unique(SDS$N)
  if(x %in% n)
    return(ret_value=10)
  else
    return(NULL)
}
