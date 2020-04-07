#' cdn_goethals_base
#'
#' @description
#' Checks Hadamard Matrix can be constructed using available base sequences.
#' @param order  integer
#' @return 7 or NULL
#' @export
#' @details This function checks whether the Hadamard matrix of given order can be constructed using base sequences.
#' If base sequences of length n+1,n+1,n,n are available, T-sequences of length 2n+1,2n+1,2n+1,2n+1 can be constructed.
#' From T-sequence of length 2n+1, Hadamard matrix of order 4(2n+1) can be constructed.
#' Returns the value 7, if it is possible otherwise NULL is returned.
#' @details Base sequences are available in the internal dataset is  1:35
#'
#' @seealso
#' \code{\link{had_goethals_base}} for Goethals-Seidel construction method.
#'
#' @seealso
#' \code{\link{baseseq}}
#'
#' @source
#' The Base sequences were obtained from \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#' @examples
#' cdn_goethals_base(20)
#' #7
#' @examples
#' cdn_goethals_base(24)
#' #NULL
#'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.




cdn_goethals_base<-function(order){
  #browser()
  x<-order/4
  m<-(x-1)/2
  n<-unique(Basesequences$BaseOrder)
  if(m %in% n)
    return(ret_value=7)
  else
    return(NULL)
}
