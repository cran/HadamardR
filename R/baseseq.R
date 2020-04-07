#' baseseq
#'
#' Extracts the selection of Basesequences from internal dataset. Not exported.
#'
#'
#' @param order integer
#' @return
#' Required Basesequences of order of x
#' @details Create Basesequence of given order from the internal dataset Basesequence
#' Base sequences are available in the internal table for order= 1:35
#' @source
#' The Base sequences were obtained from \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#'

baseseq <- function(order){
  dat <- subset(Basesequences,Basesequences$BaseOrder==order)
  return(dat)
}

