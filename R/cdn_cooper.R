#' cdn_cooper
#' @description
#' Checks Hadamard Matrix can be constructed using Williamson arrays and T- sequences.
#' @param order  integer
#' @return 11 or NULL
#' @export
#' @details Cooper-Wallis is a construction of Hadamard matrices which combines
#' Williamson matrices and T-sequences.
#'
#' @seealso
#' \code{\link{had_cooper}} for Cooper-Wallis construction method.
#'  \code{\link{get_cooper}} for finding order of Williamson and T-Sequences.
#' @details The availabile Williamson sequences in the internal data sets is
#' seq(1,63, 2) except 15, 35, 47, 53, 59 in the internal table.
#' @details The availabile T- sequences in the internal data sets is seq(1,73,2) and
#' 83, 101, 107.
#' @examples
#' cdn_cooper(20)
#' #11
#' @examples
#' cdn_cooper(16)
#' #NULL
#' @references
#' Cooper, J., and Wallis, W., D. (1972). A construction for Hadamard arrays. Bull. Austral. Math. 7, 269-278.
#'
cdn_cooper<-function(order){
  x<-get_cooper(order)
  if(is.null(x$m)==F){
    if(is.null(x$n)==F)
      ret_value<-11
    return(ret_value)
  }else
    return(NULL)
}
