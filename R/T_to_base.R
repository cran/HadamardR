#' Turyn_to_T
#' internal function. converts Turyn sequences to Base Sequences.
#'
#' @param order  integer (order of the matrix)
#' @param dat1 is the Turyn sequences subset exported from Tseq
#' @return  Basesequences of length of order
#' @details dat - Internal dataset containing 4 sequences in long form with lentgh 2n-1, 2n-1, n, n. Using the 4 Turyn sequences,
#' the function creates 4 sequences of length n+p, n+p, n, n.
#' Base Sequnces are usually used in creating matrices of Goethel Seidal array.
#' @details Turyn type-sequences are available for 28,30,34,36 in the internal table.
#' @seealso
#' \code{\link{had_goethals_Turyn}} for Goethals-Seidel construction method using Turyn sequences.
#'
#'
#'
#'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#'



Turyn_to_T <- function(dat1,order){
  x<- subset( dat1$TurynValue ,dat1$TurynMatrix ==1)
  y<-  subset( dat1$TurynValue ,dat1$TurynMatrix ==2)
  z<-  subset( dat1$TurynValue ,dat1$TurynMatrix ==3)
  w<-  subset( dat1$TurynValue ,dat1$TurynMatrix ==4)

  a1<- c(z,w)
  a2<- c(z,-w)
  a3<- x
  a4<- y

  p<-order-1

  t1 <- c((a1+a2)/2,rep(0,order))
  t2 <- c((a1-a2)/2,rep(0,order))
  t3 <- c(rep(0,order+p),(a3+a4)/2)
  t4 <- c(rep(0,order+p),(a3-a4)/2)

  Tt<-list(t1=t1,t2=t2,t3=t3,t4=t4)
  return(Tt)
}
