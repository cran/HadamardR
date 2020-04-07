#' Base_to_T
#'
#' internal function and it is not exported. It converts base sequences to T-Sequences.
#'
#' @param x  integer (order of the base sequence)
#' @param dat is the frame containing basesequences to be exported
#' @return
#' 4 T-sequences of length of 2x+p.
#'
#' @details dat - Internal dataset containing 4 sequences in long form with lentgh n+p,n+p,n,n. Using the 4 basequences,
#' the function creates 4 sequences of length 2n+p,2n+p,2n+p,2n+p.
#' T-Sequnces are usually used in creating matrices of Goethel Seidal array.
#'
#' @source
#' The Base sequences were obtained from \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#'


base_to_T <- function(dat,x){
  a1 <- subset(dat$BaseValue,dat$BaseMatrix==1)
  a2 <- subset(dat$BaseValue,dat$BaseMatrix==2)
  a3 <- subset(dat$BaseValue,dat$BaseMatrix==3)
  a4 <- subset(dat$BaseValue,dat$BaseMatrix==4)
  p <- length(a1) - x
  t1 <- c((a1+a2)/2,rep(0,x))
  t2 <- c((a1-a2)/2,rep(0,x))
  t3 <- c(rep(0,x+p),(a3+a4)/2)
  t4 <- c(rep(0,x+p),(a3-a4)/2)
  Tt<-list(t1=t1,t2=t2,t3=t3,t4=t4)
  return(Tt)
}
