#' had_goethals_T
#' had_goethals_Turyn performs the Hadamard Matrix from Goethals-Seidel method
#' by using T sequences.
#'
#' @param n integer (order of the matrix)
#' @return Hadamard matrix of order x
#' @export
#' @details
#' This function construct Hadamard matrix of given order using T sequences.
#' If T sequences of length n,n,n,n are available, Hadamard matrix of order 4n can be constructed.
#' Returns the Hadamard matrix of given order. If for given order the T sequences are not available
#' returns NULL.
#' @details The T sequences are stored in internal dataset. The available
#' T sequences of length is seq(1,73,2) and 83, 101 and 107
#' @source
#' The T sequences are available at London (2013)  and
#' The Base sequences were obtained from \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#' @references
#' London, S. 2013. Constructing New Turyn Type Sequences, T-Sequences and Hadamard Matrices. PhD Thesis, University of Illinois at Chicago, Chicago.
#' @examples
#' had_goethals_T(4)
#' #      [,1] [,2] [,3] [,4]
#' # [1,]    1   -1   -1   -1
#' # [2,]    1    1   -1    1
#' # [3,]    1    1    1   -1
#' # [4,]    1   -1    1    1
#' @examples
#' had_goethals_T(8)
#' #NULL

had_goethals_T<-function(n){
  order<-n/4
  dat<-T_seq(order)
  if(nrow(dat)==0){
    return(NULL)
  }
  a1 <- subset(dat$Value,dat$Matrix==1)
  a2 <- subset(dat$Value,dat$Matrix==2)
  a3 <- subset(dat$Value,dat$Matrix==3)
  a4 <- subset(dat$Value,dat$Matrix==4)

  T1<- circulant_mat(matrix(a1))
  T2<- circulant_mat(matrix(a2))
  T3<- circulant_mat(matrix(a3))
  T4<- circulant_mat(matrix(a4))

  A<-  T1+T2+T3+T4
  B<- -T1+T2+T3-T4
  C<- -T1-T2+T3+T4
  D<- -T1+T2-T3+T4
  n1<-nrow(A)
  R <- antidiagnol(n1)
  mat_H<- goethals_seidel_array(A,B,C,D)
  return(mat_H)
}
