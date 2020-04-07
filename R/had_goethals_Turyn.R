#' had_goethals_Turyn
#'
#' had_goethals_Turyn performs the Hadamard Matrix from Goethals-Seidel method
#' by using Turyn sequences.
#'
#' @param r integer (order of the matrix)
#' @return Hadamard matrix of order r
#' @export
#'
#' @details
#' This function construct Hadamard matrix of given order using Turyn sequences.
#' If Turyn sequences of length 2n-1, 2n-1, n, n is available then Turyn sequences are
#' converted in T sequences of length 2n+p, 2n+p, 2n+p, 2n+p and p=n-1, these T sequences are used for
#' construction of  Hadamard matrix.
#' If the given order of the the Turyn sequences are not available it returns NULL.
#' @details Turyn type-sequences are available for 28,30,34,36 in the internal dataset.
#'
#' @source
#' The Base sequences were obtained from
#' \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#'
#' @examples
#' \donttest{
#' #Big matrices
#' had_goethals_Turyn(356)
#' had_goethals_Turyn(404)
#' }


had_goethals_Turyn<- function(r){
  torder<-r/4
  order<-(torder+1)/3
  dat1<-Turyn_seq(order)
  if(nrow(dat1)==0){
    return(NULL)
  }

  Tt<-Turyn_to_T(dat1,order)

  t1<-Tt$t1
  t2<-Tt$t2
  t3<-Tt$t3
  t4<-Tt$t4

  T1<- circulant_mat(matrix(t1))
  T2<- circulant_mat(matrix(t2))
  T3<- circulant_mat(matrix(t3))
  T4<- circulant_mat(matrix(t4))

  A<-  T1+T2+T3+T4
  B<- -T1+T2+T3-T4
  C<- -T1-T2+T3+T4
  D<- -T1+T2-T3+T4
  n1<-nrow(A)
  R <- antidiagnol(n1)
  mat_H<- goethals_seidel_array(A,B,C,D)
  return(mat_H)
}
