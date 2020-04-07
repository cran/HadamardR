#' had_goethals_base
#'
#' had_goethals_base performs the construction of Hadamard Matrix from Goethals-Seidel method.
#' by using the Base sequences.
#'
#' @param x integer (order of the matrix)
#' @return Hadamard matrix of order x
#' @export
#' @details This function construct the Hadamard matrix of given order using base sequences.
#' If base sequences of length n+1,n+1,n,n are available, base sequences are converted into T-sequences of length 2n+1,2n+1,2n+1,2n+1 can be constructed.
#' From T-sequence of length 2n+1, Hadamard matrix of order 4(2n+1) can be constructed.
#' For a given order the base sequences is not available it returns NULL.
#' @details The Base sequences are stored in internal dataset. The available
#' Base sequences of length is 1,2,3,4,.....,35
#' @source
#' The Base sequences were obtained from
#' \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#' @references
#' Goethals, J. M. and Seidel, J. J. (1967). Orthogonal matrices with zero diagnol. Canad. J. Math., 19, 259-264.
#'
#' @examples
#' had_goethals_base(12)
#' #       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
#' # [1,]    1    1    1    1    1   -1    1   -1   -1    -1     1    -1
#' # [2,]    1    1    1    1   -1    1   -1   -1    1     1    -1    -1
#' # [3,]    1    1    1   -1    1    1   -1    1   -1    -1    -1     1
#' # [4,]   -1   -1    1    1    1    1    1   -1   -1     1    -1     1
#' # [5,]   -1    1   -1    1    1    1   -1   -1    1    -1     1     1
#' # [6,]    1   -1   -1    1    1    1   -1    1   -1     1     1    -1
#' # [7,]   -1    1    1   -1    1    1    1    1    1     1     1    -1
#' # [8,]    1    1   -1    1    1   -1    1    1    1     1    -1     1
#' # [9,]    1   -1    1    1   -1    1    1    1    1    -1     1     1
#' #[10,]    1   -1    1   -1    1   -1   -1   -1    1     1     1     1
#' #[11,]   -1    1    1    1   -1   -1   -1    1   -1     1     1     1
#' #[12,]    1    1   -1   -1   -1    1    1   -1   -1     1     1     1
#' @examples
#' had_goethals_base(16)
#' #NULL

had_goethals_base<- function(x){
  torder<-x/4
  order<- (torder-1)/2
  dat<- baseseq(order)
  if(nrow(dat)==0){
    return(NULL)
  }
  Tt<-base_to_T(dat,order)
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
