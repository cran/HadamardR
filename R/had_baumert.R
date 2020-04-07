#' had_baumert
#'
#' had_baumert performs the constrution of Hadamard matrix by Baumert-Hall method.
#'
#' @param n  integer (order of the matrix)
#' @return Hadamard matrix of order n
#' @export
#' @details
#' Baumert-Hall arrays extension of the williamson array. For construction of matrix
#' it requires the Williamson sequences.For different order of the matrix it requries
#' different williamson sequences.If williamson sequences are not available it Returns NULL.
#' @details
#' Williamson sequences are available for length of seq(1,63, 2) except 15, 35, 47, 53, 59 in the internal table.
#'
#' @source
#' The Williamson sequences are available in London (2013) and
#' \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#' @references
#' Baumert, L. D., and Hall, M. Jr. (1965). A new construction method for Hadamard matrices.
#' Bull. Amer. Math. Soc., 71, 169-170
#' @references
#' Hedayat, A. and Wallis, W.D. (1978). Hadamard Matrices and Their Application. Ann. Stat., 6, 1184-1238.
#' @references
#' London, S. 2013. Constructing New Turyn Type Sequences, T-Sequences and Hadamard Matrices. PhD Thesis, University of Illinois at Chicago, Chicago.
#'
#' @examples
#' \donttest{
#' had_baumert(372)
#' }
#' #Big matrix.
#' @examples
#' had_baumert(24)
#' #NULL



had_baumert <- function(n){
  order<- n/12
  dat<- seq_williamson(order)
  if(nrow(dat)==0){
    return(NULL)
  }
  a1 <- subset(dat$Value,dat$Matrix==1)
  a2 <- subset(dat$Value,dat$Matrix==2)
  a3 <- subset(dat$Value,dat$Matrix==3)
  a4 <- subset(dat$Value,dat$Matrix==4)


  A<- circulant_mat(matrix(a1))
  B<- circulant_mat(matrix(a2))
  C<- circulant_mat(matrix(a3))
  D<- circulant_mat(matrix(a4))


mat_H<- rbind(cbind( A, A, A, B,-B, C,-C,-D, B, C,-D,-D),
              cbind( A,-A, B,-A,-B,-D, D,-C,-B,-D,-C,-C),
              cbind( A,-B,-A, A,-D, D,-B, B,-C,-D, C,-C),
              cbind( B, A,-A,-A, D, D, D, C, C,-B,-B,-C),
              cbind( B,-D, D, D, A, A, A, C,-C, B,-C, B),
              cbind( B,C, -D, D, A, -A, C, -A, -D,C,B, -B),
              cbind( D,-C, B,-B, A,-C,-A, A, B, C,D,-D),
              cbind( -C,-D,-C,-D,C, A,-A,-A,-D, B,-B, -B),
              cbind( D, -C, -B, -B, -B, C, C, -D, A, A, A, D),
              cbind(-D,-B,C,C,C,B,B,-D,A,-A,D,-A),
              cbind( C, -B, -C, C, D, -B, -D, -B, A, -D, -A, A),
              cbind( -C,-D,-D, C,-C,-B, B, B, D, A, -A, -A))
return(mat_H)
}
