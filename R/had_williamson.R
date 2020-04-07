#' had_williamson
#'
#' had_williamson performs the construction Hadamard matrix from Williamson method by using the
#' williamson sequences.
#'
#'
#' @param x integer (order of the matrix)
#' @return Hadamard matrix
#' @export
#' @details
#' This function construct Hadamard matrix of given order using williamson sequences.
#' If Williamson sequences of length n,n,n,n are available, Hadamard matrix of order 4n can be constructed.
#' If for given order of Matrix Williamson sequences are not available it retuns NULL.
#' @details The  Williamson sequences are stored in internal dataset,
#' available for length of seq(1,63, 2) except 15, 35, 47, 53, 59 in the internal table.

#' @source
#' The williamson sequences are available in London(2013) and
#' \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#' @references Williamson, J. (1944). Hadamard determinant theorem and the sum of four squares. Duke. Math. J., 11, 65-81.
#' @references Williamson, J. (1947). Note on Hadamard's determnant theorem. Bull. Amer. Math. Soc., 53, 608-613.
#' @references London, S. 2013. Constructing New Turyn Type Sequences, T-Sequences and Hadamard Matrices. PhD Thesis, University of Illinois at Chicago, Chicago.
#' @examples
#' had_williamson(4)
#' #      [,1] [,2] [,3] [,4]
#' #[1,]    1    1    1    1
#' #[2,]   -1    1   -1    1
#' #[3,]   -1    1    1   -1
#' #[4,]   -1   -1    1    1
#' had_williamson(8)
#' # NULL


had_williamson <- function(x){
  order<- x/4
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

  mat_H<-rbind(cbind(A,B,C,D),
               cbind(-B,A,-D,C),
               cbind(-C,D,A,-B),
               cbind(-D,-C,B,A))
  return(mat_H)
}
