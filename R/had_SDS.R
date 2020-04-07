#' had_SDS
#'
#' had_SDS performs the construction of Hadamard matrix from SDS.
#'
#' @param x integer (order of the matrix)
#' @return Hadamard matrix of order x
#' @export
#'
#' @details
#' This function construct the Hadamard matrix of given order can be constructed using Suplementary Diffrence sets.
#' For given order the SDS set is not available it retuns NULL
#' If SDS is available it Returns Hadamard matrix of given order.
#' @details SDS are available for 103,127,151,163,181,191,239,251,463,631 in the internal table.
#'
#' @references Djokovic, D. Z. (1992a). Skew Hadamard matrices of order 4x37 and 4x39. J. Combin. Theory, A 61, 319-321.
#' @references Djokovic, D. Z. (1992b). Construction of some new Hadamard matrices. Bull. Austral. math. Soc., 45, 327-332.
#' @references Djokovic, D. Z. (1992c). Ten new Hadamard matrices of skew type. Publ.Electrotechnickog Fak., Ser. Mathematika, Univ. of Belgrade, 3, 47-59.
#' @references Djokovic, D. Z. (1992d). Ten Hadamard matrices of order 1852 of Goethals-Seidel type. Europ. J. Combinatorics, 13, 245-248.
#' @references Djokovic, D. Z. (1994a). Two Hadamard matrices of order 956 of Goethals-Seidel type. Combinatorica, 14(3), 375-377.
#' @references Djokovic, D. Z. (1994b). Five new Hadamard matrices of order skew type. Austral. J. Combinatorics, 10, 259-264.
#'
#' @examples
#' \donttest{
#' had_SDS(412)
#' }
#' @examples
#' \donttest{
#' had_SDS(508)
#' }




had_SDS<-function(x){
  r<-x/4
  A<-matrix(1,nrow = r,ncol = r)
  B<-matrix(1,nrow = r,ncol = r)
  C<-matrix(1,nrow = r,ncol = r)
  D<-matrix(1,nrow = r,ncol = r)

  sds<- subset(SDS,SDS$N==r)
  a<-subset(sds$Value,sds$SetNo==1)
  b<-subset(sds$Value,sds$SetNo==2)
  c<-subset(sds$Value,sds$SetNo==3)
  d<-subset(sds$Value,sds$SetNo==4)

  n1<-length(a)
  n2<-length(b)
  n3<-length(c)
  n4<-length(d)

  for(i in 1:r){
    for(j in 1:r){
      A[i,j]<-Initial_row_SDS(i-1,j-1,a,n1,r)
      B[i,j]<-Initial_row_SDS(i-1,j-1,b,n2,r)
      C[i,j]<-Initial_row_SDS(i-1,j-1,c,n3,r)
      D[i,j]<-Initial_row_SDS(i-1,j-1,d,n4,r)
    }
  }
  return(goethals_seidel_array(A,B,C,D))
}
