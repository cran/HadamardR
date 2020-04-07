#'had_cooper
#'
#' had_cooper performs the construction of Hadamard matrix
#' by Cooper-Wallis method.In which combines the williamson matrices and T-sequences.
#'
#'
#' @param n integer (order of the matrix=876)
#' @return Hadamard matrix of order n
#' @export
#'
#'
#' @source  The williamson sequences and Turyn sequences were obtained from
#' \href{http://www.math.ntua.gr/~ckoukouv/}{Christos Koukouvinos}
#'
#'
#' @references
#' Cooper, J. and Wallis, W. D. (1972). A construction for Hadamard arrays. Bull. Austral. Math. 7, 269-278.
#'


had_cooper<-function(n){
  if(numbers::mod(n,4)!=0){
    return(NULL)
  }
  order<-get_cooper(n)
  dat<-T_seq(order$m)
  if(nrow(dat)==0){
    return(NULL)
  }
  t1<- subset(dat$Value,dat$Matrix==1)
  t2<- subset(dat$Value,dat$Matrix==2)
  t3<- subset(dat$Value,dat$Matrix==3)
  t4<- subset(dat$Value,dat$Matrix==4)

  T1<- circulant_mat(matrix(t1))
  T2<- circulant_mat(matrix(t2))
  T3<- circulant_mat(matrix(t3))
  T4<- circulant_mat(matrix(t4))

  williorder<-seq_williamson(order$n)
  if(nrow(williorder)==0){
    return(NULL)
  }
  a1 <- subset(williorder$Value,williorder$Matrix==1)
  a2 <- subset(williorder$Value,williorder$Matrix==2)
  a3 <- subset(williorder$Value,williorder$Matrix==3)
  a4 <- subset(williorder$Value,williorder$Matrix==4)
  A<- circulant_mat(matrix(a1))
  B<- circulant_mat(matrix(a2))
  C<- circulant_mat(matrix(a3))
  D<- circulant_mat(matrix(a4))
  X<- T1%x%A + T2%x%B +T3%x%C +T4%x%D
  Y<- T1%x%-B + T2%x%A +T3%x%D +T4%x%-C
  Z<- T1%x%-C + T2%x%-D +T3%x%A +T4%x%B
  W<- T1%x%-D + T2%x%C +T3%x%-B +T4%x%A
  mat_H<- goethals_seidel_array(X,Y,Z,W)
  return(mat_H)
}
