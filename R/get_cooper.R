#' get_cooper
#'
#' @description
#' This function provides the Williamson Matrix order and T-Sequence length required to construct Hadamard matrix.
#' @param x  integer
#' Hadamard Matrix Order to Check
#' @return m Tsequence order
#' @return n Williamson  order
#' @export
#' @details
#' If m is the order of T-Sequence and n is the order of Williamson sequence and both exists.
#' Cooper and Wallis (1972) showed a construction method for Hadamard matrix of order 4mn exists. This function returns
#' m and n if they exists otherwise NULL value is returned.
#'
#' @references
#' Cooper, J., and Wallis, J. 1972. A construction for Hadamard arrays. Bull. Austral. Math. Soc., 07: 269-277.
#' @examples
#' get_cooper(340)
#' #$m
#' #[1] 5
#' #$n
#' #[1] 17
#' @examples
#' get_cooper(256)
#' #NULL



get_cooper<-function(x){
  tseq<-unique(T_sequences$Order)
  williamson<-unique(williamson_sequences$WOrder)
  if (numbers::mod(x,4) !=0){
    return(list(m=NULL, n=NULL))
  }
  mn<-x/4
  for (i in 1:length(tseq)){
    if (mn %in% tseq[i]){
      return(list(m=mn, n=1))
    }
  }
  for (i in 1:length(williamson)){
    if (mn %in% williamson[i]){
      return(list(m=1, n=mn))
    }
  }
  for (i in 2:length(tseq)){
    if (is.null(is_divisible(mn,tseq[i]))==FALSE){
      for(j in 2:length(williamson)){
        if(mn/tseq[i]==williamson[j]){
          m<-tseq[i]
          n<-williamson[j]
          return(list(m=m,n=n))
        }
      }
    }
  }
  for (i in 2:length(williamson)){
    if (is.null(is_divisible(mn,williamson[i]))==FALSE){
      for(j in 2:length(tseq)){
        if(mn/williamson[i]==tseq[j]){
          m<-williamson[i]
          n<-tseq[j]
          return(list(m=m,n=n))
        }
      }
    }
  }
  return(list(m=NULL,n=NULL))
}
