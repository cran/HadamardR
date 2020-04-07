#' Hadamard_Matrix_method
#'
#' Hadamard_Matrix_method it is also generic function but it provides some additional options.
#'
#' @param order  integer
#' @param type -1 or 0
#' @param method method=c("Kronecker", "PaleyI","PaleyII","Ehlich","Williamson","Baumert","Goethals-Seidel_Base",
#' "Goethals-Seidel_Turyn","Miyamoto","Cooper-Wallis","Kronecker_Product_Method","PaleyIPrimepower","PaleyIIPrimePower")
#' @param file  Name of the file
#' @param filetype xlsx or csv
#' @return
#' Hadamard Matrix of given Order
#' @export
#' @details If the method is not specified or incorrectly specified,
#' Hadamard matrix will be constructed using Had_method function.
#' If the method is specified, Hadamard matrix will be constructed using that method.
#' @details By default, the elements will be +1 or -1. Incase, -1 should be replaced by 0,
#'  use type=0.
#' @details TO save the generated matrix into a text file (csv) or MS-Excel,
#' filename may be specified (with extension). In case Excel file required,
#' use filetype = xlsx, otherwise csv file will be generated.
#' @details If just give the input as number it returns Hadamard matrix in console.
#'
#' @examples
#' Hadamard_matrix_method(4)
#' #      [,1] [,2] [,3] [,4]
#' #[1,]    1    1    1    1
#' #[2,]    1   -1    1   -1
#' #[3,]    1    1   -1   -1
#' #[4,]    1   -1   -1    1
#' @examples
#' Hadamard_matrix_method(8,method = "PaleyI")
#' #       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
#' # [1,]    1    1    1    1    1    1    1    1
#' # [2,]   -1    1   -1   -1    1   -1    1    1
#' # [3,]   -1    1    1   -1   -1    1   -1    1
#' # [4,]   -1    1    1    1   -1   -1    1   -1
#' # [5,]   -1   -1    1    1    1   -1   -1    1
#' # [6,]   -1    1   -1    1    1    1   -1   -1
#' # [7,]   -1   -1    1   -1    1    1    1   -1
#' # [8,]   -1   -1   -1    1   -1    1    1    1
#' @examples
#' \donttest{
#' Hadamard_matrix_method(12,method = "Williamson",
#'  file =  file.path(tempdir(), "Hadamard12.csv"))
#' #output saved in file
#' }
#' @examples
#' \donttest{
#' Hadamard_matrix_method(36,method = "Baumert",
#' file = file.path(tempdir(),"Hadamard36.xlsx"))
#' #output saved in file
#' }
#' @examples
#' \donttest{
#' Hadamard_matrix_method(20,method = "Miyamoto",
#' file = file.path(tempdir(),"Hadamard20.csv"),filetype = "csv")
#' #output saved in file
#' }
#' @examples
#' \donttest{
#' Hadamard_matrix_method(8,method =
#' "Kronecker",file = file.path(tempdir(),"Hadamard8.xlsx"), filetype = "xlsx")
#' #output saved in file
#' }


Hadamard_matrix_method<-function(order,type=-1,method="",file="",filetype=""){
  if (order==1){
    return(1)
  }
  if (order==2){
    h<-had_kronecker(n=order)
    return(h)
  }
  if(numbers::mod(order,4)!=0){
    return("order should be multiple of 4")
  }
  else if(order==668){
    return("No Construction Method Available")
  }
  else if(order==716){
    return("No Construction Method Available")
  }
  else if(order==892){
    return("No Construction Method Available")
  }
  #By kronecker method(power of 2)
  else if(method=="Kronecker"){
    h<-had_kronecker(order)
  }
  #By Paley I
  else if(method=="PaleyI"){
    h<-PaleyI(order)
  }
  #By Paley II
  else if(method=="PaleyII"){
    h<-PaleyII(order)
  }
  #By Ehlich method
  else if(method=="Ehlich"){
    h<-had_ehlich(order)
  }
  #By williamson method
  else if(method=="Williamson"){
    h<-had_williamson(order)
  }
  #By Baumert-Hall method
  else if(method=="Baumert"){
    h<-had_baumert(order)
  }
  #By Goethals-Seidel method (using Base sequences)
  else if(method=="Goethals-Seidel_Base"){
    h<-had_goethals_base(order)
    #return(h)
  }
  #By Goethals-Seidel method (using Turyn sequences)
  else if(method=="Goethals-Seidel_Turyn"){
    h<-had_goethals_Turyn(order)
  }
  #By Miyamoto method
  else if(method=="Miyamoto"){
    h<-had_miyamoto(order)
  }
  #By SDS method
  else if(method=="SDS"){
    h<-had_SDS(order)
    #return(h)
  }
  #By Cooper-Wallis method
  else if(method=="Cooper-Wallis"){
    h<-had_cooper(order)
  }
  #By Kronecker product method
  else if(method=="Kronecker_Product_Method"){
    h<-kronecker_matrix(order)
  }
  #By Goethals-Seidel method (using T sequences)
  else if(method=="Goethals-Seidel_T"){
    h<-had_goethals_T(order)
  }
  #By PaleyIPrimepower
  else if(method=="PaleyIPrimepower"){
    h<-PaleyIPrimePower(order)
  }
  #By PaleyIIPrimepower
  else if(method=="PaleyIIPrimepower"){
    h<-PaleyIIPrimePower(order)
  }
  else
    h<-Hadamard_Matrix(order)
  if (is.null(h)){
    return(cat("Not possible to construct Hadamard matrix of order %d", order))
  }
  if (type==0){
    h<-replace(h,h==-1,0)
  }
  if (!(missing(file))){
    h<-as.data.frame(h)
    if (filetype=="xlsx"){
      file<-openxlsx::write.xlsx(h,file,colNames=FALSE)
      return(cat("output saved in file"))
    }
    else {
      file<-utils::write.table(h,sep=",",file,col.names = FALSE,row.names = FALSE)
      return(cat("output saved in file"))
    }
  }
  return(h)
}
