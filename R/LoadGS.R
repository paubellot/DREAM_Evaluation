#' Load Golden standard data
#'
#' @param net integer identifier of the network 
#'
#' @return l
#' @export
#'
#' @examples
#' Data <- LaodGS(1)
LoadGS <-function(net =1){
  if(net>=1 && net<=5){
    Data <- data(paste0("D4net",net))
  }else{
    stop("net shoud be between 1 and 5")
  }
   return(Data)
}