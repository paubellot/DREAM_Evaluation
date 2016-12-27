# 
#' Load dream network
#' This function strips-off the "G" characters from the gene identifiers 
#' @param A data.frame matrix that contains the edgelist with gene identifiers 
#'
#' @return Edgelist matrix matrix 
#' @export
#'
#' @examples
#' net <- load_dream_network(data.frame(TF=c("G1","G3"),TG=c("G2","G1"),w=c(1,0.5)))
load_dream_network <- function(A){
    N <- dim(A)[1];
    B <- apply(A[,1:2],2,.stripoff)
    cbind(B,as.double(A[,3]))

}

.stripoff<-function(x){
    x <- as.character(x)
    ## split string at non-digits
    s <- strsplit(x, "[^[:digit:]]")
    ## convert strings to numeric ("" become NA)
    solution <- as.numeric(unlist(s))
    ## remove NA and duplicates
    solution[!is.na(solution)]
}
