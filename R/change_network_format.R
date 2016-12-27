#' Change Network Format
#' 
#' This function changes a network from an adjecy matrix to an edgelist 
#' matrix and viceversa
#'
#' @param net matrix containing the network in the from of adjaceny matrix or edgelist
#' @param ng Total number of genes of the network. Defaults to NULL.
#'
#' @export
#' @examples
#' net <- matrix(rnorm(100),10,10)
#' Edge_list <- change_network_format(net)
#' net2 <- change_network_format(Edge_list)
change_network_format <- function(net,ng=NULL){
    if(diff(dim(net)) == 0){
        d <- dim(net)
        n_tf <- d[1]
        n <- d[2]
        temp<-(net>0)*1.0;
        num_e <- sum(temp);
        net_vec <- as.double(net);
        tmp <- sort(net,decreasing = TRUE,index.return=TRUE)
        ind=tmp$ix
        edge_ind_sorted <- matrix(0,2,length(I)) # it has two rows, sorted by weights
        r <- row(net)[ind]
        c <- col(net)[ind]
        net2 <- cbind(r,c,tmp$x)
        net2 <- net2[1:num_e,]
    }else if(dim(net)[2]==3){
        if(is.null(ng)){
            net2 <- as.matrix(Matrix::sparseMatrix(i=net[,1],j=net[,2],x=net[,3]))
        }else{
            net2 <- as.matrix(Matrix::sparseMatrix(i=net[,1],j=net[,2],x=net[,3],
                                                 dims = c(ng,ng)))
        }
    }
    return(net2)
}
