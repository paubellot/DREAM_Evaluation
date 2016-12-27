remove_edges_not_in_gs <- function(prediction,G){
    ## prediction is the three column edge list
    ## G is the gold graph with {1, -1} positive, negative
    d <- dim(G)
    tf <- which(apply(G,1,function(x)sum(abs(x)))!=0)
    y <- apply(prediction,1,function(x) if(is.element(x[1],tf) && x[2]<=d[2]){x})
    prediction_cleaned <- matrix(unlist(y),ncol=3,byrow = T)
    #prediction_cleaned <- rm_not_gs(prediction,cleaned, G,regulators, targets)
    #prune the NaNs
    idx <- which(!is.nan(prediction_cleaned[,1]));
    prediction_cleaned[idx,]
}
