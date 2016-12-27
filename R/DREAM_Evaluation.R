#' DREAM GRN Evaluation 
#'
#' This function allows you to evaluate a GRN inference against golden standard (GS).
#' @param gold_positives matrix with three columns that contains the GS in the form 
#' of edgelist.
#' @param predict_raw  matrix with three columns that contains the inferred GRN in the form 
#' of edgelist
#' @return list containing the metrics [TPR FPR PREC REC L AUROC AUPR] 
#' TPR: vector with True Positive Ratio
#' FPR: vector with False Positive Ratio
#' PREC: vector with Precision
#' REC: vector with Recall
#' L: prediction list length
#' AUROC: area under ROC
#' AUPR: area under PR
#' @keywords main
#' @export
#' @examples
#' net <- matrix(rnorm(100),10,10)
#' GS <- matrix(rnorm(100),10,10)
#' GS[which(GS>0.5)]<-1
#' GS[GS!=1] <- 0
#' diag(GS) <- 0
#' net <- change_network_format(net)
#' GS <- change_network_format(GS)
#' Ev <- DREAM_Evaluation(GS,net)

DREAM_Evaluation <- function(gold_positives, prediction_raw){
    if(!is.numeric(gold_positives)){
        gold_positives <- load_dream_network(gold_positives)
    }
    if(!is.numeric(prediction_raw)){
        prediction_raw <- load_dream_network(prediction_raw)
    }
    idx <- which(gold_positives[,3]==1);
    gold_positives <- gold_positives[idx,];    #make sure they are really positives
    
    aux <- edgelist2gs(gold_positives)
    gold_complete <- aux[["gold_complete"]]
    G <-  aux[["G"]]
    H <- Matrix::Matrix(G>0,sparse=TRUE);	# just the positives
    ## total, positive, negative
    P <- sum(G>0);
    N <- sum(G<0);
    Tot <- P + N;
    ## preprocess prediction so only contains edges in the gold standard
    prediction <- remove_edges_not_in_gs(prediction_raw,G);
    L <- min(dim(prediction)[1],100000)
    ##create the "discovery" vector, the order with which positves are discovered
    I <- prediction[,1];
    J <- prediction[,2];
    discovery <- rep(0,L);
    for(k in seq_len(L)){
        i <- I[k];
        j <- J[k];
        discovery[k]<- H[i,j];
    }    
    #number of positives discovered in the prediction list of length L
    TPL <- sum(discovery);
    ## p = the number of remaining positives / number of remaining list entries.
    ## p is the uniform probablility of picking a positive by guessing.
    if (L < Tot){
        p <- (P - TPL) / (Tot - L);
        random_positive_discovery <- rep(p,Tot-L)
        random_negative_discovery <- rep(1-p,Tot-L)  
        # "complete" the discovery vectors
        positive_discovery <- c(discovery,random_positive_discovery);
        negative_discovery <- c(!discovery,random_negative_discovery);
        K = seq(1,Tot);
    }else{
        p <- 0;    # they were all already found
        # "complete" the discovery vectors
        positive_discovery <- discovery
        negative_discovery <- !discovery
        K = seq(1,L)
    }
    # p is also the fractional number of positives discovered per guess,
   
    #true positives (false positives) at depth k
    TPk = cumsum(positive_discovery);
    FPk = cumsum(negative_discovery);
    
    # metrics
    TPR = TPk / P;
    FPR = FPk / N;
    REC = TPR;  # same thing
    PREC = TPk / K;
    # sanity check 
    # if ((P != round(TPk[end(TPk)[1]]) || (N != round(FPk[end(FPk)[1]])))){
    #     stop('ERROR. There is a problem with the completion of the prediction list.')
    # }
    # finishing touch
    TPk[end(TPk)[1]] <- round(TPk[end(TPk)[1]]);
    FPk[end(FPk)[1]] <- round(FPk[end(FPk)[1]]);
    AUROC <- pracma::trapz(FPR,TPR);
    AUPR <- pracma::trapz(REC,PREC) / (1-1/P); #normalized by max possible value.
    list(TPR=TPR,FPR=FPR,PREC=PREC,REC=REC,L=L,AUROC=AUROC,AUPR=AUPR)
} 
