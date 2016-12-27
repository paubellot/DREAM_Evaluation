.edgelist2gs <- function(gold_positives){
    regulators <- unique(gold_positives[,1]);
    targets <- unique(as.double(gold_positives[,1:2]));
    # lookup matrix for positive edges
    #A <- edgelist2sparse(gold_positives[,1:2]);
    G <- e2l(gold_positives, regulators,targets,nrow=max(regulators),
             ncol=max(targets))
    # complete gold standard edge list (positives and negatives)
    edge_count <- sum(G!=0);
    ## [r,c] = find(A > 5) 
    ## w = which(A > 5, arr.ind=TRUE);
    ## r=w[,1]; c=w[,2]
    w <- which(G>0,arr.ind=TRUE)
    I<- w[,1]; J<-w[,2] # positives
    w <- which(G<0,arr.ind=TRUE)
    K<- w[,1]; L<-w[,2] # negatives
    gold_complete <- rbind(cbind(I,J,rep(1,length(I))),
                           cbind(K,L,rep(0,length(K))));
    list(gold_complete=gold_complete,G=G)
}
