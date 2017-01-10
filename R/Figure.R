#' Plot ROC or PR curve
#'
#' @param ev list containg the evaluation' metrics. Output of DREAM_Evaluation function
#' @param fig charater of the kind of plot ("pr" or "roc")
#' @param two logical indicating if the random extension should be marked or not
#' @param ... additional parameter to the plot
#'
#' @export
#'
#' @examples
#' net <- matrix(rnorm(100),10,10)
#' GS <- matrix(rnorm(100),10,10)
#' GS[which(GS>0.5)]<-1
#' GS[GS!=1] <- 0
#' diag(GS) <- 0
#' net <- change_network_format(net)
#' GS <- change_network_format(GS)
#' Ev <- DREAM_Evaluation(GS,net)
#' Figure(Ev)
Figure <- function(ev,fig="pr",two=FALSE,...){
  
    if(fig=="pr"){
        if(two){
            L<-ev$L
            plot(ev$REC[1:L],ev$PREC[1:L],type="l",
                 xlim=c(0,1),ylim=c(0,1),xlab="recall",ylab="precision",...)
            lines(ev$REC[-(1:L)],ev$PREC[-(1:L)],type="l",col="red",...)
        }else{
            plot(ev$REC,ev$PREC,type="l",
                 xlim=c(0,1),ylim=c(0,1),xlab="recall",ylab="precision",...)
        }
      
    }else if(fig=="roc"){
        if(two){
            L<-ev$L
            plot(ev$FPR[1:L],ev$TPR[1:L],type="l",
                 xlim=c(0,1),ylim=c(0,1),xlab="FPR",ylab="TPR",...)
            lines(ev$FPR[-(1:L)],ev$TPR[-(1:L)],type="l",col="red",...)
        }else{
            plot(ev$FPR,ev$TPR,type="l",
                xlim=c(0,1),ylim=c(0,1),xlab="FPR",ylab="TPR",...)
        }
    }
}
