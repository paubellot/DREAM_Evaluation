#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericMatrix e2l(NumericMatrix gold_positives, NumericVector regulators, 
    NumericVector targets,int nrow,int ncol){
    int np=gold_positives.nrow();  
    int i,j; //nrow=std::max(regulators), ncol=std::max(targets);
    NumericMatrix A(nrow,ncol),G(nrow,ncol);
    std::fill(A.begin(), A.end(), 0);
    std::fill(G.begin(), G.end(), 0);
    for(int k=0;k<np;k++){
        i=gold_positives(k,0)-1;
        j=gold_positives(k,1)-1;
        A(i,j)=1;
    }
    for(int k=0;k<regulators.size();k++){
        i = regulators(k)-1;
        for (int l=0;l<targets.size();l++){
            j = targets(l)-1;
            if(A(i,j)==1){
                G(i,j)=1;
            }else{
                 if(i!=j){
                     G(i,j)=-1;
                 }
            }
        }
    }
    return G;
}
