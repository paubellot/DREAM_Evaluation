#include <Rcpp.h>
using namespace Rcpp;

// Remove links not in TF list of GS

// [[Rcpp::export]]
NumericMatrix rm_not_gs(NumericMatrix prediction,NumericMatrix cleaned, NumericMatrix G,NumericVector regulators, 
    NumericVector targets){
   int  count = 0,i,j;
   
   //NumericMatrix cleaned(np,3);
   int g1 =G.nrow(), g2=G.ncol();
   //std::fill(cleaned.begin(), cleaned.end(), NumericVector::get_na());
  for(int k=0;k<prediction.nrow();k++){
        i = prediction(k,0);
        j = prediction(k,1);
        if((i <= g1) && (j <= g2)){
            //in the range of the gold standard
            if(G(i-1,j-1)!=0){
                // actually in the gold standard
                cleaned(count,0) = prediction(k,0);
                cleaned(count,1) = prediction(k,1);
                cleaned(count,2) = prediction(k,2); 
                count ++;
            }
        }
    }
   return cleaned;
}
