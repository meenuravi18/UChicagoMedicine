
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
// [[Rcpp::export]]

DataFrame normalize(DataFrame x) {
  DataFrame ret=x;
  
  for (int i=0;i<x.size();i++){
    NumericVector A= x[i];
    float mean_=mean(A);
    float sdev=sd(A);
    //Rcout<<mean_<<", "<<sdev<<"\n";
    for (int j=0;j<A.size();j++){
      NumericVector temp=ret[i];
      temp[j]=(A[j]-mean_)/sdev;
      
    }
    
  }
  return ret;
  
}
// [[Rcpp::export]]
NumericMatrix dfToMatrix(DataFrame x) {
  Function asMatrix("as.matrix");
  return asMatrix(x);
}
// [[Rcpp::export]]
arma::mat convertDataFrame(NumericMatrix nm){
    arma::mat y = as<arma::mat>(nm) ;
    return(y) ;
}
// [[Rcpp::export]]
arma::vec getEigenValues(arma::mat M) {
  arma::vec eigval = sort(arma::eig_sym(M), "descend");
  return eigval;
}
// [[Rcpp::export]]
arma::mat getEigenVectors(arma::mat M) {
  arma::vec V;
  arma::mat eigvectors;
  arma::eig_sym(V, eigvectors, M);
  arma::mat ret=M;
  unsigned int ctr=M.n_cols-1;
 
  for(unsigned int i=0;i<M.n_cols;i++){
    ret.col(i)=eigvectors.col(ctr);
    ctr=ctr-1;
  }
  return ret;

}
// [[Rcpp::export]]
arma::vec getVarExp(arma::mat M){
  arma::vec res=M;
  float sum_=sum(res);
  for (unsigned int i=0;i<M.size();i++){
    res[i]=res[i]/sum_;
  }
  return res;
}
// [[Rcpp::export]]
arma::vec getCumSum(arma::mat M){
  arma::vec res=M;
  float sum_=0;
  for (unsigned int i=0;i<M.size();i++){
    sum_=M[i]+sum_;
    res[i]=sum_;
  }
  return res;
}
// [[Rcpp::export]]
arma::mat getTransMatrix(arma::mat M,arma::mat norm){
  arma::mat res(M.n_rows,3);
  for (unsigned int i=0;i<3;i++){
    res.col(i)=M.col(i);
  }
  
  return norm*res;
}
/*** R
ptm <- proc.time()
setwd("C:/Users/meenu/OneDrive/UCM")
dataPath<-"C:/Users/meenu/OneDrive/UCM"
data <- read.csv("cot_pca.csv")
features<-c("A")
normalized<-normalize(data)
norm_matrix<-dfToMatrix(normalized)
nnorm_matrix<-convertDataFrame(norm_matrix)
head(nnorm_matrix)
cor_mat<-cor(normalized)
corr_matrix<-dfToMatrix(cor_mat)

ncorr_matrix<-convertDataFrame(corr_matrix)
ncorr_matrix
eig_values<-getEigenValues(ncorr_matrix)

var_exp<-getVarExp(eig_values)
cum_sum<-getCumSum(var_exp)
eig_vectors<-getEigenVectors(ncorr_matrix)

trans_matrix<-getTransMatrix(eig_vectors,norm_matrix)
head(trans_matrix)
proc.time() - ptm
*/