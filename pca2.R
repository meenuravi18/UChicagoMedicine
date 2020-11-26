
library(rgl)
library(ggplot2)
ptm <- proc.time()
setwd("C:/Users/meenu/OneDrive/UCM")
data <- read.csv("cot_pca.csv")

normalize<- function(x)
{
  return ((x-mean(x))/sd(x))
}

norm_data<- data %>% 
  apply(2,normalize) %>%
  as.data.frame
head(norm_data)



cor_mat<-cor(norm_data)
cor_mat
eig<-eigen(cor_mat)
eigen_vals<-eig$values
eigen_vals
eigen_vec<-eig$vectors
eigen_vec
sorted_eigenvals<- sort(eigen_vals,decreasing=T)
sorted_eigenvals
var_exp<-sorted_eigenvals/sum(eigen_vals)
var_exp
cum_sum<-cumsum(var_exp)
cum_sum

trans_matrix<-eigen_vec[,1:3]
trans_matrix
trans_data<-as.matrix(norm_data)%*% trans_matrix
head(trans_data)


fit <- hclust(dist(trans_data[,1:3]), method="complete") # 1:3 -> based on 3 components
groups <- cutree(fit, k=6)                            # k=5 -> 5 groups

plotPCA <- function(x, nGroup) {
  n <- ncol(x)
  if(!(n %in% c(2,3))) { # check if 2d or 3d
    stop("x must have either 2 or 3 columns")
  }
  
  fit <- hclust(dist(x), method="complete") # cluster
  groups <- cutree(fit, k=nGroup)
  
  if(n == 3) { # 3d plot
    plot3d(x, col=groups, type="s", size=1, axes=F)
    axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
    grid3d("x")
    grid3d("y")
    grid3d("z")
  } else { # 2d plot
    maxes <- apply(abs(x), 2, max)
    rangeX <- c(-maxes[1], maxes[1])
    rangeY <- c(-maxes[2], maxes[2])
    plot(x, col=groups, pch=19, xlab=colnames(x)[1], ylab=colnames(x)[2], xlim=rangeX, ylim=rangeY)
    lines(c(0,0), rangeX*2)
    lines(rangeY*2, c(0,0))
  }
}
plotPCA(trans_data[,1:3], 6)
proc.time() - ptm