
library(reshape)
library(data.table)
library(dplyr)
library(sqldf)
library(slam)
library(igraph)

rm(list=ls())
qpca <- function(A,rank=0){
  A.svd <- svd(A)
  if(rank>0&rank<ncol(A)){
  lambda <- A.svd$d[rank]*0.99 + A.svd$d[rank+1]*0.01
  } else {
  lambda <- 0
  }
  d <- A.svd$d-lambda
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2;
  prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop)
}

#generate insight
clabel <- matrix(rnorm(100),25,4,dimnames=list(paste0('c',1:25),paste0('l',1:4)))
clabel <- sign(clabel) * (abs(clabel)>1)
clabel[rowSums(clabel!=0)==0,1] <- 1
clabel_bk <- clabel
clabel <- filter(reshape::melt(clabel),value!=0)
colnames(clabel)[1:2] <- c('copy','label')
ulabel <- matrix(rnorm(10000),2500,4,dimnames=list(paste0('u',1:2500),paste0('l',1:4)))
ulabel_bk <- ulabel <- sign(ulabel) * (abs(ulabel)>1)
ulabel <- filter(reshape::melt(ulabel),value!=0)
colnames(ulabel)[1:2] <- c('user','label')

#generate transaction level data
trans <- sqldf(
  'select a.user, b.copy
  from ulabel a left join clabel b
  where a.label = b.label and a.value = b.value
  '
)
trans <- unique(trans)
trans <- trans[sample(1:nrow(trans),nrow(trans)*0.1),]

#copy matrix
ccscript <- sqldf(
  'select a.copy as copyi, b.copy as copyj, count(1) as count
  from trans a left join trans b
  where a.user = b.user
  group by a.copy, b.copy'
)
ccmat <- slam::simple_triplet_matrix(
  match(ccscript$copyi,paste0('c',1:25)),
  match(ccscript$copyj,paste0('c',1:25)),
  ccscript$count
)
dimnames(ccmat) <- list(paste0('c',1:25),paste0('c',1:25))
ccmat <- as.matrix(ccmat)
ccmat <- ccmat[rowSums(ccmat)>0,colSums(ccmat)>0]
ccmat <- t(ccmat/diag(ccmat))
diag(ccmat) <- 0
ccmat <- scale(ccmat)

dimnames(ccmat) <- list(
  paste(apply(clabel_bk,1,paste0,collapse=' ')),
  paste(apply(clabel_bk,1,paste0,collapse=' ')))
plot(g <- igraph::graph_from_adjacency_matrix(ccmat>1.96),
     edge.arrow.size=.3,
     vertex.size=5,
     vertex.label.cex=1)

#plot clust
fc <- function(x){
  fc <- membership(fastgreedy.community(as.undirected(x)))
  fc[] <- match(fc,unique(fc))
  fc
}
plotclust <- function(G){
  membership <- fc(G)
  plot(create.communities(G, membership), 
       # as.undirected(G), 
       as.directed(G),
       layout=layout.kamada.kawai(as.undirected(G)),
       edge.arrow.size=.1,
       vertex.size=3,
       vertex.label.cex=1,
       edge.width=.1)
  return(membership)
}
plotclust(g)
