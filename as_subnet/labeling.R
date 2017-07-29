
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

#label one
clabel2 <- clabel_bk
for(i in 1:nrow(clabel_bk)){
  sel <- which(clabel_bk[i,]!=0)
  clabel2[i,sample(sel,length(sel)-1)] <- 0
}
clabel2 <- filter(reshape::melt(clabel2),value!=0)
colnames(clabel2)[1:2] <- c('copy','label1')

#label all the trans by label1
trans_label <- sqldf(
  'select a.user, a.copy, b.label1, b.value
  from trans a left join clabel2 b
  on a.copy = b.copy
  group by a.user, a.copy, b.label1'
)
ulabel1 <- reshape::melt(ulabel_bk);colnames(ulabel1)[1:2] <- c('user','label')
ulabel2 <- trans_label %>% group_by(user,label1) %>% summarise(value=mean(value))
ulabel2 <- sqldf(
  '
  select a.user, a.label, a.value as val1, b.value as val2
  from ulabel1 a left join ulabel2 b
  on a.user = b.user and a.label = b.label1
  group by a.user, a.label
  '
)
