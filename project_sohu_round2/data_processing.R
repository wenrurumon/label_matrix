
library(data.table)
library(dplyr)
library(bit64)
library(sqldf)
library(slam)
library(igraph)

rm(list=ls())

checklagid <- function(vec){
	c(sapply(2:length(vec)-1,function(i){
			vec[i]==vec[i+1]
		}),TRUE)
}
qpca <- function(A,rank=0){
  A <- scale(A)
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-10]
  r <- length(d)
  prop <- d^2; prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop)
  return(rlt)
}

#########################

setwd("/home/huzixin/Documents/sohu_project/sample_data/al")
al <- do.call(rbind,lapply(dir(),fread))
colnames(al) <- strsplit('alid,cname,cclass,cregion',',')[[1]]


#########################

setwd('/home/huzixin/Documents/sohu_project/sample_data/')
x <- lapply(dir()[1:2*2],function(d){
	print(d)
	x <- fread(d)
	x <- filter(x,V3!=0)
	x
	})
x <- do.call(rbind,x)
colnames(x) <- strsplit('uid,time,cid,dur,adclick,adid,rid,vid',",")[[1]]

x <- arrange(x,uid,time)
x.difftime <- c(diff(as.POSIXlt(x$time)),Inf)
x <- filter(cbind(x,difftime=x.difftime,align_uid=checklagid(x$uid),dur_sel=x$dur/3<x.difftime),dur_sel)
#x <- cbind(x,difftime=x.difftime,align_uid=checklagid(x$uid),dur_sel=x$dur/3<x.difftime)

	raw <- cbind(select(x,time,dur,uid),truedur=apply(select(x,dur,difftime),1,min),al[match(x$cid,al$alid)])
	map.uid <- filter(raw %>% group_by(uid) %>% summarise(n=n(),ncid=n_distinct(cname)),ncid>1&ncid<=33)
	map.cid <- filter(raw %>% group_by(cname) %>% summarise(n=n(),nuid=n_distinct(uid)),nuid>30)
	raw <- filter(raw,uid%in%map.uid$uid & cname%in%al$cname & cname%in%map.cid$cname)
	map.cid <- raw %>% group_by(cname) %>% summarise(mdur=median(truedur),sdur=sd(truedur))
	raw <- mutate(filter(cbind(select(raw,-cname),map.cid[match(raw$cname,map.cid$cname),]),truedur>mdur),score=as.numeric(truedur-mdur)/as.numeric(sdur))


#########################
#uid - cid mix

cidlist <- unique(raw$cname)
uidlist <- unique(raw$uid)
fmat_uc <- as.matrix(simple_triplet_matrix(match(raw$uid,uidlist),match(raw$cname,cidlist),as.numeric(raw$score)))
dimnames(fmat_uc) <- list(1:length(uidlist),cidlist)
fmat_uc <- fmat_uc[rowSums(fmat_uc>0)>=1,colSums(fmat_uc>0)>=30]
fmat_uu <- fmat_uc %*% t(fmat_uc)
fmat_cc <- t(fmat_uc) %*% (fmat_uc)

#########################
#Igraph clustering

fmat_cc_graphs <- graph_from_adjacency_matrix(fmat_cc>0,mode='undirected')
#fmat_cc_subgraphs <- components(fmat_cc_graphs)
fmat_cc_subgraphs <- igraph::fastgreedy.community(fmat_cc_graphs)
