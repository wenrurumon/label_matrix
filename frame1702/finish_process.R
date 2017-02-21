
rm(list=ls())
setwd('C:\\Users\\zhu2\\Documents\\sohu\\20170220')
library(data.table)
library(dplyr)
library(bit64)
library(sqldf)
library(slam)
library(igraph)

##############################
# macro
##############################

filter_odd_dur <- function(x){
  x$time <- as.POSIXct(x$time)
  x <- arrange(x,time)
  time2 <- c(diff(as.POSIXlt(x$time)),Inf)
  timefilter <- (time2>(x$dur/3))
  filter(x,timefilter)
}

summarise_by_uid <- function(x){
  # tsum <- x %>% group_by(uid) %>% summarise(n=n(),ncid=n_distinct(cid),dur=sum(dur))
  csum <- x %>% group_by(uid,cid) %>% summarise(n=n(),dur=sum(dur))
  # list(tsum=tsum,csum=csum)
  csum
}

processdata <- function(filename){
  print(filename)
  print(Sys.time())
  #load raw data
  x <- fread(filename,encoding='UTF-8',sep='\t')
  colnames(x) <- strsplit('uid,time,cid,dur,adclick,adid,rid,vid',",")[[1]]
  x <- filter(x,cid%in%al$alid)
  #filter uid with limited observations
  u.filter <- x %>% group_by(uid) %>% summarise(n=n_distinct(cid))
  u.filter <- filter(u.filter,n>=4&n<=45)$uid
  x <- lapply(u.filter,function(u){
    filter(x,uid==u)
  })
  #filter observation with ambiguous duration
  rlt <- lapply(x,filter_odd_dur)
  #output
  names(rlt) <- u.filter
  lapply(rlt,summarise_by_uid)
}

qpca <- function(A,rank=0,ifscale=TRUE){
  if(ifscale){A <- scale(as.matrix(A))[,]}
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
  return(rlt)
}

##############################
# load copymaster
# load('preload.rda')
##############################

setwd("al")

#Regular process
al <- do.call(rbind,lapply(dir(),function(x){
  x <- fread(x,encoding='UTF-8')
}))
colnames(al) <- strsplit('alid,cname,cclass,cregion',',')[[1]]
#I do it in this way because some of the language problem on my laptop
al2 <- do.call(c,lapply(dir(),function(x){readLines(x,encoding='UTF-8')}))
al2 <- do.call(rbind,strsplit(al2,'\t'))
colnames(al2) <- strsplit('alid,cname,cclass,cregion',',')[[1]]

##############################
# load pageview record 
##############################

setwd('..');setwd('data')

#Load data, do it in parallel instead of lapply
raw <- lapply(dir(),processdata)
raw <- do.call(c,raw)

##############################
# copy matrix
##############################

#prepare data for copy matrix

raw2 <- do.call(rbind,raw)
c.filter <- raw2 %>% group_by(cid) %>% summarise(n=n_distinct(uid))
c.filter <- filter(c.filter,n>=10)$cid
raw2 <- filter(raw2,cid%in%c.filter)

#go for copy matrix

cmat <- sqldf('select a.cid as cidi, b.cid as cidj, sum(a.dur) as duri, sum(b.dur) as durj
              from raw2 a left join raw2 b
              on a.uid = b.uid
              group by cidi,cidj;')

cmat$cidi <- match(cmat$cidi,c.filter)
cmat$cidj <- match(cmat$cidj,c.filter)
almap <- al2[match(c.filter,al$alid),]
cmat <- mutate(cmat,score=as.numeric(duri)+as.numeric(durj))

##############################
# Label Matrix
##############################

cmax <- simple_triplet_matrix(cmat$cidi,cmat$cidj,cmat$score)
cmax <- (cmax <- as.matrix(cmax)) / outer(diag(cmax),diag(cmax),'+') * 2

