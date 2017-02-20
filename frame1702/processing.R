
rm(list=ls())
setwd('C:\\Users\\WenluluSens\\Documents\\Project\\SohuVedio')
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
  #load raw data
  x <- fread(filename,encoding='UTF-8',sep='\t',nrow=100000)
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

##############################
# load copymaster
##############################

setwd("al")
al <- do.call(rbind,lapply(dir(),function(x){
  x <- fread(x,encoding='UTF-8')
}))
colnames(al) <- strsplit('alid,cname,cclass,cregion',',')[[1]]

##############################
# load pageview record 
##############################

setwd('..');setwd('data')

#Load data, do it in parallel instead of lapply
raw <- lapply(dir()[1:3],processdata)
raw <- do.call(c,raw)

##############################
# copy matrix
##############################

#prepare data for copy matrix

raw2 <- do.call(rbind,raw)
# raw2 <- filter(raw2,cid%in%al$alid)
raw2$cid <- al$cname[match(raw2$cid,al$alid)]#there is an encoding warning here from inte64 to numeric

#go for copy matrix

cmat <- sqldf('select a.cid as cidi, b.cid as cidj, sum(a.dur) as duri, sum(b.dur) as durj
              from raw2 a left join raw2 b
              on a.uid = b.uid
              group by cidi,cidj;')




filter(cmat,cidi%in%c('00后萝莉王巧','一囧漫画')&cidj%in%c('00后萝莉王巧','一囧漫画'))

##############################
# Label Matrix
##############################
