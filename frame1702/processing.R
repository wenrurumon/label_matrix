
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



##############################
# load data
##############################

setwd("al")
al <- do.call(rbind,lapply(dir(),function(x){
  x <- fread(x,encoding='UTF-8')
}))
colnames(al) <- strsplit('alid,cname,cclass,cregion',',')[[1]]

setwd('..');setwd('data')

# Load and process the data into user level
# this is a map reduce proccedure

raw <- lapply(dir()[1:5],function(x){
  print(x)
  x <- fread(x,encoding='UTF-8',sep='\t',nrow=1000000)
  colnames(x) <- strsplit('uid,time,cid,dur,adclick,adid,rid,vid',",")[[1]]
  rlt <- lapply(unique(x$uid),function(v1){
    filter(x,uid==v1)
  })
  names(rlt) <- unique(x$uid)
  rlt
})
raw <- do.call(c,raw)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Pending: filtering process for the dummy records.
#for reducing the calcualtion cost, we can insert this step into the loading process
#I am thinking we can apply the abnormal sales methodology for dummy information filtering

raw2 <- lapply(raw,function(x){
  x <- arrange(x,time)
  time2 <- c(diff(as.POSIXlt(x$time)),Inf)
  timefilter <- (time2>(x$dur/3))
  filter(x,timefilter)
})

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# setwd('..')
# save(raw2,file='temp.rda')

# sumuid <- t(sapply(raw2,function(x){
#   x %>% summarise(n=n(),ncid=n_distinct(cid),dur=sum(dur))
# }))
praw <- do.call(rbind,raw2) #Assuming raw2 is the user level data after processing
sumuid <- praw %>% group_by(uid) %>% summarise(n=n(),ncid=n_distinct(cid),dur=mean(dur))
sumcid <- praw %>% group_by(cid) %>% summarise(n=n(),nuid=n_distinct(uid),dur=mean(dur))


#go for copy matrix

map_u_c <- as.data.frame(praw %>% group_by(uid,cid) %>% summarise(dur=sum(dur)))
map_u_c$cid <- al$cname[match(map_u_c$cid,al$alid)]
map_u_c <- filter(map_u_c,!is.na(cid))

cmat <- sqldf('select a.cid as cidi, b.cid as cidj, sum(a.dur) as duri, sum(b.dur) as durj
              from map_u_c a left join map_u_c b
              on a.uid = b.uid
              group by cidi,cidj;')



##############################
# Label Matrix
##############################
