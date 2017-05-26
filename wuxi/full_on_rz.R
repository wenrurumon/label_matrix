
rm(list=ls())
library(data.table)
library(dplyr)
library(sqldf)

setwd('E:\\无锡线上线下\\201605')
fd <- dir()

#load data
fdi <- fd[[1]]
raw <- lapply(fd,function(fdi){
  setwd('E:\\无锡线上线下\\201605')
  setwd(fdi)
  print(fdi)
  # fs <- dir()[1:2]
  fs <- dir()
  raw <- lapply(fs,fread,encoding='UTF-8')
  return(raw)
})
raw <- do.call(c,raw)

#filter map
map <- unique(do.call(rbind,lapply(raw,function(x){
  uid <- as.numeric(x$V1)
  pid <- x$V4
  pid <- gsub('【|】(.*)|\\[|\\](.*)','',pid)
  unique(data.table(uid=uid,pid=pid))
})))
uidt <- table(map$uid)
uidt <- uidt[which(uidt>1)]

#filter raw
raw2 <- lapply(raw,function(x){
  v1 <- paste(x$V1)
  s <- v1%in%names(uidt)
  subset(x,s)
})
