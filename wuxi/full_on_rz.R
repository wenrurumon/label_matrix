
rm(list=ls())
library(data.table)
library(dplyr)
library(sqldf)

setwd('E:\\无锡线上线下\\201605')
fd <- dir()

fdi <- fd[[1]]
raw <- lapply(fd,function(fdi){
  setwd('E:\\无锡线上线下\\201605')
  setwd(fdi)
  print(fdi)
  fs <- dir()
  raw <- lapply(fs,fread,encoding='UTF-8')
  return(raw)
})
