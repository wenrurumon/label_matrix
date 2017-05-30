
rm(list=ls())

setwd('/zfsauton/project/highmark/aska/wx/data')
library(data.table)
library(dplyr)

fd <- dir(pattern='2016-05')
raw <- lapply(fd[1:5],function(fdi){
  print(fdi)
  setwd('/zfsauton/project/highmark/aska/wx/data')
  setwd(fdi)
  fs <- dir()[1:2]
  lapply(fs,fread)
})
raw <- do.call(c,raw)

raw2 <- lapply(raw,function(x){
  
})
