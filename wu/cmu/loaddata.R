
rm(list=ls())

setwd('/zfsauton/project/highmark/aska/wx/data')
library(data.table)
library(dplyr)

fd <- dir(pattern='2016-05')
raw <- lapply(fd,function(fdi){
  print(fdi)
  setwd('/zfsauton/project/highmark/aska/wx/data')
  setwd(fdi)
  fs <- dir()
  lapply(fs,fread)
})
raw <- do.call(c,raw)
