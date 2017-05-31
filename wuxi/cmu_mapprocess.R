
rm(list=ls())
setwd('/zfsauton/project/highmark/aska/wx/data/rda')
library(data.table)
library(dplyr)

fs <- dir()
ld <- function(fi){
  load(fi)
  return(rawi)
}
