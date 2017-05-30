rm(list=ls())
library(data.table)
library(dplyr)

#Input data

setwd('/zfsauton/project/highmark/aska/wx/data')
#fd <- dir(pattern='2016-05')
fd <- dir(pattern='2016-05')
rawdata <- lapply(fd,function(x){
	print(x)
	setwd('/zfsauton/project/highmark/aska/wx/data')
	setwd(x)
	#fs <- dir()
	fs <- dir()
	rawi <- lapply(fs,function(fi){
		x <- fread(fi,integer64='numeric')
		x <- subset(x,V2>0)
		x$pid <- gsub('【|】(.*)|\\[|\\](.*)','',x$V4)
		map <- unique(x[,pid,V1])
		list(raw=x,map=map)
	})
})
rawdata <- do.call(c,rawdata)
map <- unique(do.call(rbind,lapply(rawdata,function(x){x[[2]]})))

setwd('/zfsauton/project/highmark/aska/wx/code')
save(map,file='map.rda')
