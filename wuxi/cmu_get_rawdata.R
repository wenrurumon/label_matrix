rm(list=ls())
library(data.table)
library(dplyr)

#Input data

setwd('/zfsauton/project/highmark/aska/wx/data')
fd <- dir(pattern='2016-05')
rawdata <- lapply(fd,function(x){
	print(x)
	setwd('/zfsauton/project/highmark/aska/wx/data')
	setwd(x)
	fs <- dir()
  	rawi <- lapply(fs,function(fi){
		print(fi)
		x <- fread(fi,integer64='numeric')
		x$cid <- gsub('【|】(.*)|\\[|\\](.*)','',x$V4)
    		colnames(x) <- c('uid','fb','time','log','fbcode','client','cid')
    		return(x)
	})
	return(rawi)
})
rawdata <- do.call(c,rawdata)

#Save rawdata
setwd('/zfsauton/project/highmark/aska/wx/data/rda')
idx <- 1:length(rawdata)
idx <- as.numeric(cut(idx,70))
for(i in unique(idx)){
	rawi <- rawdata[idx==i]
	print(i)
	save(rawi,file=paste0('rawdata_',i,'.rda'))
}



