
setwd('/home/zhu/test/wuxitestdata')
library(data.table)
f <- paste0(0:23,'.txt')
raw <- lapply(f,fread)

#############

x <- raw[[1]]
v6 <- x$V6
x2 <- lapply(unique(x$V6),function(v){
  x[x$V6==v]
})
