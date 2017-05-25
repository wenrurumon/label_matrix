
rm(list=ls())
setwd('/home/zhu/test/wuxitestdata')
library(data.table)

#Load all the data
f <- paste0(0:23,'.txt')
raw <- lapply(f,function(x){
  x <- fread(x)
  x$V7 <- gsub('【|】(.*)|\\[|\\](.*)','',x$V4)
  x
})
raw <- do.call(rbind,raw)


#Build the map for uid and pid
map <- unique(raw[,V7,V1])
uid <- map$V1
pid <- map$V7

#Select those with multi action information
uidt <- table(uid)
pidt <- table(pid)/length(uidt)

#filtered data

system.time(
  raw2 <- subset(raw,
               V7%in%names(which(pidt>0.0001)) & 
               V1%in%names(which(uidt>1)) &
               V2 != -1
              )[order(V1,V7)]
 )

map2 <- unique(raw2[,V1,V7]); colnames(map2) <- c('pid','uid')

#Copy Matrix
system.time(
  cmat <- as.data.table(sqldf(
    'select a.pid as pidi, b.pid as pidj, count(1) as n
    from map2 a left join map2 b
    on a.uid = b.uid
    group by pidi, pidj'
  ))
)
cmast <- subset(cmat,pidi==pidj)[,pidi,n]
cmat <- subset(cmat,pidi!=pidj)
cmat2 <- subset(
  data.table(cmat,
             ni=cmast[match(cmat$pidi,cmast$pidi)]$n,
             nj=cmast[match(cmat$pidj,cmast$pidi)]$n),
  ni>=nj
)[order(-n)]



