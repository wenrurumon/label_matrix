
setwd('/mnt/gulab/hzx/data')
#split -l 10000 matbk.txt -d -a 4 bksplit_ &
files <- dir()[grepl('split2',dir())]

result <- list()
kcore <- 30

print(paste('start',Sys.time()))
system.time(for(i in files){
  print(i)
  system.time(mat <- read.csv(i,header = F,row.names =1))
    print(paste('import data finished',Sys.time()))
  system.time(km <- kmeans(mat,kcore))
    print(paste('kmeans finished',Sys.time()))
  rm(mat)
  # kmct <- km$center
  # kmcl <- km$cluster
  result <- c(result,list(km))
    print(paste('result generated',Sys.time()))
})

setwd('/mnt/gulab/hzx/output')
  print(paste('Start Parsing',Sys.time()))
save.image("result1.RData")

###Parse

center <- lapply(result,function(x){x$center})
cluster <- lapply(result,function(x){tapply(x$cluster,x$cluster,length)})

save.image("result2.RData")

###Merge

center <- rbind(center[[1]],center[[2]],center[[3]])
cluster <- unlist(cluster)
