######################
#load data

rm(list=ls())
load("C:/Users/WenluluSens/Documents/Project/SohuVedio/rdata_jinqi/gmax_lable.RData")
s <- paste0(names(gmax_lable),"<-gmax_lable$",names(gmax_lable))
for(si in s){
  eval(parse(text=si))
}

######################
#gmax

gid <- match(unique(gnamemap$gid),gnamemap$gid)
gid <- gnamemap[gid,]
rownames(lmax) <- gid$gname

cluster <- function(lmax,thres=0.9){
  thres <- 0.9
  lmax2 <- sign(lmax) * (abs(lmax)>thres)
  cluster <- lapply(1:ncol(lmax),function(i){
    apply(lmax2,2,function(x){
      sum((lmax2[,i]*x)==-1)
    })
  })
  cluster <-  do.call(rbind,cluster)
  hc <- hclust(as.dist(cluster))
  plot(hc)
}


