
rm(list=ls())
library(data.table)
library(dplyr)
library(igraph)

#setup macros

fc <- function(x){
  fc <- membership(fastgreedy.community(x))
  fc[] <- match(fc,unique(fc))
  fc
}
plotclust <- function(G){
  membership <- fc(G)
  plot(create.communities(G, membership), 
       # as.undirected(G), 
       as.directed(G),
       layout=layout.kamada.kawai(as.undirected(G)),
       edge.arrow.size=.1,
       vertex.size=3,
       vertex.label.cex=1,
       edge.width=.1)
  return(membership)
}
plotclust2 <- function(x){
  g <- graph_from_data_frame(filter(select(x,cidi,cidj),cidi>cidj),F)
  plotclust(g)
}

qpca <- function(A,rank=0,ifscale=TRUE){
  if(ifscale){A <- scale(as.matrix(A))[,]}
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
  return(rlt)
}

#Import data

setwd('C:\\Users\\WenluluSens\\Documents\\Project\\SohuVedio\\mgroup')
load("backup_film2.RData")
eval(parse(text=paste0(gsub('2','',names(backup_film2)),'<-backup_film2$',names(backup_film2))))

#calculation of the copy matrix

head(cmat)
cmaster <- as.data.table(
  cmat %>% 
    group_by(cidi) %>% 
    summarise(dur=max(duri),tdur=sum(durj)-max(duri),n=max(ni),nuid=max(nuid),ncid=n_distinct(cidj)-1)
)
ccalc <- filter(cmat,cidi!=cidj)
ccalc <- data.table(ccalc,
                    select(cmaster[match(ccalc$cidi,cmaster$cidi)],ttduri=tdur,tduri=dur,tni=n,nui=nuid,nci=ncid),
                    select(cmaster[match(ccalc$cidj,cmaster$cidi)],ttdurj=tdur,tdurj=dur,tnj=n,nuj=nuid,ncj=ncid))
ccalc <- mutate(ccalc,
                si=duri/((nuid/nui)*ttduri),sj=durj/((nuid/nuj)*ttdurj),
                ri=tduri/ttduri,rj=tdurj/ttdurj,
                vi=si/ri,vj=sj/rj
)
ccalc2 <- select(ccalc,cidi,cidj,si,sj,ri,rj,vi,vj)

#define the rule for copy connection
################################################

#input calc as x
x <- select(ccalc2,cidi,cidj,vi,vj)
#first step set v1,v2>2
thres1 <- 2
xi <- filter(x,(vi>thres1&vj>thres1))
xi.cluster <- plotclust2(xi)
#
thres2 <- 1
thres2.plus <- 0.8
xi <- lapply(unique(xi.cluster),function(g){
  sel <- filter(x,cidi%in%names(which(xi.cluster==g))&vi>thres2&vj>thres2)
  nsel <- nrow(sel)
  sel <- filter(x,cidi%in%c(sel$cidi,sel$cidj)&vi>thres2+thres2.plus&vj>thres2+thres2.plus)
  while(nrow(sel)>nsel){
    print(nsel)
    nsel <- nrow(sel)
    sel <- filter(x,cidi%in%c(sel$cidi,sel$cidj)&vi>thres2+thres2.plus&vj>thres2+thres2.plus)
  }
  unique(c(sel$cidi,sel$cidj))
})
g <- sapply(xi,function(x1){
  sapply(xi,function(x2){
    (1-length(unique(c(x1,x2)))/(length(x1)+length(x2)))*2
  })
})
plot(qpca(g)$prop)

