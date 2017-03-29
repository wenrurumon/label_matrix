
######################
#load data

rm(list=ls())
load("C:/Users/WenluluSens/Documents/Project/SohuVedio/rdata_jinqi/gmax_lable.RData")
s <- paste0(names(gmax_lable),"<-gmax_lable$",names(gmax_lable))
for(si in s){
  eval(parse(text=si))
}

######################
#label clustering

gid <- match(unique(gnamemap$gid),gnamemap$gid)
gid <- gnamemap[gid,]
rownames(lmax) <- gid$gname

labels <- cbind(grepl('韩',gid$cregion)
                ,gid$cclass=='综艺'
                ,grepl('爱',rownames(lmax))
                ,grepl('世界杯|足球|篮球|运动',gid$cclass))+0

######################
#label clustering

lmax.model <- as.data.frame(lmax[,,drop=F])

label.kr <- grepl('韩',gid$cregion); mean(label.kr)
label.kr <- gid$cclass=='综艺'; mean(label.kr)
label.kr <- grepl('爱',rownames(lmax)); mean(label.kr)
label.kr <- grepl('世界杯|足球|篮球|运动',gid$cclass); mean(label.kr)

test.id <- sample(1:3530,1000)
train.id <- -test.id
train.lda <-lda(label.kr[train.id]~.,data=lmax.model[train.id,])
rlt.train <- table(predict(train.lda)$class,label.kr[train.id])
rlt.test <- table(predict(train.lda,lmax.model[test.id,])$class,label.kr[test.id])
rlt.train; rlt.test
sum(diag(rlt.train))/sum(rlt.train); sum(diag(rlt.test))/sum(rlt.test)

#####################
#macro

#given lmax_existed, label_existed and lmax2test estimating label_2test

colMeans(labels)
test.id <- sample(1:3530,2000)
train.id <- -test.id
lmaxe <- as.data.frame(lmax)[train.id,]
labe <- labels[train.id,]
lmaxt <- as.data.frame(lmax)[test.id,]

lab2lab <- function(lmaxt,lmaxe,labe){
  ldae <- apply(labe,2,function(y){
    list(model=MASS::lda(y~.,data=lmaxe),y=y)
  })
  print(lapply(ldae,function(x){table(predict(x$model)$class,x$y)}))
  do.call(cbind,lapply(ldae,function(x){
    as.numeric(paste(predict(x$model,lmaxt)$class))
  }))
}

labt <- lab2lab(lmaxt,lmaxe,labe)
for(i in 1:ncol(labels)){
  print(table(labt[,i],labels[test.id,i]))
}

#########################
#Given several copy then have them clustered

check_copy_clustering <- function(copy.given,thres,lmax){
  lmax.given <- lmax[rownames(lmax)%in%copy.given,]
  lhc.given <- hclust(dist(lmax.given))
  plot(lhc.given)
  label.given <- cutree(lhc.given,2)
  apply(predict(lda(label.given~lmax.given))[[2]],2,function(x){names(which(x>=thres))})
}

test <- check_copy_clustering(rownames(lmax)[abs(lmax[,2])>=0.96],.99,lmax)
test <- check_copy_clustering(rownames(lmax)[labels[,2]==1],.95,lmax)
test <- check_copy_clustering(sample(filter(gid,cclass=="动漫")$gname,100),.99,lmax)


par(mfrow=c(2,1))
hist(lmax[match(test[[1]],rownames(lmax)),1])
hist(lmax[match(test[[2]],rownames(lmax)),1])
par(mfrow=c(1,1))

print(test <- check_copy_clustering(rownames(lmax),.999,lmax))
print(test <- check_copy_clustering(test[[1]],.99,lmax))
print(test <- check_copy_clustering(test[[1]],.99,lmax))


