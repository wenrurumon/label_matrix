#####################################################
#Macro
#####################################################

#PCA
pca <- function(X){
  m = nrow(X)
  n = ncol(X)
  Xeigen <- svd(X)
  value <- (Xeigen$d)^2/m
  value <- cumsum(value/sum(value))
  score <- X %*% Xeigen$v
  mat <- Xeigen$v
  list(score=score,prop=value,mat=mat)
}
#quadratically regularized PCA
qpca <- function(A,lambda=0){
  A.svd <- svd(A)
  d <- A.svd$d-lambda*A.svd$d[1]
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
}
scale0 <- function(x,to01=T){
  x <- pnorm(scale(x))
  if(to01){
    x <- x-min(x,na.rm=TRUE)
    x <- x/max(x,na.rm=TRUE)
  }
  x[is.na(x)] <- 0.5
  x
}
mc0 <- function(X,lambda=0.2,ifprint=FALSE){
  ### input the initial values
  m <- dim(X)[1]
  n <- dim(X)[2]                                  
  Z <- matrix(rep(0,n*n),nrow=n)
  J <- matrix(rep(0,n*n),nrow=n)
  E <- matrix(rep(0,m*n),nrow=m)
  Y1 <- matrix(rep(0,m*n),nrow=m)
  Y2 <- matrix(rep(0,n*n),nrow=n)
  mu <- 10^(-6) 
  mu_max <- 10^6
  rho <- 1.1
  epsilon <- 10^(-8)
  A <- X
  I <- diag(n)
  ### compute the while loop to run matrix completion
  while ((max(abs(X-A %*% Z -E)) > epsilon) | (max(abs(Z-J)) > epsilon)) {
    if(ifprint){print(paste((max(abs(Z-J))),(max(abs(X-A %*% Z -E)))))}
    ## update the J matrix
    U_J_svd  <-  svd(Z+Y2/mu)
    big_eps <- diag(U_J_svd$d - 1/mu)
    big_eps[big_eps<0] <- 0
    J <- U_J_svd$u %*% big_eps %*% t(U_J_svd$v)
    ## update the Z matrix
    I_A_svd <- svd(I + t(A) %*% A )
    Z <- I_A_svd$v %*% diag(1/(I_A_svd$d)) %*% t(I_A_svd$u) %*% (t(A)%*%(X-E)+J
                                                                 +(t(A) %*% Y1 - Y2)/mu)
    ## update the E matrix
    Q <- X - A %*% Z + Y1/mu
    Q_col_norm <- sqrt(colSums(Q^2))
    Q_mlambda_o_mu <- Q_col_norm - lambda/mu
    Q_mlambda_o_mu[Q_mlambda_o_mu < 0] <- 0 
    Q_c_coef <- matrix(rep((Q_mlambda_o_mu/Q_col_norm),m),nrow=m,byrow=TRUE)
    E <- Q_c_coef * Q
    ## update the multipliers
    Y1 <- Y1 + mu*(X-A%*%Z-E)
    Y2 <- Y2 + mu*(Z-J)
    ## update the parameter mu
    mu <- min(rho*mu,mu_max)
  }
  #rlt <- A
  rlt <- list(Z = A%*%Z, X=A, Y=Z ,prop=cumsum(svd(X-E)$d/sum(svd(X-E)$d)))
  return(rlt)  
}
#Functional Kernal Representation
library(fda)
library(flare)
functional_transform <- function(X){
  #calculate the distance for each variables
  X.dist <- dist(t(X))
  pos <- hclust(X.dist)$order
  X <- X[,pos]
  X.dist <- as.matrix(X.dist)[pos,pos]
  pos <- c(0,cumsum(diag(X.dist[-1,-length(pos)])))
  pos <- scale0(pos,T,T)
  #set fourier basis
  fbasis<-create.fourier.basis(c(0,1),nbasis=length(pos)*2-1)
  fphi <- eval.basis(pos,fbasis)
  fcoef <- ginv(t(fphi)%*%fphi)%*%t(fphi)%*%t(X)
  rlt <- t(fcoef-rowMeans(fcoef))/sqrt(nrow(X))
  return(rlt)
}

checkrsq <- function(y,x){
  xlm <- summary(lm(y~x))
  sapply(xlm,function(x){x$r.square})
}

library(fda)
library(flare)
functional_transform <- function(X){
  #calculate the distance for each variables
  X.dist <- dist(t(X))
  pos <- hclust(X.dist)$order
  X <- X[,pos]
  X.dist <- as.matrix(X.dist)[pos,pos]
  pos <- c(0,cumsum(diag(X.dist[-1,-length(pos)])))
  pos <- scale0(pos,T)
  #set fourier basis
  fbasis<-create.fourier.basis(c(0,1),nbasis=length(pos)*2-1)
  fphi <- eval.basis(as.vector(pos),fbasis)
  fcoef <- ginv(t(fphi)%*%fphi)%*%t(fphi)%*%t(X)
  rlt <- t(fcoef-rowMeans(fcoef))/sqrt(nrow(X))
  return(rlt)
}

#####################################################
#Data Process
#####################################################

library(data.table)
library(dplyr)
library(bit64)
library(slam)

setwd('C:\\Users\\zhu2\\Documents\\sohu\\hj\\al')
al <- do.call(rbind,lapply(dir(),function(x){
  do.call(rbind,strsplit(readLines(x,encoding='UTF-8'),'\t'))
}))

#AL Recoding#我在这里做了处理，将同剧不同期不同季的信息在剧名里取出了。
al[,2] <- gsub('\\d','',al[,2])
al[grep('搜狐视频娱乐播报',al[,2]),2] <- '搜狐视频娱乐播报'
al[grep('第',al[,2]),2] <- 
  paste0(substr(grep('第',al[,2],value=T),1,regexpr('第',grep('第',al[,2],value=T))-1),
         substr(grep('第',al[,2],value=T),regexpr('第',grep('第',al[,2],value=T))+3,nchar(grep('第',al[,2],value=T))))
al <- cbind(al,match(al[,2],unique(al[,2])))

#Data Process Map

setwd('C:\\Users\\zhu2\\Documents\\sohu\\hj\\testdata\\')
load('raw_loaded_full.rda')
# raw <- lapply(dir(pattern='part'),function(rawi){
#   print(rawi)
#   rawi <- fread(rawi,sep='\t')
#   rawi <- select(rawi,uid=V1,cid=V3,time=V2,dur=as.numeric(V4))
#   rawi$cid <- al[match(paste(rawi$cid),al[,1]),5]
#   rawi$time <- unclass(as.POSIXct(rawi$time))
#   rawi <- arrange(rawi,uid,time)
#   rawi <- data.table(rawi,timediff=c(rawi$time[-1]-rawi$time[-nrow(rawi)],-1))
#   rawi <- select(filter(rawi,(timediff>=dur/3)|(timediff<0)),uid,cid,time,dur)
#   rawi %>% group_by(uid,cid) %>% summarise(n=n(),dur=sum(dur))
# })
# save(raw,file='raw_loaded_full.rda')
# Data Process Reduce
# 
euc <- do.call(rbind,raw) %>% group_by(uid,cid) %>% summarise(n=sum(n),dur=sum(dur))
ec <- euc %>% group_by(cid) %>% summarise(nuid=n_distinct(uid),mdur=mean(dur),dur=sum(dur),npuid=mean(n),n=sum(n))
eu <- euc %>% group_by(uid) %>% summarise(ncid=n_distinct(cid),mdur=mean(dur),dur=sum(dur),npcid=mean(n),n=sum(n))
eu <- filter(eu,ncid>=3)
ec <- filter(ec,nuid>=300&(!is.na(cid)))
euc <- filter(euc,(cid%in%ec$cid)&dur>0)
length(unique(euc$cid));length(unique(euc$uid))

#####################################################
#Solution with copy matrix
#####################################################

#dataframe for Copy Matrix
# 
library(sqldf)
system.time(
  eucmat_bk <- eucmat <- as.data.table(sqldf('select a.cid as cidi, b.cid as cidj,
                                sum(a.dur) as duri, sum(b.dur) as durj
                                from euc a left join euc b
                                on a.uid = b.uid
                                group by cidi, cidj
                                '))
  )
# save(raw,eucmat,ec,euc,file='raw_loaded_full.rda')
eucmst <- as.data.table(dplyr::select(filter(eucmat,cidi==cidj),cid=cidi,dur=duri))
eucmat2 <- data.table(eucmat,
                      tduri = eucmst[match(eucmat$cidi,eucmst$cid)]$dur,
                      tdurj = eucmst[match(eucmat$cidj,eucmst$cid)]$dur
)
eucmat2 <- mutate(eucmat2,pi=duri/tduri,pj=durj/tdurj,pbinom=pbinom(duri,tduri,pj))
eucmat2 <- mutate(filter(eucmat2,cidi!=cidj),p=pi)
# eucmat2 <- select(eucmat2,cidi,cidj,p=pj)

#Matrix for copy matrix

system.time(fmat2 <- simple_triplet_matrix(as.numeric(eucmat2$cidj),as.numeric(eucmat2$cidi),eucmat2$p))
dimnames(fmat2) <- list(1:ncol(fmat2),1:nrow(fmat2))
fmat2 <- fmat2[col_sums(fmat2)>0,col_sums(fmat2)>0]
fmat2 <- fmat2[rowapply_simple_triplet_matrix(fmat2,var)>0,colapply_simple_triplet_matrix(fmat2,var)>0]
fmat <- t(fmat2)

#Functional Transformation
# fmat <- scale(as.matrix(fmat))[,]
# system.time(fmat_ft <- functional_transform(fmat))
# fmat <- fmat_ft

###############################################################
#Solution with Matrix Compeletion
###############################################################

system.time(fmat.qpca <- qpca(scale(as.matrix(fmat)),0.1));print(fmat.qpca$info);print(fmat.qpca$rank)
score <- fmat.qpca$X[,1:which(fmat.qpca$prop>0.95)[1],drop=F];dim(score)
score.rsq <- checkrsq(as.matrix(fmat),score);hist(score.rsq)
names(score.rsq) <- al[match(as.numeric(rownames(fmat)),al[,5]),2]
mcscore <- mc0(score,lambda=0.2,TRUE)
score2 <- mcscore$X[,1:which(mcscore$prop>0.95)[1],drop=F];dim(score2)
score2.rsq <- checkrsq(as.matrix(fmat),score2);hist(score2.rsq)
names(score2.rsq) <- al[match(as.numeric(rownames(fmat)),al[,5]),2]

sum(ec$nuid[match(colnames(fmat),ec$cid)]*score.rsq)/sum(ec$nuid[match(colnames(fmat),ec$cid)])
sum(ec$nuid[match(colnames(fmat),ec$cid)]*score2.rsq)/sum(ec$nuid[match(colnames(fmat),ec$cid)])

#Label Matrix
label <- (score2-mean(score2))/sd(score2)
label <- (pnorm(label)-0.5)*2
rownames(label) <-  al[match(as.numeric(rownames(fmat)),al[,5]),2]
colnames(label) <- paste0('label',1:ncol(label))

#Figure out label series and significant copy in this label
splitgroup <- function(x,q=0.1,l=0){
  xpos <- sum(x>=(1-q));xneg <- sum(x<=(q-1))
  xpos <- head(cbind(sort(x,decreasing=TRUE)),max(xpos,l))
  xneg <- head(cbind(sort(x)),max(xneg,l))
  list(positive=xpos,negative=xneg)
}
splitgroup(label[,31],q=0.025,l=10)

for(i in 1:5){
  print(paste('label',i))
  print(splitgroup(label[,i],q=0.025,l=10))
}

#################################################################
#DMP
#################################################################

lab2 <- label
rownames(lab2) <- as.numeric(rownames(fmat))
ec2 <- euc %>% group_by(cid) %>% summarise(nuid=n_distinct(uid),mn=mean(n),q1dur=quantile(dur,.25),q3dur=quantile(dur,.5))
ec2 <- mutate(ec2,sdur=(q3dur-q1dur)/qnorm(0.99))
euc2 <- mutate( data.table(euc,select(ec2,-cid)[match(euc$cid,ec2$cid)]),score1=pnorm(dur,q1dur,sdur))
eu2 <- euc2 %>% group_by(uid) %>% summarise(uscore=sum(score1))
euc2 <- select(
  mutate(data.table(euc2,select(eu2,-uid)[match(euc2$uid,eu2$uid)]),score2=score1/uscore),
  uid,cid,n,dur,score1,score2)

dmp <- function(euc.input=NULL,label.input=lab2){
  if(is.null(euc.input)){
    euc.input <- select(filter(euc2,uid==sample(euc2$uid,1)),uid,cid,score=score2)
  }
  label.output <- cbind(colSums(euc.input$score * lab2[match(euc.input$cid,rownames(label.input)),]))
  euc.input <- cbind(copy=al[match(euc.input$cid,al[,5]),2],score=(euc.input$score))
  list(euc.input=euc.input,label.output=label.output)
}
dmp()
