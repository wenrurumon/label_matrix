########################################
# Dummy data
########################################

nuid <- 100
ncid <- 10
nplid <- 2

udata <- matrix(0,ncol=nplid,nrow=nuid)
udata[sample(1:length(udata),length(udata)/2)] <- 1
udata <- cbind(udata,1-udata)
dimnames(udata) <- 
  list(paste0('u',1:nuid),
       paste0(rep(c('pl','nl'),each=nplid),c(1:nplid,1:nplid)))

cdata <- matrix(0,nrow=ncid,ncol=nplid)
cdata[sample(1:length(cdata),length(cdata)/2)] <- 1
cdata <- cbind(cdata,1-cdata)
dimnames(cdata) <- list(paste0('c',1:ncid),colnames(udata))

ucdata <- udata %*% t(cdata)
ucdata[-sample(1:length(ucdata),nrow(udata)*5,prob=as.vector(ucdata))] <- 0
ucdata[ucdata>0] <- 1

umix <- udata %*% t(udata)
umix <- umix / diag(umix)

cmix <- t(ucdata) %*% ucdata
cmix <- cmix / diag(cmix)
# diag(cmix) <- 0

lmix <- cdata %*% t(cdata)
lmix <- lmix / diag(lmix)
# diag(lmix) <- 0

############################################
# Test
############################################

mean(diag(cor(qpca(ucdata,0)$Z,(udata%*%t(cdata))>0)))
mean(diag(cor(qpca(ucdata,0.2,F)$Z,(udata%*%t(cdata))>0)))
mean(diag(cor(qpca(ucdata,0.2,F)$Z,(udata%*%t(cdata)))))

hc <- hclust(dist(cmix));heatmap(cmix)
cbind(cutree(hc,qpca(cmix,0.1,F)$rank),cdata)
