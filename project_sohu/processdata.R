
setwd("C:/Users/WenluluSens/Documents/Project/SohuVedio/data/test/click5/")
alfolder <- 'C:/Users/WenluluSens/Documents/Project/SohuVedio/data/al/'

library(data.table)
library(dplyr)

raw <- do.call(rbind,lapply(dir()[1:1],read))
setwd(alfolder)
al <- do.call(rbind,lapply(dir(alfolder),function(x){fread(x,encoding='UTF-8')}))

colnames(raw) <- c('uid','time','cid','duration','click','adid','region','vehicle')
colnames(al) <- c('cid','cpnm','cptp','cprg')

euc <- raw %>% group_by(uid,cid) %>% summarise(dur=sum(as.numeric(duration)))
euc <- filter(euc,dur>0)
ec <- as.data.table(euc %>% group_by(cid) %>% summarise(nuid=n(),mdur=mean(dur),sdur=sd(dur)))
ec$sdur[is.na(ec$sdur)] <- 1
euc <- select(
        mutate(
          data.table(euc,ec[match(euc$cid,ec$cid)]),
          vote=dur/mdur/sdur),
        uid,cid,vote)
