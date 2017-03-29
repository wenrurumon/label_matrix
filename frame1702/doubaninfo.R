rm(list=ls())

library(stringr)
library(utils)
library(stringi)
library(rvest)

# 搜索电影

movie <- c('少女时代', '生活大爆炸', '喜羊羊与灰太狼')
movie_url <- function(x,wait=5){
  cat(x)
  x <- URLencode(stri_enc_toutf8(x))
  Sys.sleep(wait)
  url <- paste0('https://www.douban.com/search?cat=1002&q=',x,'&enc=utf-8')
  html <- read_html(url, encoding = "UTF-8")
  html %>% html_nodes(".result:nth-child(1) a") %>% html_attr("href") %>% .[1]
}

# 电影页面信息抓取

getdoubaninfo <- function(url,wait=5){
  Sys.sleep(wait)
  web <- read_html(url,encoding='UTF-8')
  web.summary <- gsub(" |\n","",web %>% html_nodes("#info") %>% html_text())
  web.label <- gsub(" |\n","",web %>% html_nodes("#info .pl") %>% html_text())
  web.summary <- strsplit(web.summary,paste(web.label,collapse='|'))[[1]][-1]
  names(web.summary) <- gsub(":","",web.label)
  web.story <- gsub("　| |\n","",web %>% html_nodes("#link-report") %>% html_text())
  name <- gsub(' |\n','',web %>% html_nodes('#content h1')%>%html_text())
  print(name)
  c(url=url,name=name,web.summary,'剧情'=web.story)
}

getinfo <- function(key,wait=5){
  url.douban <- movie_url(key,wait)
  if(is.na(url.douban)){
    return(c(key,NA))
  } else {
    info.douban <- getdoubaninfo(url.douban,wait)
    return(c(key,info.douban))
  }
}
# getinfo('少女时代')
# getinfo('生活大爆炸')
# getinfo('喜羊羊与灰太狼')

#测试
load("C:/Users/WenluluSens/Documents/Project/SohuVedio/rdata_jinqi/gmax_lable.RData")
s <- paste0(names(gmax_lable),"<-gmax_lable$",names(gmax_lable))
for(si in s){
  eval(parse(text=si))
}
library(dplyr);library(data.table)

alsel <- filter(al,gid%in%gnamemap$gid)
alsel <- alsel[match(unique(alsel$gid),alsel$gid),]$cname

rlt <- list()

i <- 1
while(i<=length(alsel)){
  copy <- alsel[i]
  rlti <- try(getinfo(copy,wait=rnorm(1,5,1)))
  j <- 0
  while(class(rlti)=='try-error'){
    j <- j+0.5
    print(paste('mining pause at',Sys.time(),'and copy',i,copy))
    Sys.sleep(60*60*j)
    rlti <- try(getinfo(copy,wait=rnorm(1,5,1)))
  }
  i <- i+1
  rlt[[i]] <- rlti
}
