
rm(list=ls())

library(rvest)
library(rjson)
library(stringr)
library(dplyr)
library(utils)
library(stringi)
library(data.table)
library(jiebaR)

# 搜索电影

getdoubanurl <- function(key){
  url = paste0('https://www.douban.com/search?cat=1002&q=',URLencode(stri_enc_toutf8(key)))
  web <- readLines(url,encoding='UTF-8')
  url2 <-grep('www.douban.com/link2',web,value=T)[1]
  sid <- substr(url2,regexpr('sid: ',url2)+5,regexpr('}',url2)-1)
  paste0('https://movie.douban.com/subject/',sid)
}


# 电影页面信息抓取

getdoubaninfo <- function(url){
  web <- readLines(url,encoding='UTF-8')
  list(
    label=grep('类型:',web,value=T),
    actor=grep('主演</span>',web,value=T),
    director=grep('导演</span>',web,value=T),
    editor=grep('编剧</span>',web,value=T),
    country=grep('制片国家/地区:',web,value=T),
    language=grep('语言:',web,value=T)
  )
}


#combo

getinfo <- function(key){
  url.douban <- getdoubanurl(key)
  info.douban <- getdoubaninfo(url.douban)
  return(info.douban)
}
getinfo('少女时代')
getinfo('生活大爆炸')
getinfo('喜羊羊与灰太狼')
