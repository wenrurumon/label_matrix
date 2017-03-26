rm(list=ls())

library(stringr)
library(utils)
library(stringi)
library(rvest)

# 搜索电影

movie <- c('少女时代', '生活大爆炸', '喜羊羊与灰太狼')
x <- movie[2]
movie_url <- function(x){
  x <- URLencode(stri_enc_toutf8(x))
  url <- paste0('https://www.douban.com/search?cat=1002&q=',x,'&enc=utf-8')
  html <- read_html(url, encoding = "UTF-8")
  html %>% html_nodes(".result:nth-child(1) a") %>% html_attr("href") %>% .[1]
}


url <- movie_url('喜羊羊与灰太狼')

# 电影页面信息抓取

getdoubaninfo <- function(url){
  web <- read_html(url, encoding = "UTF-8")
  result <- web %>% html_nodes("#info, #info .pl") %>% html_text()
  aa <- paste0(result[2:length(result)], collapse = "|")
  aa <- gsub(":", "", aa)
  use <- str_split(result[1], aa)
  use <- use[[1]]
  use <- use[2:length(use)]
  names(use) <- unlist(strsplit(aa, "\\|"))
  list(
    label=use["类型"],
    actor=use["主演"],
    director=use["导演"],
    editor=use["编剧"],
    country=use["制片国家/地区"],
    language=use["语言"]
  )
}

getinfo <- function(key){
  url.douban <- movie_url(key)
  info.douban <- getdoubaninfo(url.douban)
  return(info.douban)
}
