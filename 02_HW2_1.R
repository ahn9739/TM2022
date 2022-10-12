####################################
library(rvest)      #웹 스크래핑(Web Scraping)을 위한 패키지
library(tidyverse)  #package for data science  ( https://www.tidyverse.org/ )
library(stringr)
library(data.table)
library(dplyr)
library(readr)
library(httr)
library(tm)

get_notice_title <- function(page) {
  code_url <- "https://www.jbnu.ac.kr/kor/?menuID=139"

  # 페이지 주소
  url <- paste0(code_url, "&pno=", page)

  # 웹 페이지 read 
  html <- read_html(url)#, encoding="euc-kr")

  post <- html %>%
    html_nodes("td") %>%
    #  html_nodes(".left") %>%
    html_nodes("a") %>%
    html_text() 

  post <- gsub("[\r\n\t]","", post) #r태그, t태그 제거

  post2 = c()
  for(i in 1:length(post)){
    if(nchar(post[i]) > 1) {
      post2 = append(post2, post[i])
    }
  }

  d = data.frame(post2)
  setnames(d, "post2", "post_title")
  
  return (d)
}

##############################################################
# page=1~10 
pages = 1:10
notice_title <- c()

for(page in pages) notice_title = rbind(notice_title, get_notice_title(page))


#write.csv(d, "jbnu_post.csv", row.names=FALSE)

