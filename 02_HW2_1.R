####################################
library(rvest)      #�� ��ũ����(Web Scraping)�� ���� ��Ű��
library(tidyverse)  #package for data science  ( https://www.tidyverse.org/ )
library(stringr)
library(data.table)
library(dplyr)
library(readr)
library(httr)
library(tm)

get_notice_title <- function(page) {
  code_url <- "https://www.jbnu.ac.kr/kor/?menuID=139"

  # ������ �ּ�
  url <- paste0(code_url, "&pno=", page)

  # �� ������ read 
  html <- read_html(url)#, encoding="euc-kr")

  post <- html %>%
    html_nodes("td") %>%
    #  html_nodes(".left") %>%
    html_nodes("a") %>%
    html_text() 

  post <- gsub("[\r\n\t]","", post) #r�±�, t�±� ����

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
