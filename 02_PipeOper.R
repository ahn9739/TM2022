# chain/pipe operator : � �۾����� ���� �ܰ��� ������ �ʿ�� �� ��, 
# �߰� ����� ������ �� �� ����� �ļ� �������� �޾Ƽ� ����ϴ� ��쿡 ����
library(dplyr)    # to use %>%, select(), as_tibble(), mutate(), [3�� ����] count(), filter(), arrange(), ...   
                  # package for Data Manipulation


# read file and create data set
SiDo_Pop = read.csv("SiDo_Pop.csv", header=T, fileEncoding = "CP949", encoding = "UTF-8")
head(SiDo_Pop)

################################################################################
### %>% ��� �� : �Ϲ����� ������ 

m = SiDo_Pop %>% summary()
m

# year, region, total ������ ������ ���ο� data set
new_data = SiDo_Pop[, c(1, 2, 5)]
new_data = subset(SiDo_Pop, select=c(year, region, total))
new_data = SiDo_Pop %>% select(year, region, total)

# 2010 ���� ������, (year, region, total) ������ ������ ���ο� data set
new_data = subset(SiDo_Pop, year>=2010, select=c(year, region, total))
new_data = SiDo_Pop %>% filter(year>=2010) %>% select(year, region, total)

SiDo_Pop$male = as.numeric(SiDo_Pop$male)
SiDo_Pop %>% filter(year>=2010) %>% select(male) %>% colMeans

# 2000, 2010���� area_tag=='M'�� �����Ϳ��� year, region, area_tag, male, female ������ ������ data set
new_data = subset(SiDo_Pop, year<=2010 & area_tag=='M', select=c(year, region, area_tag, male, female))
new_data = SiDo_Pop %>% filter(year<=2010 & area_tag=='M') %>% select(year, region, area_tag, male, female)



################################################################################
### %>% ��� �� : �ؽ�Ʈ ������ ����ó�� �� data set ����

raw_moon = readLines("speech_moon.txt", encoding="UTF-8")
head(raw_moon)

# stringr ��Ű���� �ִ� �Լ��� �̿��Ͽ� ����ó��
library(stringr)    # to use str_replace_all(), str_squish()

# ���ʿ��� ����(Ư������, ����, ���� ��) ���� : str_replace_all()
# �Ķ���͸� �Է� : str_replace_all(string = txt, pattern = "[^��-�R]", replacement = " ")
# �Ķ���͸� ���� : str_replace_all(txt, "[^��-�R]", " ")
moon <- raw_moon %>% str_replace_all("[^��-�R]", " ")
head(moon)

# ���ӵ� ���� �ϳ��� ����� ���� : str_squish()
moon <- moon %>% str_squish()
head(moon)

# ���ڿ� ���� ������ �����͸� tibble ������ �ٲٱ�
# tibble�� simple data frame�̶�� �����ϸ� ��. �����͸� �����ϰ� ǥ���� �� �ְ�,
# �� ������ �̿��ϸ� ��뷮 ������ ��Ʈ�� �ٷ�µ� ������
moon <- as_tibble(moon)
moon



################################################################################
################################################################################
################################################################################
### Web Scraping, Crawling


install.packages("N2H4")

library(N2H4)
library(dplyr)
library(data.table)

n_url = "https://n.news.naver.com/mnews/article/001/0013299250?sid=100"
comm <- getAllComment(n_url)
d = data.frame(comm$contents)

d = d %>% mutate(d, id = row_number())
d <- d[, c("id", "comm.contents")]
setnames(d, "comm.contents", "comment")

write.csv(d, "naver_com.csv", row.names=FALSE)


####################################
library(rvest)      #�� ��ũ����(Web Scraping)�� ���� ��Ű��
library(tidyverse)  #package for data science  ( https://www.tidyverse.org/ )
library(stringr)
library(data.table)
library(dplyr)
library(readr)
library(httr)
library(tm)

code_url <- "https://www.jbnu.ac.kr/kor/?menuID=139&pno=1"

# ������ �ּ�
url <- paste0(code_url, "&pno=", 1)#page)
# �� ������ read 
html <- read_html(url)#, encoding="euc-kr")
#html <- read_html(url, encoding="UTF-8")


post <- html %>%
  html_nodes("td") %>%
  #  html_nodes(".left") %>%
  html_nodes("a") %>%
  html_text() 
post  

post <- gsub("[\r\n\t]","", post) #r�±�, t�±� ����

post2 = c()
for(i in 1:length(post)){
  if(nchar(post[i]) > 1) {
    post2 = append(post2, post[i])
  }
}
post2


d = data.frame(post2)

d = d %>% mutate(d, id = row_number())
d <- d[, c("id", "post2")]
setnames(d, "post2", "post_title")

write.csv(d, "jbnu_post.csv", row.names=FALSE)