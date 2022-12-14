# chain/pipe operator : 어떤 작업에서 여러 단계의 절차를 필요로 할 때, 
# 중간 결과를 생성한 후 그 결과를 후속 절차에서 받아서 사용하는 경우에 유용
library(dplyr)    # to use %>%, select(), as_tibble(), mutate(), [3장 이후] count(), filter(), arrange(), ...   
                  # package for Data Manipulation


# read file and create data set
SiDo_Pop = read.csv("SiDo_Pop.csv", header=T, fileEncoding = "CP949", encoding = "UTF-8")
head(SiDo_Pop)

################################################################################
### %>% 사용 예 : 일반적인 데이터 

m = SiDo_Pop %>% summary()
m

# year, region, total 변수로 구성된 새로운 data set
new_data = SiDo_Pop[, c(1, 2, 5)]
new_data = subset(SiDo_Pop, select=c(year, region, total))
new_data = SiDo_Pop %>% select(year, region, total)

# 2010 이후 데이터, (year, region, total) 변수로 구성된 새로운 data set
new_data = subset(SiDo_Pop, year>=2010, select=c(year, region, total))
new_data = SiDo_Pop %>% filter(year>=2010) %>% select(year, region, total)

SiDo_Pop$male = as.numeric(SiDo_Pop$male)
SiDo_Pop %>% filter(year>=2010) %>% select(male) %>% colMeans

# 2000, 2010년의 area_tag=='M'인 데이터에서 year, region, area_tag, male, female 변수로 구성된 data set
new_data = subset(SiDo_Pop, year<=2010 & area_tag=='M', select=c(year, region, area_tag, male, female))
new_data = SiDo_Pop %>% filter(year<=2010 & area_tag=='M') %>% select(year, region, area_tag, male, female)



################################################################################
### %>% 사용 예 : 텍스트 데이터 사전처리 및 data set 구성

raw_moon = readLines("speech_moon.txt", encoding="UTF-8")
head(raw_moon)

# stringr 패키지에 있는 함수를 이용하여 사전처리
library(stringr)    # to use str_replace_all(), str_squish()

# 불필요한 문자(특수문자, 한자, 공백 등) 제거 : str_replace_all()
# 파라미터명 입력 : str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")
# 파라미터명 생략 : str_replace_all(txt, "[^가-힣]", " ")
moon <- raw_moon %>% str_replace_all("[^가-힣]", " ")
head(moon)

# 연속된 공백 하나만 남기고 제거 : str_squish()
moon <- moon %>% str_squish()
head(moon)

# 문자열 벡터 형태의 데이터를 tibble 구조로 바꾸기
# tibble은 simple data frame이라고 생각하면 됨. 데이터를 간략하게 표현할 수 있고,
# 이 구조를 이용하면 대용량 데이터 세트를 다루는데 용이함
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
library(rvest)      #웹 스크래핑(Web Scraping)을 위한 패키지
library(tidyverse)  #package for data science  ( https://www.tidyverse.org/ )
library(stringr)
library(data.table)
library(dplyr)
library(readr)
library(httr)
library(tm)

code_url <- "https://www.jbnu.ac.kr/kor/?menuID=139&pno=1"

# 페이지 주소
url <- paste0(code_url, "&pno=", 1)#page)
# 웹 페이지 read 
html <- read_html(url)#, encoding="euc-kr")
#html <- read_html(url, encoding="UTF-8")


post <- html %>%
  html_nodes("td") %>%
  #  html_nodes(".left") %>%
  html_nodes("a") %>%
  html_text() 
post  

post <- gsub("[\r\n\t]","", post) #r태그, t태그 제거

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