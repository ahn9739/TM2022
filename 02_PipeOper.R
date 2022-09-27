# chain/pipe operator : ¾î¶² ÀÛ¾÷¿¡¼­ ¿©·¯ ´Ü°èÀÇ ÀıÂ÷¸¦ ÇÊ¿ä·Î ÇÒ ¶§, 
# Áß°£ °á°ú¸¦ »ı¼ºÇÑ ÈÄ ±× °á°ú¸¦ ÈÄ¼Ó ÀıÂ÷¿¡¼­ ¹Ş¾Æ¼­ »ç¿ëÇÏ´Â °æ¿ì¿¡ À¯¿ë
library(dplyr)    # to use %>%, select(), as_tibble(), mutate(), [3Àå ÀÌÈÄ] count(), filter(), arrange(), ...   
                  # package for Data Manipulation


# read file and create data set
SiDo_Pop = read.csv("SiDo_Pop.csv", header=T, fileEncoding = "CP949", encoding = "UTF-8")
head(SiDo_Pop)

################################################################################
### %>% »ç¿ë ¿¹ : ÀÏ¹İÀûÀÎ µ¥ÀÌÅÍ 

m = SiDo_Pop %>% summary()
m

# year, region, total º¯¼ö·Î ±¸¼ºµÈ »õ·Î¿î data set
new_data = SiDo_Pop[, c(1, 2, 5)]
new_data = subset(SiDo_Pop, select=c(year, region, total))
new_data = SiDo_Pop %>% select(year, region, total)

# 2010 ÀÌÈÄ µ¥ÀÌÅÍ, (year, region, total) º¯¼ö·Î ±¸¼ºµÈ »õ·Î¿î data set
new_data = subset(SiDo_Pop, year>=2010, select=c(year, region, total))
new_data = SiDo_Pop %>% filter(year>=2010) %>% select(year, region, total)

SiDo_Pop$male = as.numeric(SiDo_Pop$male)
SiDo_Pop %>% filter(year>=2010) %>% select(male) %>% colMeans

# 2000, 2010³âÀÇ area_tag=='M'ÀÎ µ¥ÀÌÅÍ¿¡¼­ year, region, area_tag, male, female º¯¼ö·Î ±¸¼ºµÈ data set
new_data = subset(SiDo_Pop, year<=2010 & area_tag=='M', select=c(year, region, area_tag, male, female))
new_data = SiDo_Pop %>% filter(year<=2010 & area_tag=='M') %>% select(year, region, area_tag, male, female)



################################################################################
### %>% »ç¿ë ¿¹ : ÅØ½ºÆ® µ¥ÀÌÅÍ »çÀüÃ³¸® ¹× data set ±¸¼º

raw_moon = readLines("speech_moon.txt", encoding="UTF-8")
head(raw_moon)

# stringr ÆĞÅ°Áö¿¡ ÀÖ´Â ÇÔ¼ö¸¦ ÀÌ¿ëÇÏ¿© »çÀüÃ³¸®
library(stringr)    # to use str_replace_all(), str_squish()

# ºÒÇÊ¿äÇÑ ¹®ÀÚ(Æ¯¼ö¹®ÀÚ, ÇÑÀÚ, °ø¹é µî) Á¦°Å : str_replace_all()
# ÆÄ¶ó¹ÌÅÍ¸í ÀÔ·Â : str_replace_all(string = txt, pattern = "[^°¡-ÆR]", replacement = " ")
# ÆÄ¶ó¹ÌÅÍ¸í »ı·« : str_replace_all(txt, "[^°¡-ÆR]", " ")
moon <- raw_moon %>% str_replace_all("[^°¡-ÆR]", " ")
head(moon)

# ¿¬¼ÓµÈ °ø¹é ÇÏ³ª¸¸ ³²±â°í Á¦°Å : str_squish()
moon <- moon %>% str_squish()
head(moon)

# ¹®ÀÚ¿­ º¤ÅÍ ÇüÅÂÀÇ µ¥ÀÌÅÍ¸¦ tibble ±¸Á¶·Î ¹Ù²Ù±â
# tibbleÀº simple data frameÀÌ¶ó°í »ı°¢ÇÏ¸é µÊ. µ¥ÀÌÅÍ¸¦ °£·«ÇÏ°Ô Ç¥ÇöÇÒ ¼ö ÀÖ°í,
# ÀÌ ±¸Á¶¸¦ ÀÌ¿ëÇÏ¸é ´ë¿ë·® µ¥ÀÌÅÍ ¼¼Æ®¸¦ ´Ù·ç´Âµ¥ ¿ëÀÌÇÔ
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
library(rvest)      #À¥ ½ºÅ©·¡ÇÎ(Web Scraping)À» À§ÇÑ ÆĞÅ°Áö
library(tidyverse)  #package for data science  ( https://www.tidyverse.org/ )
library(stringr)
library(data.table)
library(dplyr)
library(readr)
library(httr)
library(tm)

code_url <- "https://www.jbnu.ac.kr/kor/?menuID=139&pno=1"

# ÆäÀÌÁö ÁÖ¼Ò
url <- paste0(code_url, "&pno=", 1)#page)
# À¥ ÆäÀÌÁö read 
html <- read_html(url)#, encoding="euc-kr")
#html <- read_html(url, encoding="UTF-8")


post <- html %>%
  html_nodes("td") %>%
  #  html_nodes(".left") %>%
  html_nodes("a") %>%
  html_text() 
post  

post <- gsub("[\r\n\t]","", post) #rÅÂ±×, tÅÂ±× Á¦°Å

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