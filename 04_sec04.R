
###################################################################
# 예제4-4-1 : 영문 텍스트 
###################################################################

#감정어휘 사전을 활용한 텍스트 감정분석을 위한 패키지 
#install.packages("backports")
#install.packages("broom")    # Convert Statistical Objects into Tidy Tibbles
#install.packages("tidytext")
#install.packages("tidyr")
#install.packages("textdata")

library(tidytext)
library(tidyr)
library(textdata)
library(ggplot2)

#AFINN 감정어휘 사전 호출
get_sentiments("afinn")

#감정어휘 사전은 별도로 저장하여 사용도 가능함
AFINN <- data.frame(get_sentiments("afinn"))
summary(AFINN)
hist(AFINN$value,breaks=20,xlim=c(-6,6),col='blue',
     xlab='sentiment score in AFINN',
     main='')

#Bing Liu's opinion lexicon 사전 저장 후 기초통계 분석
oplex <- data.frame(get_sentiments("bing"))
table(oplex$sentiment)

#EmoLex 사전 저장 후 기초통계 분석
emolex <- data.frame(get_sentiments("nrc"))
table(emolex$sentiment)

emolex$word[emolex$sentiment=='anger']  #anger에 해당하는 단어들


#-------------------------------------------------------------------------------
# 텍스트 데이터 호출
library('tm')
library('stringr')
library('dplyr')

getwd()
text.location <- paste0(getwd(), "/epapers") # data read

#말뭉치 만들기 
papers  <- VCorpus(DirSource(text.location), readerControl = list(language="en"))

txts <- c(rep(NA),24)
for (i in 1:24) {
  txts[i] <- as.character(papers[[i]][1])
}


#tidytext 형태의 데이터 구성 
df.text <- data_frame(papers.id=1:24, doc=txts)

# 24x2 데이터 행렬
df.text



#이제 문서-단어로 구성된 행렬 생성 
#unnest_tokens() 함수 : text 변수를 word로 구분

df.text.word <- df.text %>% 
  unnest_tokens(word, doc)
df.text.word



#inner_join() 함수를 감정사전과 결합 
result.sa <- df.text.word %>% inner_join(get_sentiments("bing"))
result.sa 
result.sa <- result.sa %>% count(word, papers.id, sentiment) #단어별 papers.id별 sentiment별 카운트 
result.sa
result.sa <- result.sa %>% spread(sentiment, n, fill=0) #Spread a key-value pair across multiple columns
result.sa                #fill=0 : 결측치는 0 으로 대체



#문서별 긍정단어와 부정단어 갯수 및 차이
agg <- summarise(group_by(result.sa, papers.id),
                   pos.sum = sum(positive),
                   neg.sum = sum(negative),
                   pos.sent = pos.sum-neg.sum)
agg



#문서의 메타데이터로 데이터 셑 구성
paper.name <- list.files(path=text.location, pattern=NULL, all.files=FALSE)

pub.year <- as.numeric(unlist(
  str_extract_all(paper.name, "[[:digit:]]{4}")))  #paper.name에서 숫자 4개 추출
papers.id <- 1:24
pub.year.df <- data.frame(papers.id, paper.name, pub.year)


# 출간년도별 감정어휘의 변화
agg <- merge(agg, pub.year.df, by='papers.id', all=TRUE)


# 출간년도별 감정어휘의 변화 그래프
agg.long <- reshape(agg,idvar='papers.id',
                      varying=list(2:4),timevar = "category",
                      v.names='value',direction="long")
agg.long$cate[agg.long$category==1] <- 'Positive words'
agg.long$cate[agg.long$category==2] <- 'Negative words'
agg.long$cate[agg.long$category==3] <- 'Positivity score'


ggplot(data=agg.long, aes(x=pub.year,y=value)) + 
  geom_bar(stat="identity")+
  labs(x='Publication year',y='value')+
  scale_x_continuous(limits=c(2008,2016))+
  facet_grid(cate~.)



# 출간년도별 긍정, 부정, 긍정과 부정의 차이의 평균값 그래프
agg.long.year <- aggregate(value~pub.year+cate, agg.long, mean) #app.long 데이터의 pub.year+cate별 value의 평균 집계
agg.long.year

ggplot(data=agg.long.year,aes(x=pub.year,y=value)) + 
  geom_line()+
  labs(x='Publication year',y='value')+
  scale_x_continuous(limits=c(2009,2015))+
  facet_grid(cate~.)

