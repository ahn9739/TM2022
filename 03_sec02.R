# 필요한 패키지 load
library(KoNLP)      # to use useSejongDic(), extractNoun(), useNIADic()  
library(wordcloud)  # to use wordcloud()
library(dplyr)      # to use %>%, select(), as_tibble(), mutate(), count(), filter(), arrange(), ...   
                    # package for Data Manipulation
library(stringr)    # to use str_replace_all(), str_squish()
                    # stringr 패키지에 있는 함수를 이용하여 사전처리
library(tidytext)   # to use unnest_tokens(), cast_dtm()   
                    # 텍스트 포맷의 데이터를 여러 토큰(token) 단위로 쪼개서 분석하는데 필요한 패키지
library(ggplot2)
library(ggwordcloud)   # to use geom_text_wordcloud()  # ggplot()에서 워드 클라우드 만들도록 도와주는 패키지  
library(readr)      # to use read_file(), read_csv()


useNIADic()  #한국정보화진흥원 사전


raw_moon = readLines("speech_moon.txt", encoding="UTF-8")
head(raw_moon)


###################################################################
# 예제3-2-1 : 형태소 분석 : 데이터 읽기, 사전처리, 토큰화(명사)
###################################################################

# 기본적인 전처리
moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ") %>%  # 한글만 남기기
  str_squish() %>%                      # 중복 공백 제거
  as_tibble()                           # tibble로 변환

# 명사 기준 토큰화
word_noun <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)


####################################################
# 예제3-2-2 : 명사 빈도 분석
####################################################

# 명사 빈도 & 두 글자 이상만 남기기
word_noun <- word_noun %>% 
  count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
  filter(str_count(word) > 1)  # 두 글자 이상만 남기기
word_noun

# 상위 20개 명사 추출
top20 <- word_noun %>%
  head(20)

# 워드 클라우드 
#library(ggwordcloud)
ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),
               range = c(3, 15)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()



####################################################
# 예제3-2-3 : 단어 빈도 분석
####################################################

raw_speeches <- read_csv("speeches_presidents.csv")
#raw_speeches <- read_csv("speeches_presidents2.csv", locale=locale('ko',encoding='euc-kr'))
raw_speeches

# 기본적인 전처리
speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

# 토큰화
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

# 단어 빈도 구하기
frequency <- speeches %>%
  count(president, word, sort = T) %>%
  filter(str_count(word) > 1)
frequency

# 단어 빈도 누적합 및 누적비율 구하기
frequency = frequency %>% group_by(president) %>%
  mutate(cn = cumsum(n),
         prop = cn/sum(n))


nrow(raw_speeches)
ncol(raw_speeches)

# 상위 발현 단어들이 차지하는 비율 그래프로 표현  
for (i in 1:nrow(raw_speeches)) {
  freq = frequency %>% filter(president==raw_speeches$president[i])

  plot((1:nrow(freq))/nrow(freq)*100, freq$prop, type='l', xlab='단어의 발현빈도', ylab='누적비율',
       main=raw_speeches$president[i], ylim=c(0, 1))
}

freq = frequency %>% filter(president==raw_speeches$president[4])
#데이터는 테이블에서 확인



##############################################################
###### 참고 : # 상위 발현 단어들이 차지하는 비율 그래프로 표현 (labels 추가)  

for (i in 1:nrow(raw_speeches)) {
  freq = frequency %>% filter(president==raw_speeches$president[i])
  
  #상위 발현 단어들이 차지하는 비율  
  plot(1:nrow(freq), freq$prop, type='l', xlab='단어의 발현빈도', ylab='누적비율',
       main=raw_speeches$president[i], axes=FALSE)
  axis(1, at=round(nrow(freq)/10*(0:10)), labels=paste(10*(0:10), "%", sep=""))
  axis(2, at=0.20*(0:5), labels=paste(20*(0:5), "%", sep=""))
  
  for (j in 1:9) {
    text(nrow(freq)/100*10*j, 0.05+freq$prop[nrow(freq)/100*10*j], 
         labels=paste(round(100*freq$prop[nrow(freq)/100*10*j]), "%", sep=""))
    points(nrow(freq)/100*10*j, freq$prop[nrow(freq)/100*10*j], pch=19)
  }
}


##############################################################

# 상위 발현 단어 워드 클라우드  
pal <- brewer.pal(4, "Dark2")

for (i in 1:nrow(raw_speeches)) {
  freq = frequency %>% filter(president==raw_speeches$president[i])
  
  wordcloud(freq$word, freq=freq$n, scale=c(4, 0.05), rot.per=0.0, min.freq=2, 
            random.order=FALSE, col=pal)
}




#######################################################################
### 예제3-2-4 : 문서들간의 상관계수
#######################################################################

raw_speeches <- read_csv("speeches_presidents.csv")
raw_speeches

# 기본적인 전처리
speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

# 토큰화
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

# 단어 빈도 구하기
frequency <- speeches %>%
  count(president, word, sort = T) 
#  %>% filter(str_count(word) > 1)
frequency

# DTM 만들기
dtm <- frequency %>%
  cast_dtm(document = president, term = word, value = n)
dtm

# 벡터화
as.matrix(dtm[1:4, 1:16])
dtm = as.matrix(dtm)

#문서와 문서의 상관계수 행렬 계산 
doc.corr <- matrix(NA, nrow=nrow(dtm), ncol=nrow(dtm))

for (i in 1:nrow(dtm)) {
  for (j in 1:nrow(dtm)) {
    doc.corr[i,j] <- cor(dtm[, i], dtm[, j])
  }
}

rownames(doc.corr) <- colnames(doc.corr) <- rownames(dtm)

#상관계수 행렬
round(doc.corr, 3)



#######################################################################
### 예제3-2-5 : 특정 단어가 사용된 문장
#######################################################################

raw_moon = readLines("speech_moon.txt", encoding="UTF-8")
head(raw_moon)

# 토큰화 : 전처리 작업 후 텍스트를 분석목적에 따라 토큰(단락, 문장, 단어 등)으로 나누는 작업 
sentences_moon <- raw_moon %>%
  str_squish() %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")
sentences_moon


# 특정 단어가 사용된 문장
sentences_moon %>%
  filter(str_detect(sentence, "국민"))

# 특정 단어가 사용된 문장
sentences_moon %>%
  filter(str_detect(sentence, "일자리"))



#######################################################################
### 예제3-2-6 : 특정 단어가 사용된 문장 수 비교
#######################################################################

raw_speeches <- read_csv("speeches_presidents.csv")
raw_speeches

sp_Pres = c()

for (i in 1:nrow(raw_speeches)) {
  temp = raw_speeches[i, ] %>%
    str_squish() %>%
    as_tibble() %>%
    unnest_tokens(input = value,
                  output = sentence,
                  token = "sentences")

  
  # 특정 단어가 사용된 문장
  sp_Pres[i] = temp %>%
    filter(str_detect(sentence, "국민"))
  
  print(length(sp_Pres[[i]]))
  
}



