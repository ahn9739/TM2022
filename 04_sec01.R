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


# 감정(성)사전 불러오기
# 감정(성)사전은 단어와 감정(성)의 강도를 숫자로 표현. 군산대 'KNU 한국어 감성사전' 이용
dic <- read_csv("knu_sentiment_lexicon.csv")

# 긍정 단어 보기
dic %>% 
  filter(polarity == 2) %>% 
  arrange(word)

# 부정 단어 보기
dic %>% 
  filter(polarity == -2) %>% 
  arrange(word)


# 감정 단어의 종류 살펴보기
dic %>% 
  filter(word %in% c("좋은", "나쁜"))

dic %>% 
  filter(word %in% c("기쁜", "슬픈"))

dic %>%
  filter(word %in% c("행복하다", "좌절하다"))


# 이모티콘의 감정 강도 살펴보기
dic %>% 
  filter(!str_detect(word, "[가-힣]")) %>% 
  arrange(word)


# 감정 사전의 단어 구성
dic %>% 
  mutate(sentiment = ifelse(polarity >=  1, "pos",
                            ifelse(polarity <= -1, "neg", "neu"))) %>% 
  count(sentiment)




###################################################################
# 예제4-1-1 : 문장의 감정점수 구하기
###################################################################
df <- tibble(sentence = c("디자인 예쁘고 마감도 좋아서 만족스럽다.",
                          "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다."))
df

df <- df %>% 
  unnest_tokens(input = sentence,
                output = word,
                token = "words",
                drop = F)
df

# 단어에 감정점수 부여하기
df <- df %>% 
  left_join(dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
df

# 문장별로 감정점수 합산하기
score_df <- df %>% 
  group_by(sentence) %>% 
  summarise(score  = sum(polarity))
score_df



