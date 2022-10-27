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



###################################################################
# 예제4-2-1 : 댓글 감정 분석하기
###################################################################

# 이용 데이터 : news_comment_parasite.csv
raw_news_comment <- read_csv("news_comment_parasite.csv")


# 기본적인 전처리
# install.packages("textclean")
library(textclean)   # to use replace_html() : html 태그를 제거/변경 

news_comment <- raw_news_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))  #html 태그를 공백으로  
                                                   #raw_news_comment, news_comment 데이터에서 확인
# 데이터 구조 확인
glimpse(news_comment)


# 토큰화
word_comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)

word_comment %>%
  select(word, reply)

# 감정 점수 부여
word_comment <- word_comment %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

word_comment %>%
  select(word, polarity)


# 감정 분류하기
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity ==  2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))

word_comment %>%
  count(sentiment)


# 감정별로(긍정/부정, 중립 제외) 자주 사용된 단어 10개씩 추출
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)

top10_sentiment


# 막대 그래프 만들기
ggplot(top10_sentiment, aes(x = reorder(word, n), 
                            y = n, 
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +  
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))



###################################################################
# 예제4-2-2 : 댓글별 감정점수 구하기
###################################################################
# (id, reply)별별(문장별) 감정점수 합산
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

score_comment %>% 
  select(score, reply)


# 감정점수 높은 댓글
# 긍정 댓글
score_comment %>%
  select(score, reply) %>% 
  arrange(-score)   # 내림차순으로 정렬

# 부정 댓글
score_comment %>%
  select(score, reply) %>% 
  arrange(score)    # 오름차순으로 정렬


# 감정점수 빈도구하기
score_comment %>%
  count(score)


# 감정 분류
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >=  1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 감정별(긍정,부정,중립) 빈도수 및 비율
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
frequency_score


# 막대 그래프 만들기
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) + 
  scale_x_discrete(limits = c("pos", "neu", "neg"))


# 비율 누적 막대 그래프
# x축을 구성할 변수를 만들기 위해 더미 변수 생성
frequency_score$dummy <- 0
frequency_score


# -------------------------------------------------------------------------
ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")),      
            position = position_stack(vjust = 0.5)) + 
  theme(axis.title.x = element_blank(),  # x축 이름 삭제
        axis.text.x  = element_blank(),  # x축 값 삭제
        axis.ticks.x = element_blank())  # x축 눈금 삭제
