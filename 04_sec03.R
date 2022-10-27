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
# 예제4-3-1 : 감정 변수별 단어빈도
###################################################################

raw_news_comment <- read_csv("news_comment_parasite.csv")


# 기본적인 전처리
# install.packages("textclean")
library(textclean)   # to use replace_html() : html 태그를 제거/변경 

news_comment <- raw_news_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))  #html 태그를 공백으로  
                                                   #raw_news_comment, news_comment 데이터에서 확인

# 토큰화
word_comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)

# 감정 점수 부여
word_comment <- word_comment %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))


# 감정 분류하기
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity ==  2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))


# (id, reply)별(문장별) 감정점수 합산
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

# 감정 분류
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >=  1, "pos",
                            ifelse(score <= -1, "neg", "neu")))


# -------------------------------------------------------------------------
# 토큰화 및 두글자 이상 단어만 남기기
comment <- score_comment %>%
  unnest_tokens(input = reply,          # 단어 기준 토큰화
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &  # 한글 추출
           str_count(word) >= 2)          # 두 글자 이상 추출


# 감정 및 단어별 빈도 구하기
frequency_word <- comment %>%
  count(sentiment, word, sort = T)


# 긍정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "pos")

# 부정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "neg")


# wide form 변환 및 로그 오즈비 구하기
library(tidyr)
comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%  # 중립 제외
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
comment_wide


# 로그 오즈비 구하기
comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))

comment_wide


# 로그 오즈비가 큰 단어 10개씩 추출
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10)

top10 %>% print(n = Inf)


# 로그 오즈비가 동점인 단어 제외하고 추출
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10 


# 막대 그래프 만들기
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))



###################################################################
# 예제4-3-2 : 감정 사전 수정하기
###################################################################

# 감정 단어가 사용된 원문 살펴보기

# "소름"이 사용된 댓글
score_comment %>%
  filter(str_detect(reply, "소름")) %>%
  select(reply)

# "미친"이 사용된 댓글
score_comment %>%
  filter(str_detect(reply, "미친")) %>%
  select(reply)


# 감정사전에서 감정 점수
dic %>% filter(word %in% c("소름", "소름이", "미친"))


# 감정사전 수정하기
new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% c("소름", "소름이", "미친"), 2, polarity))

new_dic %>% filter(word %in% c("소름", "소름이", "미친"))


# 수정한 사전으로 감정점수 부여하기
new_word_comment <- word_comment %>%
  select(-polarity) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))


# 댓글별 감정점수
new_score_comment <- new_word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

new_score_comment %>%
  select(score, reply) %>%
  arrange(-score)


# 감정경향 살펴보기
# 1점 기준으로 긍정 중립 부정 분류
new_score_comment <- new_score_comment %>%
  mutate(sentiment = ifelse(score >=  1, "pos",
                            ifelse(score <= -1, "neg", "neu")))

# 원본 감정 사전 활용
score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

# 수정한 감정 사전 활용
new_score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)


# 분석결과 비교하기
word <- "소름|소름이|미친"

# 원본 감정 사전 활용
score_comment %>%
  filter(str_detect(reply, word)) %>%
  count(sentiment)

# 수정한 감정 사전 활용
new_score_comment %>%
  filter(str_detect(reply, word)) %>%
  count(sentiment)


####################################################
# 참고 : 신조어에 감정 점수를 부여하는 방법
df <- tibble(sentence = c("이번 에피소드 쩐다", 
                          "이 영화 핵노잼")) %>% 
  unnest_tokens(input = sentence, 
                output = word, 
                token = "words", 
                drop = F)

df %>% 
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  group_by(sentence) %>% 
  summarise(score = sum(polarity))


# 신조어 목록 생성
newword <- tibble(word = c("쩐다", "핵노잼"), 
                  polarity = c(2, -2))

# 사전에 신조어 추가
newword_dic <- bind_rows(dic, newword)

# 새 사전으로 감정 점수 부여
df %>% 
  left_join(newword_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  group_by(sentence) %>% 
  summarise(score = sum(polarity))
####################################################



# 토큰화 및 전처리
new_comment <- new_score_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &
           str_count(word) >= 2)

# 감정 및 단어별 빈도 구하기
new_frequency_word <- new_comment %>%
  count(sentiment, word, sort = T)


# -------------------------------------------------------------------------
# Wide form으로 변환
new_comment_wide <- new_frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기
new_comment_wide <- new_comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))


# 로그 오즈비가 큰 단어로 막대그래프 만들기
new_top10 <- new_comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

# 막대 그래프 만들기
ggplot(new_top10, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))



# 주요 단어가 사용된 댓글 살펴보기
# 긍정 댓글 원문
new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "축하")) %>%
  select(reply)

new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "소름")) %>%
  select(reply)


# 부정 댓글 원문
new_score_comment %>%
  filter(sentiment == "neg" & str_detect(reply, "좌빨")) %>%
  select(reply)

new_score_comment %>%
  filter(sentiment == "neg" & str_detect(reply, "못한")) %>%
  select(reply)



# 분석결과 비교하기
# 원본 감정 사전 활용
top10 %>% 
  select(-pos, -neg) %>% 
  arrange(-log_odds_ratio)

# 수정한 감정 사전 활용
new_top10 %>%
  select(-pos, -neg) %>%
  arrange(-log_odds_ratio)


# -------------------------------------------------------------------------
new_comment_wide %>%
  filter(word == "미친")


