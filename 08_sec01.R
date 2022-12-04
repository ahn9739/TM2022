# 필요한 패키지 load
library(KoNLP)      # to use useSejongDic(), extractNoun(), useNIADic()  
library(readr)      # to use read_file(), read_csv()
library(dplyr)      # to use %>%, select(), as_tibble(), mutate(), count(), filter(), arrange(), ...   
# package for Data Manipulation
library(tidytext)   # to use unnest_tokens(), cast_dtm()   
# 텍스트 포맷의 데이터를 여러 토큰(token) 단위로 쪼개서 분석하는데 필요한 패키지
library(tidyr)
library(widyr)

library(ggplot2)
library(ggwordcloud)   # to use geom_text_wordcloud()  # ggplot()에서 워드 클라우드 만들도록 도와주는 패키지  

library(tidygraph)           #to use as_tbl_graph()
library(ggraph)
library(showtext)

#install.packages("scales")
library(scales)

#install.packages("lubridate")
library(lubridate) # 날짜와 시간 다루기 위한 패키지
library(textclean)
library(stringr)    # to use str_replace_all(), str_squish()
# stringr 패키지에 있는 함수를 이용하여 사전처리


####################################################################
# 텍스트 마이닝 프로젝트 : SNS 여론 분석
####################################################################
# 예제8-1-1 : 전처리, SNS 언급량 추이, SNS 이슈 살펴보기
####################################################################

# 데이터 불러오기 & 합치기
bind_tweet <- bind_rows(
  read_csv("tweet_nak.csv") %>% mutate(candidate = "이낙연"),
  read_csv("tweet_jae.csv") %>% mutate(candidate = "이재명"))

glimpse(bind_tweet)


# 사전처리
set.seed(1234)
tweet <- bind_tweet %>%
  
  mutate(text = replace_tag(str_to_lower(text)),  # id 태그 제거
         text = str_squish(replace_html(text)),   # html 특수 문자 제거
         date = date(created_at)) %>%             # 날짜 변수 생성
  
  filter(!str_detect(text, "https://")) %>%       # 광고 트윗 제거
  
  group_by(candidate) %>%                         # 중복 글 제거
  distinct(text, .keep_all = T) %>%
  
  group_by(candidate, date, screen_name) %>%      # 사용자별 하루 최대 5개 추출
  slice_max(n = 5, order_by=screen_name) %>% # 교재 코드 수정
  ungroup()

glimpse(tweet)


# 트윗 빈도 초이 선 그래프
# 날짜, 후보별 빈도
frequency_date <- tweet %>%
  count(date, candidate)

frequency_date

# 선 그래프
library(ggplot2)
ggplot(frequency_date, aes(x = date, y = n, col = candidate)) +
  geom_line()


# 그래프 다듬기
# 후보 색상 목록 생성
col_candidate <- c("#619CFF", "#B79F00")

ggplot(frequency_date, aes(x = date, y = n, col = candidate)) +  
  geom_line(size = 1) +
  geom_point(size = 2) +
  
  scale_x_date(date_labels = "%m/%d",                         # x축 날짜 포맷
               date_breaks  = "1 day") +                      # x축 날짜 간격
  scale_y_continuous(limits = c(0, 1300),                     # y축 범위
                     breaks = seq(0, 1300, 300)) +            # y축 간격
  scale_color_manual(values = col_candidate) +                # 선 색깔
  
  labs(title = "차기 대선주자 트위터 언급량 추이",            # 그래프 제목
       subtitle = "2020.8.13 ~ 2020.8.21",                    # 보조 제목
       x = NULL, y = NULL, col = NULL) +                      # 축 이름 삭제
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),  # 제목 폰트
        plot.subtitle = element_text(size = 12),              # 부제목 폰트
        panel.grid.minor.x = element_blank())                 # x축 보조축 삭제




# SNS 이슈
# 단어로 토큰화
word_tweet_raw <- tweet %>%
  unnest_tokens(input = text,
                output = word,
                token = "words",
                drop = F)


# 날짜 분류 및 단어별 빈도 구하기
frequency14 <- word_tweet_raw %>%
  mutate(category = ifelse(date == "2020-08-14", "target", "etc")) %>%
  filter(str_count(word) >= 2) %>%
  count(category, word, sort = T)

frequency14


# 로그 오즈비 구하기
# Wide form으로 변환
wide14 <- frequency14 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 변수 추가
wide14 <- wide14 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc  + 1) / (sum(etc    + 1)))))

# log_odds_ratio 높은 순 출력
wide14 %>%
  arrange(-log_odds_ratio) %>%
  head(20)


# 원문 살펴보기
# 트윗 내용 확인
tweet %>%
  filter(date == "2020-08-14" & str_detect(text, "조사")) %>%
  head(10) %>%
  pull(text)


# 8월 18일~19일 이슈 알아보기
# 날짜 분휴하고 단어별 빈도 구하기
frequency_nak1819 <- word_tweet_raw %>%
  mutate(category = ifelse(date >= "2020-08-18" &
                             date <= "2020-08-19", "target", "etc")) %>%
  filter(candidate == "이낙연" & str_count(word) >= 2) %>%
  count(category, word, sort = T)


# 로그 오즈비 구하기
# Wide form으로 변환
wide_nak1819 <- frequency_nak1819 %>%
  pivot_wider(names_from = category,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 변수 추가
wide_nak1819 <- wide_nak1819 %>%
  mutate(log_odds_ratio = log(((target + 1) / (sum(target + 1))) /
                                ((etc  + 1) / (sum(etc    + 1)))))


# log_odds_ratio 높은 순 출력
wide_nak1819 %>%
  arrange(-log_odds_ratio) %>%
  head(20)


# 원문 살펴보기
# 트윗 내용 확인
tweet %>%
  filter(date >= "2020-08-18" & date <= "2020-08-19" &
           candidate == "이낙연" & str_detect(text, "다행입니다")) %>%
  head(10) %>%
  pull(text)



####################################################################
# 예제8-1-2 : 감정 단어, 감정 경향, 감정 추이 살펴보기
####################################################################
# 감정 단어 살펴보기
# 감정 사전 불러오기
dic <- read_csv("knu_sentiment_lexicon.csv")

# 감정 점수 부여, 감정 극성 분류
word_tweet <- word_tweet_raw %>%  # 단어로 토큰화 해 놓은 데이터 셑
  left_join(dic, by = "word") %>%                              # 감정 점수 부여
  mutate(polarity = ifelse(is.na(polarity), 0, polarity),      # NA를 0으로 변환
         sentiment = ifelse(polarity ==  2, "긍정",            # 감정 범주 분류
                            ifelse(polarity == -2, "부정", "중립")))


# 자주 언급한 단어 추출
top10_word <- word_tweet %>%
  
  # 불용어 제거
  filter(!(candidate == "이낙연" & str_detect(word, "이낙연")) &
           !(candidate == "이재명" & str_detect(word, "이재명"))) %>%
  
  filter(str_count(word) >= 2) %>%
  count(candidate, sentiment, word) %>%
  
  group_by(candidate, sentiment) %>%
  slice_max(n, n = 10, with_ties = F)

top10_word


# 막대 그래프
ggplot(top10_word, aes(x = reorder_within(word, n, candidate),
                       y = n,
                       fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(candidate ~ sentiment,  # 후보, 감정 범주별 그래프 생성
             scales = "free") +
  scale_x_reordered()


# 감정 색깔 및 그래프 순서 변경
col_sentiment <- c("#619CFF", "#00BA38", "#F8766D")  # 감정 색깔 목록
order_sentiment <- c("긍정", "중립", "부정")         # 감정 범주 목록

# 그래프 순서 지정
top10_word$sentiment <- factor(top10_word$sentiment,
                               levels = order_sentiment)

ggplot(top10_word, aes(x = reorder_within(word, n, candidate),
                       y = n,
                       fill = sentiment)) +
  geom_col() +
  coord_flip() +
  facet_wrap(candidate ~ sentiment,
             scales = "free") +
  scale_x_reordered() +
  scale_fill_manual(values = col_sentiment) +
  
  labs(title = "차기 대선주자 감정 단어",
       subtitle = "감정 극성별 빈도 Top 10",
       x = NULL, y = NULL, fill = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.position = "bottom")  # 범례 위치


############### 감정 경향 살펴보기
# 트윗 감정 점수 구하기
sentiment_tweet <- word_tweet %>%
  group_by(candidate, status_id) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

# 트윗 원문에 감정 점수 결합
tweet <- tweet %>%
  left_join(sentiment_tweet, by = c("candidate", "status_id"))

# 감정 점수 히스토그램
hist(tweet$score)


### 감정 범주 살펴보기
# 감정 분류 변수 생성
tweet <- tweet %>%
  mutate(sentiment = ifelse(score >= 1, "긍정",
                            ifelse(score <= -1, "부정", "중립")))

# 후보, 감정별 빈도 및 비율
frequency_sentiment <- tweet %>%
  group_by(candidate) %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n))

frequency_sentiment


# 막대 그래프
ggplot(frequency_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ candidate)


# 감정 색깔 및 그래프 순서 변경
# 순서 설정
frequency_sentiment$sentiment <- factor(frequency_sentiment$sentiment,
                                        levels = order_sentiment)

ggplot(frequency_sentiment, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = F) +                   
  facet_wrap(~ candidate) +
  geom_text(aes(label = comma(n)), vjust = -0.5) +
  
  ylim(0, 3500) +
  scale_fill_manual(values = col_sentiment) +  # 막대 색깔
  
  labs(title = "차기 대선주자 트윗 감정 빈도",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL) +
  
  theme_bw(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank())



####### 감정 추이 살펴보기
# 날짜, 후보, 감정별 빈도
sentiment_candidate <- tweet %>%
  count(date, candidate, sentiment)

sentiment_candidate


## 트윗 감정 추이 선 그래프
ggplot(sentiment_candidate, aes(x = date, y = n, col = sentiment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x")


# 중립 트윗 제외
tweet_polar <- sentiment_candidate %>%
  filter(sentiment != "중립")

ggplot(tweet_polar, aes(x = date, y = n, col = sentiment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x")


# 감정 색깔 변경
# 색깔 목록 생성
col_polar <- c("#619CFF", "#F8766D")

ggplot(tweet_polar, aes(x = date, y = n, col = sentiment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ candidate, nrow = 2, scales = "free_x") +
  
  scale_x_date(date_labels = "%m/%d",
               date_breaks  = "1 day") +
  ylim(0, 250) +
  scale_color_manual(values = col_polar) +
  
  labs(title = "차기 대선주자 트윗 감정 추이",
       subtitle = "2020.8.13 ~ 2020.8.21",
       x = NULL, y = NULL, col = NULL) +
  
  theme_minimal(12) +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(2, "lines"))  # 그래프 간격

