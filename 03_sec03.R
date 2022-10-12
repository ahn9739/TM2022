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

# read the data
raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon")

raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")



###################################################################
# 예제3-3-1 : 집단별 단어 빈도 구하기
###################################################################

# 데이터 합치기
bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)

head(bind_speeches)
tail(bind_speeches)


# 기본적인 전처리 및 토큰화
speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches


# count 함수 사용 예
df <- tibble(class = c("a", "a", "a", "b", "b", "b"),
             sex = c("female", "male", "female", "male", "male", "female"))
df %>% count(class, sex)


# 하위집단별(대통령별) 사용단어 빈도 구하기
frequency <- speeches %>%
  count(president, word) %>%   # 연설문 및 단어별 빈도
  filter(str_count(word) > 1)  # 두 글자 이상 추출

head(frequency)



###################################################################
# 예제3-3-2 : 자주 사용된 단어 추출하기
###################################################################
# 연설문에 가장 많이 사용된 단어 추출하기
top10 <- frequency %>%
  group_by(president) %>%  # president별로 분리
  slice_max(n, n = 10)     # 상위 10개 추출

top10


# 같은 횟수 출현한 단어 확인 
top10 %>%
  filter(president == "park")


# 빈도 동점 단어 제외
top10 <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)

top10



###################################################################
# 예제3-3-3 : 막대그래프로 표현
###################################################################

# 변수의 항목별 그래프
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president) #변수 항목별로 그래프 생성 함수


# 스래프별로 y축 설정하기
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president,         # president별 그래프 생성
             scales = "free_y")   # y축 통일하지 않음


### 특정 단어 제거하고 그래프 생성
# 특정 단어 제거
top10 <- frequency %>%
  filter(word != "국민") %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)
top10

# 그래프 생성
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")


# 축 정렬하기
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")


# 변수항목 제거하기
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL) +                                    # x축 삭제
  theme(text = element_text(family = "nanumgothic"))  # 폰트



###################################################################
# 예제3-3-4 : 오즈비 - 상대적으로 중요한 단어 비교하기
###################################################################
#처음부터 에제3-3-1 까지 실행한 후

df_long <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10) %>%
  filter(word %in% c("국민", "우리", "정치", "행복"))

df_long


# wide form으로 변환
#install.packages("tidyr")
library(tidyr)

df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n)

df_wide


# NA -> 0 으로 변환
df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))

df_wide


# 연설문 단어빈도 wide form으로 변환
frequency_wide <- frequency %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))

frequency_wide


# 오즈비 구하기 위해 단어의 비중을 나타내는 변수 추가
frequency_wide <- frequency_wide %>%
  mutate(ratio_moon = ((moon + 1)/(sum(moon + 1))),  # moon에서 단어의 비중
         ratio_park = ((park + 1)/(sum(park + 1))))  # park에서 단어의 비중

frequency_wide


# 오즈비 변수 추가
frequency_wide <- frequency_wide %>%
  mutate(odds_ratio = ratio_moon/ratio_park)

frequency_wide %>%
  arrange(-odds_ratio)   # 내림차순

frequency_wide %>%
  arrange(odds_ratio)   # 오름차순


# 오즈비 구하는 간단한 code 1
frequency_wide <- frequency_wide %>%
  mutate(ratio_moon  = ((moon + 1)/(sum(moon + 1))),
         ratio_park  = ((park + 1)/(sum(park + 1))),
         odds_ratio = ratio_moon/ratio_park)


# 오즈비 구하는 간단한 code 2
frequency_wide <- frequency_wide %>%
  mutate(odds_ratio = ((moon + 1)/(sum(moon + 1)))/
           ((park + 1)/(sum(park + 1))))


# 오즈비가 가장 높거나 낮은 단어 추출 
top10 <- frequency_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)

top10 %>%
  arrange(-odds_ratio)


# rank() 사용 예
df <- tibble(x = c(2, 5, 10))
df %>% mutate(y = rank(x))     # 값이 작을수록 앞순위
df %>% mutate(y = rank(-x))    # 값이 클수록 앞순위


# 어느 연설문에서 비중이 큰 단어인지 나타내는 변수 추가
top10 <- top10 %>%
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))

top10



###################################################################
# 예제3-3-5 : 막대그래프
###################################################################

# top10 막대그래프
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered()


# 그래프별 축 설정
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL) +                                    # x축 삭제
  theme(text = element_text(family = "nanumgothic"))  # 폰트



###################################################################
# 예제3-3-6 : 주요 단어가 사용된 문장 살펴보기
###################################################################

# 문장 기준으로 토큰화
speeches_sentence <- bind_speeches %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")

head(speeches_sentence)
tail(speeches_sentence)


# 주요 단어가 사용된 문장 추출
speeches_sentence %>%
  filter(president == "moon" & str_detect(sentence, "복지국가"))

speeches_sentence %>%
  filter(president == "park" & str_detect(sentence, "행복"))


# 중요도가 비슷한 단어
frequency_wide %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)

# 중요도가 비슷하면서 빈도가 높은 단어
frequency_wide %>%
  filter(moon >= 5 & park >= 5) %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)




###################################################################
# 예제3-3-7 : 로그 오즈비로 단어 비교하기
###################################################################

# 로그 오즈비 변수 추가
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(odds_ratio))

# moon에서 비중이 큰 단어
frequency_wide %>%
  arrange(-log_odds_ratio)

# park에서 비중이 큰 단어
frequency_wide %>%
  arrange(log_odds_ratio)

# 비중이 비슷한 단어
frequency_wide %>%
  arrange(abs(log_odds_ratio))


# 로그 오즈비
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((moon + 1) / (sum(moon + 1))) /
                                ((park + 1) / (sum(park + 1)))))


# 로그 오즈비 이용 중요한 단어 비교
top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10 %>% 
  arrange(-log_odds_ratio) %>% 
  select(word, log_odds_ratio, president)


# 막대 그래프
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))




###################################################################
# 예제3-3-8 : TF-IDF - 여러 텍스트의 단어 비교하기
###################################################################

# 데이터 불러오기
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
  count(president, word) %>%
  filter(str_count(word) > 1)

frequency


# TF-IDF 구하기
frequency <- frequency %>%
  bind_tf_idf(term = word,           # 단어
              document = president,  # 텍스트 구분 변수
              n = n) %>%             # 단어 빈도
  arrange(-tf_idf)

frequency

# TF-IDF가 높은 단어 
frequency %>% filter(president == "문재인")

frequency %>% filter(president == "박근혜")

frequency %>% filter(president == "이명박")

frequency %>% filter(president == "노무현")


# TF-IDF가 낮은 단어 
frequency %>%
  filter(president == "문재인") %>%
  arrange(tf_idf)

frequency %>%
  filter(president == "박근혜") %>%
  arrange(tf_idf)


# 그래프 생성을 위한 주요 단어 추출
top10 <- frequency %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

# 그래프 순서 정하기
top10$president <- factor(top10$president,
                          levels = c("문재인", "박근혜", "이명박", "노무현"))

# 막대 그래프 만들기
ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +  
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

