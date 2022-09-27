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
