# 필요한 패키지 load
library(KoNLP)      # to use useSejongDic(), extractNoun(), useNIADic()  
library(readr)      # to use read_file(), read_csv()
library(dplyr)      # to use %>%, select(), as_tibble(), mutate(), count(), filter(), arrange(), ...   
# package for Data Manipulation
library(stringr)    # to use str_replace_all(), str_squish()
# stringr 패키지에 있는 함수를 이용하여 사전처리
library(tidytext)   # to use unnest_tokens(), cast_dtm()   
# 텍스트 포맷의 데이터를 여러 토큰(token) 단위로 쪼개서 분석하는데 필요한 패키지
library(tidyr)
library(widyr)

library(textclean)

library(ggplot2)
library(ggwordcloud)   # to use geom_text_wordcloud()  # ggplot()에서 워드 클라우드 만들도록 도와주는 패키지  

library(tidygraph)           #to use as_tbl_graph()
library(ggraph)
library(showtext)


###################################################################
# 예제6-1-1 : LDA모형 만들기 전 사전작업
###################################################################

# 기생충 기사 댓글 불러오기
raw_news_comment <- read_csv("news_comment_parasite.csv") %>%
  mutate(id = row_number())


# 기본적인 전처리
news_comment <- raw_news_comment %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply)) %>%
  
  # 중복 댓글 제거
  distinct(reply, .keep_all = T) %>%
  
  # 짧은 문서 제거 - 3 단어 이상 추출
  filter(str_count(reply, boundary("word")) >= 3)


# -------------------------------------------------------------------------
# 명사 추출
comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1) %>%
  
  # 댓글 내 중복 단어 제거
  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  select(id, word)

comment


# 빈도가 높은 단어 제거 : 빈도가 높은 단어는 거의 모든 댓글에 나타남
count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)


# 불용어, 유의어 확인하기
count_word %>%
  count(word, sort = T) %>%
  print(n = 200)

# 불용어 목록 만들기
stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼")


# 불용어, 유의어 처리하기
count_word <- count_word %>%
  filter(!word %in% stopword) %>%
  mutate(word = recode(word,
                       "자랑스럽습니" = "자랑",
                       "자랑스럽" = "자랑",
                       "자한" = "자유한국당",
                       "문재" = "문재인",
                       "한국의" = "한국",
                       "그네" = "박근혜",
                       "추카" = "축하",
                       "정경" = "정경심",
                       "방탄" = "방탄소년단"))


#####################################################################
# 참고 : 불용어 목록을 파일로 만들어 활용하면 편리
# tibble 구조로 불용어 목록 만들기
stopword <- tibble(word = c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
                            "해요", "이것", "니들", "하기", "하지", "한거", "해주",
                            "그것", "어디", "여기", "까지", "이거", "하신", "만큼"))

# 불용어 목록 저장하기
write_csv(stopword, "stopword.csv")

# 불용어 목록 불러오기
stopword <- read_csv("stopword.csv")

# 불용어 제거하기
count_word <- count_word %>%
  filter(!word %in% stopword$word)


count_word <- count_word %>%
  anti_join(stopword, by = "word")
#####################################################################



###################################################################
# 예제6-1-2 : LDA 모형 만들기
###################################################################

# 문서별 단어 빈도 구하기
count_word_doc <- count_word %>%
  count(id, word, sort = T)

count_word_doc


# DTM 만들기 : cast_dtm()
dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)

dtm_comment


# DTM (일부) 내용 확인
as.matrix(dtm_comment)[1:8, 1:8]


# LDA 모델 만들기 : LDA()
install.packages("topicmodels")
library(topicmodels)

# 토픽 모델 만들기
lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = "Gibbs",
                 control = list(seed = 1234))
lda_model

# 모델 내용 확인
glimpse(lda_model)
