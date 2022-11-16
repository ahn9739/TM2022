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
#install.packages("topicmodels")
library(topicmodels)

# 토픽 모델 만들기
lda_model <- LDA(dtm_comment,
                 k = 8,
                 method = "Gibbs",
                 control = list(seed = 1234))
lda_model

# 모델 내용 확인
glimpse(lda_model)





###################################################################
###################################################################
# 예제6-2-1 : 토픽별 주요 단어 
###################################################################

# beta 추출하기 : 단어가 각 토픽에 등장할 확률
term_topic <- tidy(lda_model, matrix = "beta")
term_topic

# gamma 추출하기 : 문서가 각 토픽에 등장할 확률
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic


# beta 살펴보기
# 토픽별 단어 수
term_topic %>%
  count(topic)

# 토픽 1의 beta 합계
term_topic %>%
  filter(topic == 1) %>%
  summarise(sum_beta = sum(beta))

# 특정 단어의 토픽별 확률
term_topic %>%
  filter(term == "작품상")


# 특정 토픽 (예: 토픽 6)에서 beta가 높은 단어 살펴보기
term_topic %>%
  filter(topic == 6) %>%
  arrange(-beta)


# 모든 토픽의 주요 단어 살펴보기 
# terms() - 토픽별로 등장 확률이 높은 단어 추출
terms(lda_model, 20) %>%
  data.frame()


# 토픽별 beta 상위 10개 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)


###################################################################
# 예제6-2-2 : 막대 그래프 
###################################################################
#install.packages("scales")
library(scales)
library(ggplot2)

ggplot(top_term_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 4,
                     labels = number_format(accuracy = .01)) + 
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))



###################################################################
# 예제6-2-3 : 문서를 토픽별로 분류
###################################################################

# gamma 추출
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic


# 3203개 문서, 3203x8
doc_topic %>%
  count(topic)

# 문서 1의 gamma 합계
doc_topic %>%
  filter(document == 1) %>%
  summarise(sum_gamma = sum(gamma))


# 문서를 확률이 가장 높은 토픽으로 분류하기
# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_class


# 원문에 확률이 가장 높은 토픽 번호 부여하기
# integer로 변환
doc_class$document <- as.integer(doc_class$document)

# 원문에 토픽 번호 부여
news_comment_topic <- raw_news_comment %>%
  left_join(doc_class, by = c("id" = "document"))


# 결합 확인
news_comment_topic %>%
  select(id, topic)


# 토픽별 문서 수
news_comment_topic %>%
  count(topic)


# topic이 NA인 문서 제거
news_comment_topic <- news_comment_topic %>%
  na.omit()


###########################################################################
# 문서를 한 토픽으로만 분류하기
doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  count(document) %>%
  filter(n >= 2)


# -------------------------------------------------------------------------
set.seed(1234)
doc_class_unique <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1) %>%
  slice_sample(n = 1)

doc_class_unique


# 문서 빈도 구하기
doc_class_unique %>%
  count(document, sort = T)
##########################################################################



# 토픽별 문서 수와 단어 시각화
# 토픽별 주요 단어 목록
top_terms <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_terms


# 토픽별 문서 빈도
count_topic <- news_comment_topic %>%
  count(topic)

count_topic


# 문서 빈도에 주요 단어 결합
count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

count_topic_word



###################################################################
# 예제6-2-4 : 토픽별 문서 수와 주요 단어 그래프
###################################################################
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  
  geom_text(aes(label = n) ,                # 문서 빈도 표시
            hjust = -0.2) +                 # 막대 밖에 표시
  
  geom_text(aes(label = term),              # 주요 단어 표시
            hjust = 1.03,                   # 막대 안에 표시
            col = "white",                  # 색깔
            fontface = "bold",              # 두껍게
            family = "nanumgothic") +       # 폰트
  
  scale_y_continuous(expand = c(0, 0),      # y축-막대 간격 줄이기
                     limits = c(0, 820)) +  # y축 범위
  labs(x = NULL)
