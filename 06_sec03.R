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
 ##############-> 4150개 문서

# 기본적인 전처리
news_comment <- raw_news_comment %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply)) %>%
  
  # 중복 댓글 제거
  distinct(reply, .keep_all = T) %>%
  
  # 짧은 문서 제거 - 3 단어 이상 추출
  filter(str_count(reply, boundary("word")) >= 3)
 ##############-> 3329개 문서

length(unique(news_comment$id))
 ##############-> unique한 문서는 3329


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
 ##############-> 21457개 명사

comment

length(unique(comment$id))
 ##############-> unique한 문서는 3286


# 빈도가 높은 단어 제거 : 빈도가 높은 단어는 거의 모든 댓글에 나타남
count_word <- comment %>%
  add_count(word) %>%
  filter(n <= 200) %>%
  select(-n)
 ##############-> 제거 후 18210개 명사 남음
length(unique(count_word$id))
 ##############-> unique한 문서는 3219


# 불용어, 유의어 확인하기
count_word %>%
  count(word, sort = T) %>%
  print(n = 200)
 ##############-> 6022개 명사

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
 ##############-> 처리 후 17604개 명사 남음 (같은 단어 많이 있음)

length(unique(count_word$id))
length(unique(count_word$word))
 ##############-> unique한 문서는 3203, unique한 단어는 5995개


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
 ##############-> 3203개 문서 x 5995 단어


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
 ##############-> 3203개 문서 x 5995 단어


###################################################################
# 예제6-2-1 : 토픽별 주요 단어 
###################################################################

# beta 추출하기 : 단어가 각 토픽에 등장할 확률
term_topic <- tidy(lda_model, matrix = "beta")
term_topic
 ##############-> 47960개 (5995개 단어 x 8개 토픽)

# gamma 추출하기 : 문서가 각 토픽에 등장할 확률
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic
 ##############-> 25624개 (3203개 문서 x 8개 토픽)

# 토픽별 beta 상위 10개 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)
 ##############-> 83개 (동점 beta 값 있음)


###################################################################
# 예제6-2-3 : 문서를 토픽별로 분류
###################################################################

# gamma 추출
doc_topic <- tidy(lda_model, matrix = "gamma")
doc_topic
 ##############-> 25,624 (3203x8) 개 gamma 값

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
 ##############-> 5328 개 gamma 값 (3203 보다 큰 이유는 동점 값이 있기 때문)


# 원문에 확률이 가장 높은 토픽 번호 부여하기
# integer로 변환
doc_class$document <- as.integer(doc_class$document)

# 원문에 토픽 번호 부여
news_comment_topic <- raw_news_comment %>%
  left_join(doc_class, by = c("id" = "document"))
 ##############-> 6275 개 gamma 값 (4150 보다 큰 이유는 동점 값이 있기 때문)
 ##############-> 사전처리에서 제거된 문서는 topic이 NA

# 결합 확인
news_comment_topic %>%
  select(id, topic)


# 토픽별 문서 수
news_comment_topic %>%
  count(topic)
 ##############-> 6275 개 문서 (동점, NA 등 포함)

# topic이 NA인 문서 제거
news_comment_topic <- news_comment_topic %>%
  na.omit()

# 토픽별 문서 수
news_comment_topic %>%
  count(topic)
 ##############-> 5328 (6275-947) 개 문서 (NA 947 제거)


###################################################################
###################################################################
# 예제6-3-1 : 토픽 이름 짓기
###################################################################
# gamma가 높은 순으로 정리
comment_topic <- news_comment_topic %>%
  mutate(reply = str_squish(replace_html(reply))) %>%
  arrange(-gamma)

comment_topic %>%
  select(gamma, reply)


# 주요 단어가 사용된 문서를 살펴보기 위해
# 토픽 1 내용 살펴보기
comment_topic %>%
  filter(topic == 1 & str_detect(reply, "작품")) %>%
  head(50) %>%
  pull(reply)

comment_topic %>%
  filter(topic == 1 & str_detect(reply, "진심")) %>%
  head(50) %>%
  pull(reply)

comment_topic %>%
  filter(topic == 1 & str_detect(reply, "정치")) %>%
  head(50) %>%
  pull(reply)


# 토픽 이름 목록 만들기
name_topic <- tibble(topic = 1:8,
                     name = c("1. 작품상 수상 축하, 정치적 댓글 비판",
                              "2. 수상 축하, 시상식 감상",
                              "3. 조국 가족, 정치적 해석",
                              "4. 새 역사 쓴 세계적인 영화",
                              "5. 자랑스럽고 감사한 마음",
                              "6. 놀라운 4관왕 수상",
                              "7. 문화계 블랙리스트, 보수 정당 비판",
                              "8. 한국의 세계적 위상"))


# 토픽 이름 결합하기
top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")

top_term_topic_name
 ##############-> 83개 (동점 beta 값 있음)


# -------------------------------------------------------------------------
# 막대 그래프 만들기
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "영화 기생충 아카데미상 수상 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 Top 10",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        title = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


###################################################################
# 예제6-3-2 : 최적의 토픽 수 정하기
###################################################################

# 토픽 수 바꿔가며 LDA 모델 여러 개 만들기
#install.packages("ldatuning") # LDA Tunning, to use FindTopicsNumber(), FindTopicsNumber_plot(), ...
library(ldatuning)

models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))

models %>%
  select(topics, Griffiths2004)  #Griffiths2004 : 모델의 성능 지표


# 토픽 수에 따라 모델의 성능 지표를 그래프로 표현
FindTopicsNumber_plot(models)


# -------------------------------------------------------------------------
# 토픽 수가 8개인 모델 추출하기
optimal_model <- models %>%
  filter(topics == 8) %>%
  pull(LDA_model) %>%              # 모델 추출
  .[[1]]                           # list 추출


# -------------------------------------------------------------------------
# optimal_model
tidy(optimal_model, matrix = "beta")


# -------------------------------------------------------------------------
# lda_model
tidy(lda_model, matrix = "beta")


