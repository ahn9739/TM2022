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

#install.packages("scales")
library(scales)

####################################################################
# 텍스트 마이닝 프로젝트 : 타다금지법 기사 댓글 분석
# 타다(TADA) : 수도권 지역에서의 렌터카 서비스를 주력으로 시작했다가, 
#              2021년 현재는 수도권과 부산광역시에서의 가맹형 콜택시 
#              서비스 영업을 주력으로 하는 대한민국의 모빌리티 브랜드
####################################################################
# 예제7-1-1 : 전처리 및 주요단어 살펴보기
####################################################################

# 데이터 불러오기
raw_tada <- read_csv("news_comment_tada.csv") %>%
  mutate(id = row_number())

# 사전처리
tada <- raw_tada %>%
  filter(str_count(reply, " ") >= 1) %>%                   # 띄어쓰기 1개 이상 추출
  mutate(reply_raw = str_squish(replace_html(reply)),      # 원문 보유
         reply = str_replace_all(reply, "[^가-힣]", " "),  # 한글만 남기기
         reply = str_squish(reply))                        # 중복 공백 제거



####################################################################
####################################################################
# 예제7-3-1 : 토픽 모델링
####################################################################

# 모델링을 위한 전처리 : 명사 추출
noun_tada <- tada %>%
  distinct(reply, .keep_all = T) %>%                   # 중복 댓글 제거
  filter(str_count(reply, boundary("word")) >= 3) %>%  # 짧은 댓글 제거
  unnest_tokens(input = reply,                         # 명사 추출
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1)

# 중복, 고빈도 단어 제거
unique_noun_tada <- noun_tada %>%
  group_by(id) %>%                                     # 중복 단어 제거
  distinct(word, .keep_all = T) %>%
  ungroup() %>%
  add_count(word) %>%                                  # 고빈도 단어 제거
  filter(n <= 200) %>%
  select(id, word)

unique_noun_tada


# LDA 모델 만들기
# 문서별 단어 빈도를 이용해 DTM 만들기
# 문서별 단어 빈도 구하기
count_word <- unique_noun_tada %>%
  count(id, word, sort = T)

# DTM 만들기
dtm_tada <- count_word %>%
  cast_dtm(document = id, term = word, value = n)

dtm_tada


# 토픽 수를 정하기 위한 튜닝 (2~20개 까지 모델)
library(ldatuning)
models_tada <- FindTopicsNumber(dtm = dtm_tada,
                                topics = 2:20,
                                return_models = T,
                                control = list(seed = 1234))

# 성능 지표 그래프
FindTopicsNumber_plot(models_tada)

# 토픽 수가 9개인 모델 추출
lda_model <- models_tada %>%
  filter(topics == 9) %>%
  pull(LDA_model) %>%              # 모델 추출
  .[[1]]                           # list 추출

lda_model


# 토픽별 주요 단어 살펴보기

# 토픽별 단어 확률 beta 추출
term_topic <- tidy(lda_model, matrix = "beta")

# 토픽별 beta 상위 단어 추출
term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 15) %>%
  print(n = Inf)


# 불용어 제외 후 토픽별 주요 단어를 살펴보기 위해
# 불용어 목록 생성
stopword_lda <- c("하게", "하다", "하려", "해라", "그것", "하면", "하네",
                  "하기", "하나", "해서", "하면", "하지", "한거", "니들")

# 불용어 제외 후 상위 10개 단어 추출
top_term_topic <- term_topic %>%
  filter(!term %in% stopword_lda) %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)

top_term_topic

# 막대그래프
ggplot(top_term_topic, aes(x = reorder_within(term, beta, topic),
                           y = beta,
                           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))



####################################################################
# 예제7-3-2 : 토픽별로 댓글 분류, 댓글 수, 단어 시각화
####################################################################

# 토픽별로 댓글을 분류하기 위해
# 문서별 토픽 확률 gamma 추출하기
doc_topic <- tidy(lda_model, matrix = "gamma")

# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)

doc_class


# integer로 변환
doc_class$document <- as.integer(doc_class$document)

# 원문에 토픽 번호 부여
tada_topic <- tada %>%
  left_join(doc_class, by = c("id" = "document"))


# 토픽별 댓글 수와 단어 시각화하기
# 토픽별 주요 단어 목록 만들기
top_terms <- term_topic %>%
  filter(!term %in% stopword_lda) %>%
  group_by(topic) %>%
  slice_max(beta, n = 6, with_ties = F) %>%
  summarise(term = paste(term, collapse = ", "))

top_terms

# 토픽별 문서 빈도 구하기
count_topic <- tada_topic %>%
  count(topic) %>%
  na.omit()

count_topic


# 문서 빈도에 주요 단어 결합하기
count_topic_word <- count_topic %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic_name = paste("Topic", topic))

count_topic_word


# 막대그래프
library(scales)
ggplot(count_topic_word,
       aes(x = reorder(topic_name, n),
           y = n,
           fill = topic_name)) +
  geom_col(show.legend = F) +
  coord_flip() +
  
  geom_text(aes(label = comma(n, accuracy = 1)),  # 문서 빈도 표시
            hjust = -0.2) +
  
  geom_text(aes(label = term),                    # 주요 단어 표시
            hjust = 1.03,
            col = "white",
            fontface = "bold",
            family = "nanumgothic") +
  
  scale_y_continuous(expand = c(0, 0),            # y축-막대 간격 줄이기
                     limits = c(0, 1100)) +       # y축 범위
  
  labs(title = "타다 금지법 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 및 댓글 빈도",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))


####################################################################
# 예제7-3-3 : 토픽 이름 짓기
####################################################################

# 토픽별 주요 문서 추출
reply_topic <- tada_topic %>%
  group_by(topic) %>%
  slice_max(gamma, n = 100)


# 토픽 1 내용 살펴보기
reply_topic %>%
  filter(topic == 1) %>%
  pull(reply_raw)

# 토픽 2 내용 살펴보기
reply_topic %>%
  filter(topic == 2) %>%
  pull(reply_raw)


# 토픽 이름 목록 만들기
name_topic <- tibble(topic = 1:9,
                     name = c("1. 신사업 가로막는 국회",
                              "2. 시대 흐름 역행하는 법안",
                              "3. 택시 업계 보호, 국민 무시",
                              "4. 자유 시장경제 반하는 결정",
                              "5. 불만족스러운 택시 서비스",
                              "6. 국가 발전 가로막는 정부",
                              "7. 기존 업계 밥그릇 지키는 정치인",
                              "8. 총선만 신경 쓰는 국회의원",
                              "9. 타다는 렌트카, 무면허 택시 안된다"))


# 토픽 이름 결합하기
top_term_topic_name <- top_term_topic %>%
  left_join(name_topic, name_topic, by = "topic")

top_term_topic_name


# 막대 그래프 만들기
ggplot(top_term_topic_name,
       aes(x = reorder_within(term, beta, name),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ name, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "타다 금지법 기사 댓글 토픽",
       subtitle = "토픽별 주요 단어 Top 10",
       x = NULL, y = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text.x = element_blank(),   # x축 이름 삭제
        axis.ticks.x = element_blank())  # x축 눈금 삭제

