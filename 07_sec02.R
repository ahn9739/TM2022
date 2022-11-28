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


# 주요 단어 추출하기
word_noun <- tada %>%
  unnest_tokens(input = reply,
                output = word,
                token = extractNoun,
                drop = F)

# 단어 빈도 구하기
frequency <- word_noun %>%
  count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
  filter(str_count(word) > 1)  # 두 글자 이상만 남기기


# 불용어 제거하기
# 불용어 목록 생성
stopword_noun <- c("들이", "하면", "하게", "해서")

# 주요 단어 목록 만들기
top20_noun <- frequency %>%
  filter(!word %in% stopword_noun) %>%
  head(20)

top20_noun 


####################################################################
# 예제7-1-2 : 공감/비공감 댓글 비교하기
####################################################################

# 공감 여부 변수 만들기
word_sympathy <- word_noun %>%
  rename(like = sympathyCount,
         dislike = antipathyCount) %>%
  
  mutate(diff = like - dislike,
         sympathy = ifelse(diff >=  1, "like",
                           ifelse(diff <= -1, "dislike", "neutral")))


# 관심 단어 가 사용된 텍스트를 추출해 스타일을 입히는 함수 만들기
find_word <- function(df, x, keyword, n = 6) {
  
  # 스타일 함수 설정
  font <- combine_styles(make_style("ivory"),
                         make_style("deeppink", bg = TRUE),
                         make_style("bold"))
  
  # 키워드 추출해 스타일 적용
  df %>%
    filter(str_detect({{x}}, keyword)) %>%                  # 키워드 추출
    head(n) %>%                                             # n행 추출
    mutate(x = paste0("[", row_number(), "] ", {{x}}),      # 행번호 삽입
           x = paste0(str_replace_all(x,
                                      keyword,
                                      font(keyword)))) %>%  # 스타일 적용
    pull(x) %>%                                             # 텍스트 추출
    cat(sep = "\n\n")                                       # 줄바꿈 출력
}


####################################################################
####################################################################
# 예제7-2-1 : 관심 댓글 비교하기
####################################################################
# TF-IDF 구하기

# 카테고리별 주요 단어 목록을 만든 다음, 주요 단어를 언급한 댓글을 추출해 카테고리 이름을 부여하고 결합
# 단어 목록 생성
category1 <- "택시 업계|택시업계|조합"
category2 <- "정부"
category3 <- "국회의원|한국당|자유한국당|자한당|자한|민주당|더불어민주당"

# 추출 및 결합
bind_category <- bind_rows(
  word_sympathy %>%
    filter(str_detect(reply, category1)) %>%
    mutate(category = "택시업계"),
  
  word_sympathy %>%
    filter(str_detect(reply, category2)) %>%
    mutate(category = "정부"),
  
  word_sympathy %>%
    filter(str_detect(reply, category3)) %>%
    mutate(category = "국회의원"))


# 카테고리별 빈도
bind_category %>%
  group_by(id) %>%
  distinct(category, .keep_all = T) %>%
  ungroup() %>%
  count(category)


# TF-IDF 구하기 : 불용어 목록 생성
stopword_category <- c("택시 업계", "택시업계", "업계", "조합",
                       "정부", "국회의원", "한국당", "자유한국당",
                       "자한당", "자한", "민주당", "더불어민주당")


# 중복 단어 제거하고 카테고리별 단어 빈도 구하기
frequency_category <- bind_category %>%
  filter(!word %in% stopword_category) %>%  # 불용어 제외
  
  group_by(id) %>%                          # 댓글별 분리
  distinct(word, .keep_all = T) %>%         # 댓글 내 중복 단어 제거
  ungroup() %>%                             # 그룹 해제
  
  count(category, word, sort = T) %>%       # 카테고리별 단어 빈도
  filter(str_count(word) >= 2)              # 2글자 이상 추출


# tf-idf 구하기
tfidf_category <- frequency_category %>%
  bind_tf_idf(term = word,
              document = category,
              n = n) %>%
  arrange(-tf_idf)

tfidf_category


# 카테고리별 주요 단어 비교하기
# 불용어 목록 만들기 : 주요 단어 추출, 불용어 확인
tfidf_category %>%
  group_by(category) %>%
  slice_max(tf_idf, n = 15, with_ties = F) %>%
  print(n = Inf)


# 불용어 목록 생성
stopword_tfidf <- c("국회의윈님하고", "현정부", "에휴")


# 불용어 제거하고 카테고리별로 tf-idf가 가장 높은 단어 10개씩 추출
top10 <- tfidf_category %>%
  filter(!word %in% stopword_tfidf) %>%
  group_by(category) %>%
  slice_max(tf_idf, n = 10, with_ties = F)


# 막대그래프
# 그래프 순서 정하기
top10$category <- factor(top10$category,
                         levels = c("택시업계", "정부", "국회의원"))

# 막대 그래프 만들기
ggplot(top10, aes(x = reorder_within(word, tf_idf, category),
                  y = tf_idf,
                  fill = category)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ category, scales = "free", ncol = 3) +
  scale_x_reordered() +
  scale_y_continuous(n.breaks = 5,
                     labels = number_format(accuracy = .001)) +
  
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "카테고리별 TF-IDF Top 10",
       x = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 11))  # 카테고리명 폰트


# 카테고리별 댓글 내용 살펴보기
# 중복 댓글 제거
reply_category <- bind_category %>%
  group_by(category) %>%
  distinct(id, .keep_all = T)


# 댓글 내용 살펴보기
# 택시업계 카테고리
reply_category %>%
  filter(category == "택시업계") %>%
  find_word(x = reply_raw, keyword = "대기업")

# 정부 카테고리
reply_category %>%
  filter(category == "정부") %>%
  find_word(x = reply_raw, keyword = "지원")

# 국회의원 카테고리
reply_category %>%
  filter(category == "국회의원") %>%
  find_word(x = reply_raw, keyword = "박홍근")


####################################################################
# 예제7-2-2 : 단어간 관계 살펴보기
####################################################################

# 파이계수(단어간 상관관계) 구하기
# 토큰화
pos_tada <- tada %>%
  unnest_tokens(input = reply,
                output = word_pos,
                token = SimplePos22,
                drop = F)

# 품사 태그 정리
separate_pos_tada <- pos_tada %>%
  separate_rows(word_pos, sep = "[+]") %>%                   # 품사 태그 분리
  filter(str_detect(word_pos, "/n|/pv|/pa")) %>%             # 품사 추출
  mutate(word = ifelse(str_detect(word_pos, "/pv|/pa"),      # "/pv", "/pa" 추출
                       str_replace(word_pos, "/.*$", "다"),  # "~다"로 바꾸기
                       str_remove(word_pos, "/.*$"))) %>%    # 태그 제거
  filter(str_count(word) >= 2) %>%                           # 2글자 이상 추출
  arrange(id)

separate_pos_tada %>%
  select(word)


# 파이계수 구하기
library(widyr)
word_cors <- separate_pos_tada %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word, feature = id, sort = T)

word_cors


# 관심 단어와 관련성이 큰 단어로 막대그래프 만들기
target <- c("타다", "정부", "택시")

# 상위 10개 추출
top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 10)

top_cors


# 막대그래프
# 그래프 순서 정하기
top_cors$item1 <- factor(top_cors$item1, levels = target)

# 막대 그래프 만들기
ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "파이 계수 Top 10",
       x = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        strip.text = element_text(size = 11))


# -------------------------------------------------------------------------
# 네트워크 그래프 데이터 만들기
library(tidygraph)
set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),       # 중심성
         group = as.factor(group_infomap()))     # 커뮤니티

# 네트워크 그래프 만들기
library(ggraph)
set.seed(1234)
ggraph(graph_cors, layout = "fr") +              # 레이아웃
  
  geom_edge_link(color = "gray50",               # 엣지 색깔
                 aes(edge_alpha = correlation,   # 엣지 명암
                     edge_width = correlation),  # 엣지 두께
                 show.legend = F) +              # 범례 삭제
  scale_edge_width(range = c(1, 4)) +            # 엣지 두께 범위
  
  geom_node_point(aes(size = centrality,         # 노드 크기
                      color = group),            # 노드 색깔
                  show.legend = F) +             # 범례 삭제
  scale_size(range = c(5, 10)) +                 # 노드 크기 범위
  
  geom_node_text(aes(label = name),              # 텍스트 표시
                 repel = T,                      # 노드밖 표시
                 size = 5,                       # 텍스트 크기
                 family = "nanumgothic") +       # 폰트
  
  theme_graph()                                  # 배경 삭제



# 몇가지 눈에 띄는 단어쌍(선거-내년, 내년-총선)을 언급한 댓글 내용 살펴보기
tada %>%
  filter(str_detect(reply_raw, "선거")) %>%
  find_word(x = reply_raw, keyword = "내년", n = 10)

tada %>%
  filter(str_detect(reply_raw, "내년")) %>%
  find_word(x = reply_raw, keyword = "총선", n = 10)

tada %>%
  filter(str_detect(reply_raw, "목적지")) %>%
  find_word(x = reply_raw, keyword = "손님", n = 10)

tada %>%
  filter(str_detect(reply_raw, "손님")) %>%
  find_word(x = reply_raw, keyword = "골라", n = 10)


# 바이그램으로 토큰화해 빈도구하기
# 한 댓글이 하나의 행을 구성하도록 결합
line_comment <- separate_pos_tada %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

# 바이그램으로 토큰화
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)

bigram_comment


# 바이그램 분리하기
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()

pair_bigram


# -------------------------------------------------------------------------
# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),       # 중심성
         group = as.factor(group_infomap()))     # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +            # 레이아웃
  
  geom_edge_link(color = "gray50",               # 엣지 색깔
                 alpha = 0.5) +                  # 엣지 명암
  
  geom_node_point(aes(size = centrality,         # 노드 크기
                      color = group),            # 노드 색깔
                  show.legend = F) +             # 범례 삭제
  scale_size(range = c(5, 15)) +                 # 노드 크기 범위
  
  geom_node_text(aes(label = name),              # 텍스트 표시
                 repel = T,                      # 노드밖 표시
                 size = 5,                       # 텍스트 크기
                 family = "nanumgothic") +       # 폰트
  
  theme_graph()                                  # 배경 삭제


# 눈에 띄는 바이그램 단어쌍을 언급한 댓글 출력해 내용 살펴보기
line_tada <- line_comment %>%
  left_join(tada, by = "id")

line_tada %>%
  select(sentence)


# -------------------------------------------------------------------------
line_tada %>%
  filter(str_detect(sentence, "시대 역행")) %>%
  find_word(x = reply_raw, keyword = "역행", n = 10)

line_tada %>%
  filter(str_detect(sentence, "시대 뒤떨어지다")) %>%
  find_word(x = reply_raw, keyword = "뒤떨어", n = 10)

line_tada %>%
  filter(str_detect(sentence, "택시 면허")) %>%
  find_word(x = reply_raw, keyword = "면허", n = 10)

line_tada %>%
  filter(str_detect(sentence, "면허 사다")) %>%
  find_word(x = reply_raw, keyword = "사서", n = 10)
