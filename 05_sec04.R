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


#####################################################################
### 샘플 텍스트로 엔그램 토큰화하기
text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")

# 바이그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 2)

# 트라이그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 3)


# 단어 기준 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# 유니그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 1)


###################################################################
# 5.1~5.3 절

# 이용 데이터 : news_comment_parasite.csv
raw_news_comment <- read_csv("news_comment_parasite.csv")

# 전처리
news_comment <- raw_news_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number())


# 품사기준 토큰화
comment_pos <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22,  #단어를 22개의 품사로 구분
                drop = F)

### 품사를 나타내는 태그 : https://github.com/haven-jeon/KoNLP/blob/master/etcs/KoNLP-API.md

# 품사별로 행 분리
comment_pos <- comment_pos %>%
  separate_rows(word, sep = "[+]")
###################################################################


###################################################################
# 예제5-4-1 : 바이그램(bigram)
###################################################################
#명사, 동사, 형용사 추출하고 2글자 이상만 남기기
comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)


# 유의어 처리
comment_new <- comment_new %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "봉준호", word), 
         word = ifelse(word  == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))

comment_new %>%
  select(word)


# 단어 동시 출현 빈도 구하기
pair <- comment_new %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)



# 한 댓글이 하나의 행이 되도록 id별로 word 결합
line_comment <- comment_new %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))

line_comment


# 바이그램으로 토큰화하기
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)

bigram_comment


# 바이그램 분리하기
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_seprated


# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()

pair_bigram


# 단어쌍 살펴보기
# 동시 출현 단어쌍
pair %>%
  filter(item1 == "대한민국")

# 바이그램 단어쌍
pair_bigram %>%
  filter(word1 == "대한민국")

# 동시 출현 단어쌍
pair %>%
  filter(item1 == "아카데미")

# 바이그램 단어쌍
pair_bigram %>%
  filter(word1 == "아카데미")



###################################################################
# 예제5-4-2 : 네트워크 그래프
###################################################################

# 네트워크 그래프 데이터 만들기
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph()


# 네트워크 그래프 함수 만들기
word_network <- function(x) {
  ggraph(x, layout = "fr") +
    geom_edge_link(color = "gray50",
                   alpha = 0.5) +
    geom_node_point(color = "lightcoral",
                    size = 5) +
    geom_node_text(aes(label = name),
                   repel = T,
                   size = 5,
                   family = "nanumgothic") +
    theme_graph()
}

# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_bigram)


# 유의어 처리하고 네트워크 그래프 만들기
# 유의어 처리
bigram_seprated <- bigram_seprated %>%
  mutate(word1 = ifelse(str_detect(word1, "대단"), "대단", word1),
         word2 = ifelse(str_detect(word2, "대단"), "대단", word2),
         
         word1 = ifelse(str_detect(word1, "자랑"), "자랑", word1),
         word2 = ifelse(str_detect(word2, "자랑"), "자랑", word2),
         
         word1 = ifelse(str_detect(word1, "짝짝짝"), "짝짝짝", word1),
         word2 = ifelse(str_detect(word2, "짝짝짝"), "짝짝짝", word2)) %>%
  
  # 같은 단어 연속 제거
  filter(word1 != word2)

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()


# 참고 : 여러 유의어 한번에 처리하기
bigram_seprated_new <- bigram_seprated %>%
  mutate_at(vars("word1", "word2"),
            ~ case_when(
              str_detect(., "대단") ~ "대단",
              str_detect(., "자랑") ~ "자랑",
              str_detect(., "짝짝짝") ~ "짝짝짝",
              T ~ .))


# -------------------------------------------------------------------------
# 네트워크 그래프 데이터 만들기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),    # 중심성
         group = as.factor(group_infomap()))  # 커뮤니티


# -------------------------------------------------------------------------
# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +         # 레이아웃
  
  geom_edge_link(color = "gray50",            # 엣지 색깔
                 alpha = 0.5) +               # 엣지 명암
  
  geom_node_point(aes(size = centrality,      # 노드 크기
                      color = group),         # 노드 색깔
                  show.legend = F) +          # 범례 삭제
  scale_size(range = c(4, 8)) +               # 노드 크기 범위
  
  geom_node_text(aes(label = name),           # 텍스트 표시
                 repel = T,                   # 노드밖 표시
                 size = 5,                    # 텍스트 크기
                 family = "nanumgothic") +    # 폰트
  
  theme_graph()                               # 배경 삭제

