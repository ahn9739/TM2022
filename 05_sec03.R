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


###################################################################
# 5.1~5.2 절


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

### 품사 추출하기
# 명사 추출하기
noun <- comment_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))

# 동사, 형용사 추출하기
pvpa <- comment_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>%         # "/pv", "/pa" 추출
  mutate(word = str_replace(word, "/.*$", "다"))  # "/"로 시작 문자를 "다"로 바꾸기


# 추출한 데이터 결합하기 : 품사 결합
comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)


# 유의어 처리하기
comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "봉준호", word), 
         word = ifelse(word == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))

###################################################################


###################################################################
# 예제5-3-1 : 파이 계수 및 막대그래프
###################################################################
#파이 계수
word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors

# 특정 단어와 관련성이 큰 단어
word_cors %>% 
  filter(item1 == "대한민국")

word_cors %>% 
  filter(item1 == "역사")


# 관심 단어별로 파이 계수가 큰 단어
# 관심 단어 목록 생성
target <- c("대한민국", "역사", "수상소감", "조국", "박근혜", "블랙리스트")

top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8)


# 막대그래프-------------------------------------------------------------------------
# 그래프 순서 정하기
top_cors$item1 <- factor(top_cors$item1, levels = target)

ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))


###################################################################
# 예제5-3-2 : 네트워크 그래프, 연결중심성 및 커뮤니티
###################################################################

library(tidygraph)           #to use as_tbl_graph()
library(ggraph)
library(showtext)

# word_cors에서 correlation이 0.15 이상인 단어쌍 추출
set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))


# 네트워크 그래프
set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,   # 엣지 명암
                     edge_width = correlation),  # 엣지 두께
                 show.legend = F) +              # 범례 삭제
  scale_edge_width(range = c(1, 4)) +            # 엣지 두께 범위
  
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  
  theme_graph()
