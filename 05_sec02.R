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
# 5.1 절


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


# 단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)
###################################################################


###################################################################
# 예제5-2-1 : 네트워크 그래프 데이터 만들기
###################################################################
#install.packages("tidygraph")
library(tidygraph)           #to use as_tbl_graph()

graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()    # 네트워크 그래프 데이터 생성

graph_comment


# 네트워크 그래프
install.packages("ggraph")
library(ggraph)

ggraph(graph_comment) +
  geom_edge_link() +                 # 엣지
  geom_node_point() +                # 노드
  geom_node_text(aes(label = name))  # 텍스트


# 그래프 다듬기 - 폰트 설정
install.packages("showtext")
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

# 그래프 다듬기
set.seed(1234)                              # 난수 고정
ggraph(graph_comment, layout = "fr") +      # 레이아웃
  
  geom_edge_link(color = "gray50",          # 엣지 색깔
                 alpha = 0.5) +             # 엣지 명암
  
  geom_node_point(color = "lightcoral",     # 노드 색깔
                  size = 5) +               # 노드 크기
  
  geom_node_text(aes(label = name),         # 텍스트 표시
                 repel = T,                 # 노드밖 표시
                 size = 5,                  # 텍스트 크기
                 family = "nanumgothic") +  # 폰트
  
  theme_graph()                             # 배경 삭제


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

# 함수 호출 예
set.seed(1234)
word_network(graph_comment)


# 유의어 처리하기
comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "봉준호", word), 
         word = ifelse(word == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))

# 단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)

# 네트워크 그래프 데이터 만들기
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_comment)



###################################################################
# 예제5-2-2 : 연결중심성과 커뮤니티 표현
###################################################################
# 연결중심성, 커뮤니티 변수 추가하기
set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),        # 연결 중심성
         group = as.factor(group_infomap()))      # 커뮤니티

graph_comment

# 연결중심성, 커뮤니티 표현하기
set.seed(1234)
ggraph(graph_comment, layout = "fr") +      # 레이아웃
  
  geom_edge_link(color = "gray50",          # 엣지 색깔
                 alpha = 0.5) +             # 엣지 명암
  
  geom_node_point(aes(size = centrality,    # 노드 크기
                      color = group),       # 노드 색깔
                  show.legend = F) +        # 범례 삭제
  scale_size(range = c(5, 15)) +            # 노드 크기 범위
  
  geom_node_text(aes(label = name),         # 텍스트 표시
                 repel = T,                 # 노드밖 표시
                 size = 5,                  # 텍스트 크기
                 family = "nanumgothic") +  # 폰트
  
  theme_graph()                             # 배경 삭제


# 주요 단어의 커뮤니티 살펴보기
graph_comment %>%
  filter(name == "봉준호")

# 같은 커뮤니티로 분류된 단어 살펴보기
graph_comment %>%
  filter(group == 4) %>%
  arrange(-centrality) %>%
  data.frame()


# 연결중심선이 높은 주요 단어 살펴보기 
graph_comment %>%
  arrange(-centrality)

# 2번 커뮤니티로 분류된 단어 : 봉준호 감독 수상 축하 내용
graph_comment %>%
  filter(group == 2) %>%
  arrange(-centrality) %>%
  data.frame()


# 주요단어가 사용된 원문 살펴보기
news_comment %>%
  filter(str_detect(reply, "봉준호") & str_detect(reply, "대박")) %>%
  select(reply)

news_comment %>%
  filter(str_detect(reply, "박근혜") & str_detect(reply, "블랙리스트")) %>%
  select(reply)

news_comment %>%
  filter(str_detect(reply, "기생충") & str_detect(reply, "조국")) %>%
  select(reply)