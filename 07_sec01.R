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

glimpse(raw_tada)

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

# 상위 단어 추출
frequency %>%
  head(30) %>%
  print(n = Inf)


# 불용어 제거하기
# 불용어 목록 생성
stopword_noun <- c("들이", "하면", "하게", "해서")

# 주요 단어 목록 만들기
top20_noun <- frequency %>%
  filter(!word %in% stopword_noun) %>%
  head(20)

top20_noun 


# 막대그래프
ggplot(top20_noun, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = comma(n, accuracy = 1)), hjust = -0.3) +  
  scale_y_continuous(limits = c(0, 3200)) +
  
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "언급 빈도 Top 20",
       x = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic", size = 12),
        plot.title = element_text(size = 14, face = "bold"),      # 제목 폰트
        plot.subtitle = element_text(size = 13))                  # 부제목 폰트


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

# 공감 여부별 댓글 수
word_sympathy %>%
  distinct(id, .keep_all = T) %>%
  count(sympathy, sort = T)


# 로그 오즈비를 구하기 위한 
# 단어 빈도 구하기
frequency_sympathy <- word_sympathy %>%
  count(sympathy, word) %>%              # 공감 여부 및 단어별 빈도
  filter(str_count(word) > 1 &           # 두 글자 이상 추출
           sympathy != "centrism")         # centrism 제거

# Wide form으로 변환하기
frequency_wide <- frequency_sympathy %>%
  pivot_wider(names_from = sympathy,
              values_from = n,
              values_fill = list(n = 0))

# 로그 오즈비 구하기
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((like    + 1) / (sum(like    + 1))) /
                                ((dislike + 1) / (sum(dislike + 1)))))

frequency_wide %>%
  arrange(-log_odds_ratio)


# 주요 단어를 비교하기 위해
# 주요 단어 추출
top10_odds <- frequency_wide %>%
  filter(like >= 20 | dislike >= 20) %>%
  group_by(sympathy = ifelse(log_odds_ratio > 0, "like", "dislike")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)

top10_odds %>%
  arrange(log_odds_ratio)


# 막대그래프
# 막대 색깔 목록 생성
col_sentiment <- c("#619CFF", "#F8766D")

# 막대 순서 지정
top10_odds$sympathy <- factor(top10_odds$sympathy,
                              levels = c("like", "dislike"))

ggplot(top10_odds, aes(x = reorder(word, log_odds_ratio),
                       y = log_odds_ratio,
                       fill = sympathy)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = col_sentiment,          # 막대 색깔
                    labels = c("공감", "비공감")) +  # 범례 순서
  
  labs(title = "타다 금지법 기사 댓글 주요 단어",
       subtitle = "공감 vs 비공감 로그 오즈비 Top 10",
       x = NULL, fill = NULL) +
  
  theme_minimal() +
  theme(text = element_text(family = "nanumgothic"),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))


###########################################################################
# 댓글 내용 살펴보기

# 주요 단어를 언급한 댓글 추출하기 (예: 조합)
tada %>%
  filter(str_detect(reply_raw, "조합")) %>%
  head(3) %>%
  pull(reply)


# 스타일 함수로 관심 단어만 눈에 띄게 출력하기
# 스타일 함수 만들기
library(crayon)
font <- combine_styles(make_style("ivory"),
                       make_style("deeppink", bg = TRUE),
                       make_style("bold"))
font

font("폰트를 적용해 출력") %>% cat()


# 관심 단어 설정
keyword <- "조합"

# 댓글 추출해 스타일 적용
tada %>%
  filter(str_detect(reply_raw, keyword)) %>%
  head(3) %>%
  mutate(reply = paste0(str_replace_all(reply,
                                        keyword,
                                        font(keyword)))) %>%  # 스타일 적용
  pull(reply) %>%                                             # reply 추출
  cat(sep = "\n\n")                                           # 줄바꿈 출력


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


# 함수 사용 예
tada %>% find_word(x = reply_raw, keyword = "조합", n = 2)
tada %>% find_word(reply_raw, "조합", 2)


# 공감/비공감 댓글 원문 추출하기
# tada와 word_sympathy 결합하고 중복 댓글 제거하기
tada <- tada %>%
  left_join(word_sympathy %>%
              distinct(id, .keep_all = T) %>%  # 중복 댓글 제거
              select(id, sympathy, diff),      # 주요 변수 추출
            by = "id")


# 공감 댓글 추출
reply_like <- tada %>%
  filter(sympathy == "like") %>%     # like 추출
  arrange(-diff)                     # 공감 높은순 정렬

# 비공감 댓글 추출
reply_dislike <- tada %>%
  filter(sympathy == "dislike") %>%  # dislike 추출
  arrange(diff)                      # 비공감 높은순 정렬


# 겅감 댓글 중 주요 단어가 언급된 댓글
# 조합
reply_like %>% find_word(x = reply_raw, keyword = "조합", n = 10)
# 소비자
reply_like %>% find_word(x = reply_raw, keyword = "소비자", n = 10)
# 동남아
reply_like %>% find_word(x = reply_raw, keyword = "동남아", n = 10)
# 렌트카
reply_dislike %>% find_word(x = reply_raw, keyword = "렌트카", n = 10)


# 비공감 댓글 내용 살펴보기
# "한국당" 언급 댓글 제거 후 "한국" 언급한 댓글 추출
reply_dislike %>%
  filter(!str_detect(reply, "한국당")) %>%
  find_word(x = reply, keyword = "한국", n = 10)


# -------------------------------------------------------------------------
# 댓글
reply_dislike %>% find_word(x = reply, keyword = "댓글", n = 10)


