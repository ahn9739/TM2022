####################################################
# 예제3-1-1 : 사전처리 및 data set 구성
####################################################

# 필요한 패키지 load
library(KoNLP)      # to use useSejongDic(), extractNoun(), useNIADic()  
library(wordcloud)  # to use wordcloud()
library(dplyr)      # to use %>%, select(), as_tibble(), mutate(), count(), filter(), arrange(), ...   
                    # package for Data Manipulation
library(stringr)    # to use str_replace_all(), str_squish()
                    # stringr 패키지에 있는 함수를 이용하여 사전처리


raw_moon = readLines("speech_moon.txt", encoding="UTF-8")
head(raw_moon)

# 불필요한 문자(특수문자, 한자, 공백 등) 제거 : str_replace_all()
# 파라미터명 입력 : str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")
# 파라미터명 생략 : str_replace_all(txt, "[^가-힣]", " ")
moon <- raw_moon %>% str_replace_all("[^가-힣]", " ")
head(moon)

# 연속된 공백 하나만 남기고 제거 : str_squish()
moon <- moon %>% str_squish()
head(moon)

# 문자열 벡터 형태의 데이터를 tibble 구조로 바꾸기
# tibble은 simple data frame이라고 생각하면 됨. 데이터를 간략하게 표현할 수 있고,
# 이 구조를 이용하면 대용량 데이터 세트를 다루는데 용이함
moon <- as_tibble(moon)
moon


# 위의 과정을 한꺼번에 처리하기
moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ") %>%  # 한글만 남기기
  str_squish() %>%                      # 연속된 공백 제거
  as_tibble()                           # tibble로 변환


####################################################
# 예제3-1-2 : 토큰화 하기
####################################################
# 토큰화 : 전처리 작업 후 텍스트를 분석목적에 따라 토큰(단락, 문장, 단어 등)으로 나누는 작업 

library(tidytext)   # to use unnest_tokens(), cast_dtm()   
     # 텍스트 포맷의 데이터를 여러 토큰(token) 단위로 쪼개서 분석하는데 필요한 패키지

word_space <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")
word_space


#############################################################################
# 참고 : 여기부터 ~~
library(readr)  # to use read_file(), read_csv()

raw_moon = read_file("speech_moon.txt")
head(raw_moon)

raw_moon = read_file("speech_moon.txt")
head(raw_moon)

moon <- raw_moon %>% str_replace_all("[^가-힣]", " ")
head(moon)

# 연속된 공백 하나만 남기고 제거 : str_squish()
moon <- moon %>% str_squish()
head(moon)

# 문자열 벡터 형태의 데이터를 tibble 구조로 바꾸기
# tibble은 simple data frame이라고 생각하면 됨. 데이터를 간략하게 표현할 수 있고,
# 이 구조를 이용하면 대용량 데이터 세트를 다루는데 용이함
moon <- as_tibble(moon)
moon

#moon$value


# 위의 과정을 한꺼번에 처리하기
moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ") %>%  # 한글만 남기기
  str_squish() %>%                      # 연속된 공백 제거
  as_tibble()     

### 토큰화하기
word_space <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")
word_space

# 참고 : 여기까지
#############################################################################


####################################################
# 예제3-1-3 : 단어 빈도 분석
####################################################

# 자주 사용된 단어를 살펴보면 무엇을 강조했는지 알 수 있음
word_space <- word_space %>%
  count(word, sort = T)
word_space

# 두 글자 이상만 남기기
word_space <- word_space %>%
  filter(str_count(word) > 1)
word_space

# 위의 과정 한 번에 처리
word_space <- word_space %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1)


# 빈도가 높은 상위 20개 단어
top20 <- word_space %>%
  head(20)
top20


####################################################
# 예제3-1-4 : 단어 빈도 그래프로 표현하기
####################################################

# 막대그래프1
barplot(top20$n, horiz = TRUE, names.arg = top20$word, cex.names=0.6, las=1)

# 막대그래프2
library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +            # 막대 밖 빈도 표시
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",  # 그래프 제목
       x = NULL, y = NULL) +                           # 축 이름 삭제
  theme(title = element_text(size = 12))               # 제목 크기


# reorder(top20$word, top20$n)
# ggplot(top20, aes(x = word, y=n))


# 워드 클라우드
library(ggwordcloud)   # to use geom_text_wordcloud()  # ggplot()에서 워드 클라우드 만들도록 도와주는 패키지  

ggplot(word_space, 
       aes(label = word, 
           size = n, 
           col = n)) +                      # 빈도에 따라 색깔 표현
  geom_text_wordcloud(seed = 1234) +  
  scale_radius(limits = c(3, NA),           # 최소, 최대 단어 빈도
               range = c(3, 30)) +          # 최소, 최대 글자 크기
  scale_color_gradient(low = "#66aaf2",     # 최소 빈도 색깔
                       high = "#004EA1") +  # 최고 빈도 색깔
  theme_minimal()                           # 배경 없는 테마 적용
