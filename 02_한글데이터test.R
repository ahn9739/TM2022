####################################################
# 예제 : 한국어 활용을 위한 환경/패키지 세팅
####################################################


### 자바와 rJava 패키지, KoNLP 의존성 패키지 등 설치
install.packages("multilinguer")
library(multilinguer)
install_jdk()

install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"),
                 type = "binary")

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))



# 필요한 패키지 load
library(KoNLP)      # to use useSejongDic(), extractNoun(), useNIADic(), ...  
library(wordcloud)  # to use wordcloud()

# KoNLP에 있는 세종사전을 사용하기 위한 함수
useSejongDic()

# ①② 데이터 준비 및 코퍼스 구성 (데이터 준비 및 read)
text<- "텍스트마이닝은 자연어 (Natural Language)로 구성된 비정형데이터 (unstructured data)에서 패턴 또는 관계를 추출하여 의미있는 정보를 찾아내는 기법으로, 컴퓨터가 사람들이 말하는 언어를 이해할 수 있는 자연어 처리 (Natural Language Processing)에 기반으로 둔 기술이다. 트위터, 페이스북과 같은 소셜 미디어에서 생산되는 데이터는 비정형데이터이기 때문에 텍스트마이닝을 이용하여 분석할 수 있다.
 텍스트마이닝은 말 그대로 텍스트 형태의 비정형데이터에 마이닝 기법을 적용한 것이다. 즉, 텍스트에 나타나는 단어를 분해, 정제하고, 특정 단어의 출현빈도 등을 파악하여 단어들 간의 관계를 조사하는 기법이다. 
데이터마이닝 (data mining)은 대규모 DB에 저장된 정형화된 데이터로부터 정보를 찾아내는 기법이라면 텍스트마이닝은 비정형화된 텍스트 문서에서 정보를 찾아내는 기법이라 할 수 있다.
그림 2.1은  데이터마이닝 과 텍스트마이닝의 관계를 나타내는 그림이다."

# ③ 데이터 사전처리 및 형태소 분석
nouns = extractNoun(text) #명사만 추출

nouns <- nouns[nchar(nouns)>=2]   #명사 중에서 2글자 이상만 남기기

nouns <- gsub("텍스트마이닝.*","텍스트마이닝", nouns)
nouns <- gsub("데이터마이닝.*","데이터마이닝", nouns)

# ④ 데이터 분석
wordFreq <-table(nouns)    #빈도 계산
sort.wordFreq = sort(wordFreq, decreasing=TRUE)
sort.wordFreq = sort.wordFreq[1:10]

# 막대그래프
sort.wordFreq = sort(sort.wordFreq)
barplot(sort.wordFreq, horiz = TRUE, cex.names=0.6, las=1)

# 워드 클라우드
pal <- brewer.pal(6, "Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))

wordcloud(words=names(wordFreq), freq=wordFreq, colors=pal, min.freq=1, random.order=F, family="malgun")



################################################################################################
# 참고 
# - KoNLP 이용에 문제 있을때 / Fail to locate 'scala-library-2.11.8.jar'... 메세지가 나오면...
# (1) 'scala-library-2.11.8.jar' 화일을 ..../KoNLP/java 폴더에 복사하고
# (2) R Studio 종료 후 다시 시작하고 코드 실행 
###############################################