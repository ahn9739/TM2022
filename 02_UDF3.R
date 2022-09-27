# 필요한 패키지 load
library(KoNLP)      # to use useSejongDic(), extractNoun(), useNIADic(), ...  
library(wordcloud)  # to use wordcloud()

# KoNLP에 있는 세종사전을 사용하기 위한 함수
useSejongDic()


### (1) 데이터 
make_dataset <- function() {
  text<- "텍스트마이닝은 자연어 (Natural Language)로 구성된 비정형데이터 (unstructured data)에서 패턴 또는 관계를 추출하여 
          의미있는 정보를 찾아내는 기법으로, 컴퓨터가 사람들이 말하는 언어를 이해할 수 있는 자연어 처리 (Natural Language 
          Processing)에 기반으로 둔 기술이다. 트위터, 페이스북과 같은 소셜 미디어에서 생산되는 데이터는 비정형데이터이기 
          때문에 텍스트마이닝을 이용하여 분석할 수 있다.
          텍스트마이닝은 말 그대로 텍스트 형태의 비정형데이터에 마이닝 기법을 적용한 것이다. 즉, 텍스트에 나타나는 단어를 
          분해, 정제하고, 특정 단어의 출현빈도 등을 파악하여 단어들 간의 관계를 조사하는 기법이다. 
          데이터마이닝 (data mining)은 대규모 DB에 저장된 정형화된 데이터로부터 정보를 찾아내는 기법이라면 텍스트마이닝은 
          비정형화된 텍스트 문서에서 정보를 찾아내는 기법이라 할 수 있다.
          그림 2.1은  데이터마이닝 과 텍스트마이닝의 관계를 나타내는 그림이다."
  
  return (text)
}

### (2) 명사추출 및 사전처리
kword_pre = function(text) {
  nouns = extractNoun(text) #명사만 추출
  
  nouns <- nouns[nchar(nouns)>=2]   #명사 중에서 2글자 이상만 남기기
  
  nouns <- gsub("텍스트마이닝.*","텍스트마이닝", nouns)
  nouns <- gsub("데이터마이닝.*","데이터마이닝", nouns)
  
  return (nouns)
}

### (3) 워드 클라우드
wordcl = function(nouns){
  # ④ 데이터 분석
  wordFreq <-table(nouns)    #빈도 계산
  
  
  # 워드 클라우드
  pal <- brewer.pal(6, "Dark2")
  windowsFonts(malgun=windowsFont("맑은 고딕"))
  
  wordcloud(words=names(wordFreq), freq=wordFreq, colors=pal, min.freq=1, random.order=F, family="malgun")  
}

text = make_dataset()
nouns = kword_pre(text)
wordcl(nouns)