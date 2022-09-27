####################################################
# 예제1-1 : 데이터 입력하여 데이터 셋 생성 
####################################################

# input variables
id = c(1, 2, 3, 4, 5, 6, 7) # id = 1:7
region = c('서울', '부산', '대구', '인천', '광주', '대전', '울산')
pop = c(9.7, 3.4, 2.4, 2.9, 1.5, 1.5, 1.1)
pop

# create data set
pop_2020 = data.frame(id, region, pop)


####################################################
# 예제1-2 : csv 파일 읽어 데이터 셋 생성
####################################################

# Working directory 지정 : setwd("d:/TM") 또는 Session 메뉴 이용
# setwd("d:/TM")

# read file and create data set
SiDo_Pop = read.csv("SiDo_Pop.csv", header=T, fileEncoding = "CP949", encoding = "UTF-8")

head(SiDo_Pop)


####################################################
# 예제1-3 : 텍스트 데이터 읽기
####################################################

# 방법1
#install.packages("readr")  
library(readr)  # to use read_file(), read_csv()

raw_moon = read_file("speech_moon.txt")
head(raw_moon)

# 방법 2
raw_moon = readLines("speech_moon.txt", encoding="UTF-8")
head(raw_moon)


########################################################
# 예제1-4 : Data set 다루기 (특정한 행/열 이용, 새로운 data set 생성 등)
########################################################

# read file and create data set
SiDo_Pop = read.csv("SiDo_Pop.csv", header=T, fileEncoding = "CP949", encoding = "UTF-8")
head(SiDo_Pop)

# 데이터 셋의 행/열의 수
nr = nrow(SiDo_Pop)
nc = ncol(SiDo_Pop)

# 데이터(변수) 요약 정보
summary(SiDo_Pop)

### data set_name[행, 열]
# 특정한 변수 이용
SiDo_Pop[, 5] #total은 열의 5번째에 위치한 변수
SiDo_Pop$total # $ 기호 활용하면 편리

summary(SiDo_Pop$total)	#summary(SiDo_Pop[, 5])

# chain/pipe operator : 어떤 작업에서 여러 단계의 절차를 필요로 할 때, 
# 중간 결과를 생성한 후 그 결과를 후속 절차에서 받아서 사용하는 경우에 유용
library(dplyr)   # to use %>%, select(), as_tibble(), mutate(), [3장 이후] count(), filter(), ...    
# package for Data Manipulation

m = SiDo_Pop %>% summary()
m

# 특정한 관측치 이용
SiDo_Pop[SiDo_Pop$year==2000, ]	# == 는 같음을 나타내는 비교연산자
SiDo_Pop[SiDo_Pop$year!=2000, ]

#########################################
# data frame/set에서 일부 데이터로 구성된 새로운 data set 생성 방법

# year, region, total 변수로 구성된 새로운 data set
new_data = SiDo_Pop[, c(1, 2, 5)]
new_data = subset(SiDo_Pop, select=c(year, region, total))
new_data = SiDo_Pop %>% select(year, region, total)

# year, region, total 변수를 제거한 data set
new_data = SiDo_Pop[, -c(1, 2, 5)]
new_data = subset(SiDo_Pop, select=-c(year, region, total))

# 2010년 이후의 데이터로 구성된 새로운 data set
new_data = SiDo_Pop[SiDo_Pop$year>=2010, ]
new_data = subset(SiDo_Pop, year>=2010)

# 2010 이후 데이터, (year, region, total) 변수로 구성된 새로운 data set
new_data = subset(SiDo_Pop, year>=2010, select=c(year, region, total))
new_data = SiDo_Pop %>% filter(year>=2010) %>% select(year, region, total)

SiDo_Pop$male = as.numeric(SiDo_Pop$male)
SiDo_Pop %>% filter(year>=2010) %>% select(male) %>% colMeans

# 2000, 2010년의 area_tag=='M'인 데이터에서 year, region, area_tag, male, female 변수로 구성된 data set
new_data = subset(SiDo_Pop, year<=2010 & area_tag=='M', select=c(year, region, area_tag, male, female))
new_data = SiDo_Pop %>% filter(year<=2010 & area_tag=='M') %>% select(year, region, area_tag, male, female)


####################################################
# 예제1-5 : Loop and Condition statement 
####################################################
# 2020년 female 인구 합 계산
new_data = SiDo_Pop %>% filter(year==2020) 
n = nrow(new_data)

# 방법 1 : 반복문 이용
female_pop_2020 = 0

for(i in 1:n) {
  female_pop_2020 = female_pop_2020 + new_data[i, 'female']  #new_data[i, 7]
}

# 방법 2 : 함수 이용
female_pop_2020 = sum(new_data$female)


# SiDo_Pop 데이터 셋을 이용한 2020년 female 인구 합 계산
n = nrow(SiDo_Pop)

# 방법 1: 반복문과 조건문 같이 이용
female_pop_2020 = 0

for(i in 1:n) {
  
  if (SiDo_Pop[i, 'year']==2020) {
    female_pop_2020 = female_pop_2020 + SiDo_Pop[i, 'female']  
  } else {  }  
}

# 방법 2 : pipe operator와 함수 이용
female_pop_2020 = SiDo_Pop %>% filter(year==2020) %>% select(female) %>% sum


####################################################
# 예제2-1 : 기본 plot
####################################################

# 데이터 scale 조정
SiDo_Pop$total = round(SiDo_Pop$total/1000000, 1)
SiDo_Pop$male = round(SiDo_Pop$male/1000000, 1)
SiDo_Pop$female = round(SiDo_Pop$female/1000000, 1)

# 2020년 데이터
Pop_2020 = SiDo_Pop %>% filter(year==2020) 

# 가장 기본적인 함수: plot(x, y)
plot(Pop_2020$male, Pop_2020$female) 

### 자주 쓰는 함수
hist(Pop_2020$total)

barplot(table(Pop_2020$area_tag))
pie(table(cut(Pop_2020$total, breaks=4)))

# 전체 데이터 이용
SiDo_Pop$year = as.factor(SiDo_Pop$year)
pairs(SiDo_Pop[5:7], main = "Population", pch = 21, bg = c("red", "green3", "blue")[SiDo_Pop$year])
pairs(SiDo_Pop[5:7], main = "Population", pch = 21, col = c("red", "green3", "blue")[SiDo_Pop$year])

# 년도별로 점(point)의 색을 다르게
SiDo_Pop$Colour="red"
SiDo_Pop$Colour[SiDo_Pop$year==2010]="green3"
SiDo_Pop$Colour[SiDo_Pop$year==2020]="blue"

plot(SiDo_Pop$male, SiDo_Pop$female, main="Scatter plot", xlab="Male", ylab="Female",
     xlim=c(0, 10), ylim=c(0, 10), type="p", pch=20, cex=1, col=SiDo_Pop$Colour)


####################################################
# 예제2-2 : 기본 plot에 내용 추가
####################################################

# plot에 내용 추가 함수 활용
# 그래프에 text 및 화살표 추가
plot(SiDo_Pop$male, SiDo_Pop$female, main="Scatter plot", xlab="Male", ylab="Female",
     xlim=c(0, 10), ylim=c(0, 10), type="p", pch=20, cex=1, col=SiDo_Pop$Colour, font=3)
text(7, 8, "r = 0.95", font=3)

arrows(6.8, 2, 6.8, 6.5, length=0.2, angle=20)
text(6.8, 1.5, "경기도", col="red")

# 범례(legend) 작성
legend(8, 9, legend=c("2000", "2010", "2020"), col=c("red", "green3", "blue"), pch=20)


####################################################
# 예제2-3 : ggplot 함수 활용
####################################################

### Graph 1: Basic graph function
hist(SiDo_Pop$total, breaks=7, col="red")

### Graph 2: ggplot2 package
library(ggplot2)  # to use ggplot(), geom_histogram(), annotate(), ...

# histogram
ggplot(SiDo_Pop) +
  geom_histogram(aes(x=total), bins=7, fill="red") +
  annotate("text", x=13, y=3, col = "blue", label = "경기도?")

# scatter plot
SiDo_Pop$year = as.factor(SiDo_Pop$year)

ggplot(SiDo_Pop, aes(x=male, y=female, fill=year)) +
  geom_point(na.rm=TRUE, aes(colour=year)) +
  annotate("text", x=7.5, y=7.2, label="r = 0.95") 

