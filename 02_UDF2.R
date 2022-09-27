##########################################
# User-Defined Function : 과제 0.1
##########################################

### (1) 데이터 셑 생성
make_dataset <- function() {

  region = c('전북', '전남', '경북', '경남', '제주', '강원', '충북', '충남')
  male = c(896874, 930615, 1329211, 1681423, 338609, 776505, 811591, 1082634)
  female = c(907230, 920934, 1310211, 1658793, 336026, 766335, 789246, 1038395)
  
  # create data set
  data_set = data.frame(region, male, female)
  
  return (data_set)
}

ds = make_dataset()


### (2) 평균, 표준편차, 중위수
# 데이터의 합 계산
sum_calc <- function(var) {

  n = length(var)
  sum = 0
  
  # calculation
  for(i in 1:n) {
    sum = sum + var[i]
  }
  
  return (sum)
}

#s = sum_calc(ds$female)

# 데이터의 평균 계산
aver_calc = function(var) {
  
  n = length(var)
  
  aver = sum_calc(var)/n
  
  return (aver)
}

aver = aver_calc(ds$female)
aver

mean(ds$female)


# 데이터의 제곱합 계산
ssum_calc <- function(var) {
  
  n = length(var)
  ssum = 0
  
  # calculation
  for(i in 1:n) {
    ssum = ssum + var[i] * var[i]
  }
  
  return (ssum)
}


# 데이터의 표준편차 계산
sd_calc = function(var) {
  
  n = length(var)
  
  aver = sum_calc(var)/n
  ssum = ssum_calc(var)
  
  sd = sqrt((ssum - n*aver*aver)/(n-1))
    
  return (sd)
}

s = sd_calc(ds$female)
s

sd(ds$female)


# 데이터의 중위수 계산 
med_calc = function(var){
  n = length(var)
  
  var = sort(var)
  
  if (n%%2 == 0){
    med = (var[n/2]+var[n/2 + 1]) / 2
  } else {
    med = var[(n+1)/2]
  }
  
  return (med)
}

m = med_calc(ds$female)


### (3) 히스토그램
make_hist <- function(var) {
  
  hist(var)
}

make_hist(ds$female)



### (4) 벡터의 거리 계산
neardist = function (data) {
  n = nrow(data)
  first_d = data[1, c(2,3)]

  d = 10^10
  
  for(i in 2:n) {
    x = data[i, c(2,3)]
    d1 = dist(rbind(first_d, x))
    
    if (d1 < d) { 
      reg = data[i, 1] 
      d = d1
    }
  }
  
  return (reg)
}

near_reg = neardist(ds)
near_reg

