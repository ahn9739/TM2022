################################
# User-Defined Function
################################

# (1) f()             # parameter 없음
# (2) f(x)            # parameter 1개
# (3) f(x1, x2, ...)  # parameter 여러 개

# f(...)              # return 값 없음
# y = f(...)          # return 값 있음



### 예1
simple_calc <- function() {
  
  data = c(10, 20, 30, 40, 50)
  
  sum_data = 0
  
  # calculation
  for(i in 1:length(data)) {
    sum_data = sum_data + data[i]
  }
  
  print (sum_data)
}

simple_calc()


### 예2
simple_calc <- function() {
  
  data = c(10, 20, 30, 40, 50)
  
  sum_data = 0
  
  # calculation
  for(i in 1:length(data)) {
    sum_data = sum_data + data[i]
  }
  
  return (sum_data)
}

sum_data = simple_calc()
sum_data



### 예3
data = c(10, 20, 30, 40, 50)

simple_calc <- function(data) {
  
  sum_data = 0
  
  # calculation
  for(i in 1:length(data)) {
    sum_data = sum_data + data[i]
  }
  
  print (sum_data)
}

simple_calc(data)


### 예4
simple_calc <- function(data) {
  
  sum_data = 0
  
  # calculation
  for(i in 1:length(data)) {
    sum_data = sum_data + data[i]
  }
  
  return (sum_data)
}

sum_data = simple_calc(data)
sum_data



### 예5
data = c(10, 20, 30, 40, 50)
weight = c(0.4, 0.3, 0.1, 0.1, 0.1) 

simple_calc <- function(data, w) {

  w_sum = 0
  
  # calculation
  for(i in 1:length(data)) {
    w_sum = w_sum + data[i]*w[i]
  }
  
  print (w_sum)
}

simple_calc(data, weight)


### 예6
simple_calc <- function(data, w) {
  
  w_sum = 0
  
  # calculation
  for(i in 1:length(data)) {
    w_sum = w_sum + data[i]*w[i]
  }
  
  return (w_sum)
}

w_sum = simple_calc(data, weight)
w_sum

