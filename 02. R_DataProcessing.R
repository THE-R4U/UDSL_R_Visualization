require(httr)
require(rvest)

finace_data = NULL
for(i in 1:20) {
  url <- paste0("https://finance.naver.com/item/sise_day.nhn?code=044340&page=", i)
  url_get <- GET(url)
  url_html <- read_html(url_get)
  url_html
  tmp_table_list <- url_html %>% html_nodes('table.type2') %>% html_table()
  tmp_table = tmp_table_list[[1]]
  finace_data = rbind(finace_data, tmp_table)
  Sys.sleep(3)
  cat(i, 'page completed\n')
}

head(finace_data)

write.csv(finace_data, "finance_data.csv")

tt = read.csv("finance_data.csv")
head(tt)


#####################
# Data Preprocessing
# 열을 선택
# 행을 선택
# 새로운 열을 생성
# Group 별로 계산
# 정렬
#####################

surveys = read.csv("surveys.csv", stringsAsFactors = FALSE)

### plot, species, wgt 뽑아 오겠다 (1, 3번이 좀더 빠르다)
# 1. 열 이름을 그대로 작성
surveys[, c("plot", "species", "wgt")]
surveys[c("plot", "species", "wgt")]

# 2. 해당 열의 숫자를 입력
match(c("plot", "species", "wgt"), colnames(surveys))
surveys[, c(5, 6, 8)]
surveys[, match(c("plot", "species", "wgt"), colnames(surveys))]

# 3. 열을 T, F루 인덱싱
colnames(surveys) %in% c("plot", "species", "wgt")
surveys[, colnames(surveys) %in% c("plot", "species", "wgt")]



####### 행을 선택하는 방법

head(surveys)
surveys$year == 1995
sum(surveys$year == 1995)

head(surveys[surveys$year == 1995, ])

## wgt 열이 5보다 작은 wgt < 5 인 행을 뽑아오기
surveys[surveys$wgt < 5, ]
surveys$wgt < 5

# 위치 함수 which 사용
surveys[which(surveys$wgt < 5), ]

which(c(TRUE, FALSE, TRUE, NA))

## wgt 열이 5보다 작으며 species, sex, wgt 만 가져와라
surveys[which(surveys$wgt < 5), c("species", "sex", "wgt")]
surveys[!is.na(surveys$wgt), ] # wgt 가 NA면 제외

######## 새로운 열을 생성
surveys_ex = surveys
surveys_ex$wgt_g = surveys$wgt * 1000
tail(surveys_ex, 20)

####### Index 별로 계산
head(surveys)

sex_uniq = unique(surveys$sex)
sex_uniq

mean(surveys[surveys$sex == sex_uniq[1], "wgt"], na.rm = TRUE)
mean(surveys[surveys$sex == sex_uniq[2], "wgt"], na.rm = TRUE)
mean(surveys[surveys$sex == sex_uniq[3], "wgt"], na.rm = TRUE)

###
tapply(surveys$wgt, surveys$sex, mean, na.rm = T)
by(surveys$wgt, surveys$sex, mean, na.rm = T)
aggregate(formula = wgt ~ sex, data = surveys, FUN = mean, na.rm = T)
aggregate(formula = wgt ~ sex + month, data = surveys, FUN = mean, na.rm = T)


####### 정렬
head(surveys_ex, 50)
surveys_ex[order(surveys_ex$plot), ]
head(surveys_ex[order(surveys_ex$plot, decreasing = T), ])

#### month 내림차순을 하고 plot 으로 오림차순으로 정렬
surveys_ex = surveys_ex[order(surveys_ex)]


data(iris)
head(iris)

#### Sepal.Length 를 내림차순으로 정렬, Sepeal, Width를 오림순으로 정렬
iris_ex = iris
iris_ex = iris_ex[order(iris_ex$Sepal.Width), ]
iris_ex = iris_ex[order(iris_ex$Sepal.Length, decreasing = T), ]
head(iris_ex, 20)

######
###### dplyr package 사용
######

## 컬럼을 뽑는 함수 : select()

library(dplyr)
head(surveys)

select(.data = surveys, species, sex, wgt)
head(select(.data = surveys, species, sex, wgt))

surveys %>% select(species, sex, wgt) %>% head()

c_name = c("plot", "species", "wgt")
surveys %>% select(c_name) %>% head()
surveys %>% select(one_of(c_name)) %>% head()

c_name = c("plot", "species", "wgt", "shy")
surveys %>% select(c_name) %>% head() # 컬럼 이름이 없으면 오류
surveys %>% select(one_of(c_name)) %>% head()  # shy 컬럼은 없지만 있는것이라도 보여줌

# 특정 컬럼 제외
c_name = c("plot", "species", "wgt")
surveys %>% select(-c_name) %>% head()

# 컬럼 중 S로 시작하는 컬럼만 가져온다
surveys %>% select(starts_with("s")) %>% head()  

head(iris)
iris %>% select(starts_with('Sepal')) %>% head()
iris %>% select(ends_with("Length")) %>% head()

# : 연산자를 사용하여 컬럼을 선택해서 가져올 수 있다
head(surveys)
surveys %>% select(record_id:species) %>% head()


## 행을 뽑는 함수 : filter()

surveys %>% filter(year == 1995) %>% head()
surveys %>% filter(year == 1995, wgt < 30) %>% head()  # & 연산자는 , 와 같다
surveys %>% filter(year == 1995 & wgt < 30) %>% head()  # & 연산자는 , 와 같다

surveys %>% filter(year == 1995 | wgt < 30) %>% head()

surveys %>% filter(wgt >= 30 & wgt <= 50) %>% head()
surveys %>% filter(between(wgt,30, 50)) %>% head()


surveys %>% select(species:wgt) %>% filter(wgt < 5) %>% head()

# dplyr에서는 NA 값은 무시하고 계산한다
surveys %>% filter(wgt < 5) %>% 
  select(species:wgt) %>% head()


## 새로운 열을 추가하는 방법 : mutate()
# 굳이 mutate를 쓰는 이유는 추가 열을 여러개 만들 수 있다

surveys %>% mutate(wgt_g = wgt * 1000) %>% tail()

head(iris)
# Sepal.area = Sepal.Length * Sepal.Width
# Petal.area = Petal.Length * Petal.Width

iris %>% mutate(Sepal.area = Sepal.Length * Sepal.Width,
                Petal.area = Petal.Length * Petal.Width) %>% head()

surveys %>% group_by(sex) %>% summarise(mean(wgt, na.rm = T))

# tibble 형식은 DataFrame 형태로 변형해서 사용해야 오류가 적다..


# table 형태는 dplyr의 group_by 함수 형태로 쓸 수 있다
table(surveys$sex)
surveys %>% group_by(sex) %>% summarise(n()) 
surveys %>% group_by(sex) %>% tally() 


## 정렬하는 방법
## arrange() 함수 사용

iris %>% arrange(Sepal.Length) %>% head() # 오름차순 정렬
iris %>% arrange(desc(Sepal.Length)) %>% head() # 오름차순 정렬

# Sepal.Length 내림차순 정렬, Speal.Width 오름차순 정렬
iris %>% arrange(desc(Sepal.Length), Sepal.Width) %>% head()




# 데이터는 크게 2가지 포맷이 있다
# wide format vs. long format
# 보기에는 wide 포맷이 보기 편하나 (deep learning 등에 사용)
# 시각화를 할때는 long 포맷을 쓸때가 많다
# 변환 함수는 reshape2 패키지를 사용한다

require(reshape2)

data(airquality)
head(airquality)

# Column 이름이 대소문자로 섞여 있으면 소문자로 통일해 주자

colnames(airquality) = tolower(colnames(airquality))
head(airquality)

aq_melted = melt(data = airquality)
head(aq_melted)

# Melt 시킬때도 좀 보기 좋게 할 수 있다

aq_melted = melt(data = airquality, 
                 id.vars = c("month", "day")) # month, day는 melt 시키지 말라
aq_melted

aq_melted = melt(data = airquality,
                 id.vars = c('month', 'day'),
                 variable.name = 'climate_variable',
                 value.name = 'climate_value')
head(aq_melted)

# Melted 된걸 다시 돌려놓기 (왼쪽은 녹여지지 않는 컬럼, 오른쪽에는 해당 컬럼의 이름)
aq_casted = dcast(data = aq_melted, 
                  formula = month + day ~ climate_variable,
                  value.var = 'climate_value')
head(aq_casted)


## Finance 데이터
finance_data = finace_data
head(finance_data)

# finance_data에서 종가열의 ','를 제거하고
# 과거에서부터 오늘까지의 종가를 line plot으로 시각화 

library(stringr)

finance_data$종가 = str_replace_all(finance_data$종가, ",", "")
head(finace_data)

head(finance_data$종가)


ggplot(data = finace_data, aes(x = 날짜, y = 종가)) + 
  geom_line()

finace_data = finace_data[complete.cases(finace_data), ]
sub_data = finace_data %>% select(date = 날짜, price = 종가) %>% 
  mutate(price = gsub(',', '', price))

head(sub_data)
plot(sub_data$price, type = 'l')
plot(sub_data$price[nrow(sub_data):1], type = 'l')

