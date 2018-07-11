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

# 2. 해당 열의 숛자를 입력
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



###### dplyr package 사용
