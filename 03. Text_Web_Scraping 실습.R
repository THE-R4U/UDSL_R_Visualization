library(stringr)

url <- "http://www.naver.com"

url_lines <- readLines(url, encoding = "UTF-8")
head(url_lines, 50)

inx <- str_detect(url_lines, '<span class=\"ah_k\">')
head(inx)

top_keyword_mat = NULL
while (1) {
  top_keyword = url_lines[inx]
  top_keyword = gsub("<.*?>", "", top_keyword)
  top_keyword = top_keyword[1:20]
  top_keyword_mat = rbind(top_keyword_mat, top_keyword)
  Sys.sleep(3600)
}


# 웹 스크랩핑 도구
library(httr)
library(rvest)

##
# GET : 주소창이 변함
# POST : 주소창이 변하지 않음

url <- "http://www.baseball-reference.com/leagues/MLB/2017.shtml"
url_get <- GET(url)  ## httr:GET
url_get$status_code

url_html = read_html(url_get)
tmp_baseball = url_html %>% html_nodes("div#div_teams_standard_batting") %>%
  html_nodes("table#teams_standard_batting") %>% html_table()

tmp_baseball <- tmp_baseball[[1]]
head(tmp_baseball)
dim(tmp_baseball)

##
# 2010년 부터 2017년까지 스크랩핑
##
year_vec <- c(2010:2017)
baseball_result_list = list()

for(i in 1:length(year_vec)) {
  url <- paste0("http://www.baseball-reference.com/leagues/MLB/", 
                year_vec[i], ".shtml")
  url_get <- GET(url)  ## httr:GET
  #url_get$status_code

  url_html = read_html(url_get)
  tmp_baseball = url_html %>% html_nodes("div#div_teams_standard_batting") %>%
    html_nodes("table#teams_standard_batting") %>% html_table()

  tmp_baseball <- tmp_baseball[[1]]
  baseball_result_list[[i]] = tmp_baseball
}


# 웹페이지에서 GET / POST 방식을 확인하는 방법
# F12(개발자 도구) 눌러서, Network 탭을 누른 다음 페이즐 바꿔본다..


url <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=144330&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false"

url_get = GET(url)
url_html = read_html(url_get)

url_html %>% html_nodes('div.score_reple p') %>% html_text()
url_html %>% html_nodes('div.score_reple p') %>% html_text()
