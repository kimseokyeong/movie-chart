# movie-chart

require(rvest)

require(lubridate)

library(dplyr)

library(ggplot2)

 

folder = 'c:/movie'

 

if(!dir.exists(folder)) dir.create(folder)  

 

setwd(folder)

 

date = Sys.Date()

 

h <- hour(Sys.time())

m <- minute(Sys.time())

now <- paste(date, h, m, sep='-')

now.folder <- paste(folder, now, sep='/')

 

if(!dir.exists(now.folder)) dir.create(now.folder)  # movie라는 폴더에 저장하도록 설정

setwd(now.folder)  

 

 

url ="https://movie.naver.com/movie/sdb/rank/rmovie.nhn?sel=cnt&tg=0&date=" # 네이버 영화 박스오피스순위 사이트

page = 20191101:20191130 # 20191101~20191130 박스오피스 순위 

 

pages = paste0(url , page , sep = '')

 

file.name = paste('page' , page , '.txt')

 

 

for (i in 1:length(pages)){
 
  file = read_html(pages[i])
 
 
 
  write_xml(file , file = file.name[i])
 
}

 

 

extra = function(url) {
 
  html = read_html(url)
 
  link = html %>% html_nodes("table") %>% html_nodes("td") %>% html_text()
 
  text = gsub("(\r)(\n)(\t)*", "", link) 
 
  text = gsub("\\d+" , "" , text)                                          
 
  text = gsub("\\s+" , "" , text) 
 
  text = text[text!=""]
  text = text[1:5]
 
 
 
  return(text)
 
}

 

top = lapply(pages,extra)  

 

data = as.data.frame(top)
data = as.matrix(data)
colnames(data) = c("20191101":"20191130") # data의 열이름을 박스오피스 순위날짜로 설정
rownames(data) = c("1":"5") # data의 행이름을 1위~5위로 설정

col = colnames(data)
row = rownames(data)
dark = data=="터미네이터:다크페이트" # 터미네이터:다크페이트의 순위만을 뽑아내기위해 true인 값을 추출



# 터미네이터:다크페이트 의 20191101~20191130 순위변동
ggplot(data = dark , aes(x=col , y=row)) + geom_point() + theme(axis.text.x = element_text(angle = 45 , hjust=1))




# 20191101~20191130 의 영화별 1위 횟수
first = data[1,]
first = table(first)

ggplot(data = first , aes(x=col , y=row)) + geom_bar(stat = "identity" ) + coord_flip() + ggtitle("20191101~20191130 1위횟수") + theme(title = element_text(color = "red" , size = 28)) 


