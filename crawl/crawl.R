pkgs <- c('dplyr','rvest','stringr','rlist','pbapply',
          'tidyr','progress','data.table','qdapRegex')
sapply(pkgs,require,character.only = TRUE)

setwd('D:/승훈/Data/공모전/포스코/data')

# 식단 및 칼로리 크롤링
# 일자를 다 크롤링하고, 없는 데이터를 지운다.
date <- read.csv('craa.csv')[-1]
date <- apply(date,2,as.character) %>% as.data.frame()
date[,c(2,3)] <- apply(date[,c(2,3)],2,function(x)
  ifelse(str_length(x) == 1,paste0(0,x),x))
date[,1] <- as.character(date[,1])
date <- unique(date)
row.names(date) <- 1:nrow(date)
date <- date[4430:nrow(date),]

# 리스트 만들고
# 리스트에 메뉴 크롤링결과 저장
res <- list()

for(i in 1:nrow(date)){
  cat('Take menu in date : ',date[i,1],date[i,2],date[i,3],'\n')
  url <- paste0('https://www.poswel.co.kr/fmenu/todaymenu.php?area_code=A1&nyear=',
                date[i,1],'&nmonth=',date[i,2],'&reqday=',date[i,3])
  a <- read_html(url)
  reada <- a %>%
           html_nodes('div.menu_list_icon02') %>%
           html_text() %>% str_remove_all(.,'\r|\n|\t|    ')
  readb <- a %>%
           html_nodes('div.list_menu_explain') %>%
           html_text() %>% str_remove_all(.,'\r|\n|\t|    ')
  readc <- a %>%
           html_nodes('div.list_comment_explain') %>%
           html_text() %>% str_remove_all(.,'\r|\n|\t|    ')
  dd <- rep(a %>% 
          html_nodes('div#today_menu_tit_text01') %>%
          html_text() %>% str_remove_all(.,'\r|\n|\t|    '),length(reada))
  res[[i]] <- cbind(dd,reada,readb,readc) %>% data.frame()
}

df <- rbindlist(res)
df <- separate(df,dd,sep = ' ',
               into = c('weekday','date'))
df <- separate(df,reada,sep = ' ',
               into = c('time','cate','kcal'))

# 도시락 없는 행 날리기
# 7월 7일부터.
df <- df[33:nrow(df),]

# 날짜정리
df$date <- str_remove_all(df$date,'-')

# 메뉴, 가격 정리
df$cate <- str_remove_all(df$cate,'[:punct:]')
df$price <- str_extract_all(df$cate,'[:digit:]') %>%
  str_remove_all(.,'c[(]|[)]|,|"| ') %>% as.numeric()
df$cate <- str_remove_all(df$cate,'[:digit:]|원')

# 칼로리
df$kcal <- str_remove_all(df$kcal,'[^[:digit:]]') %>% as.numeric()

# 도시락은 예측에 들어가지 않으므로
# 따로 변수를 만들어 활용할 수 있을 것.
dosirak <- df[df$time == '도시락',]
df <- df[df$time != '도시락',]

# crawl_data save
save(df,res,dosirak,file = 'crawl_save.RData')

