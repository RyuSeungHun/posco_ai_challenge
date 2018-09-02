pkgs <- c('dplyr','readxl','caret','lubridate','qdapRegex',
          'ggplot2','randomForest','stringr','tidyr','xlsx',
          'neuralnet','tree','pbapply','arules','rlist','progress')
sapply(pkgs,require,character.only = TRUE)

wd <- 'D:/승훈/Data/공모전/포스코/data/new'
setwd(wd)

people <- read_xlsx('data.xlsx',skip = 1,sheet = 2,
                    col_names = c('a','date','time','consume'),
                    col_types = c('guess','text','text','numeric'))[-1]

df <- read_xlsx('data.xlsx',skip = 1,sheet = 3,
                col_names = c('a','date','time','menu'),
                col_types = c('guess','text','text','text'))[-1]

# 데이터 정리
people$time[people$time == '아침식사1'] <- '아침식사'
people$time[people$time == '점심식사1'] <- '점심식사'
people$time[people$time == '저녁식사1'] <- '저녁식사'
people$time[people$time == '점심식사2(양식)'] <- '점심식사2'

people <- people %>% group_by(date,time) %>%
  summarise(consume = sum(consume))

df <- df[(min(which(df$time == '점심식사2'))-4):nrow(df),]
snack <- df[df$time == '아침식사2',]
df <- df[df$time != '아침식사2',]
people <- people[(min(which(people$time == '점심식사2'))-3):nrow(people),]

# data join
data <- left_join(df,people,by = c('date' = 'date',
                                   'time' = 'time'))
remove(df,people)

data$time <- factor(data$time,
                    levels = c('아침식사','점심식사','점심식사2','저녁식사'))

# eda
# month
data$season <- ymd(data$date) %>%
  lubridate::month() %>% as.factor()

data %>% group_by(season) %>% 
  summarise(a = mean(consume,na.rm = TRUE)) %>%
  ggplot() + geom_bar(aes(factor(season),a),
                      stat = 'identity') +
  coord_cartesian(ylim = c(17,25))

# weekday or weekend
data$weekend <- ymd(data$date) %>%
  wday(label = TRUE)

data %>% group_by(weekend) %>%
  summarise(a = mean(consume,na.rm = TRUE)) %>%
  ggplot() + geom_bar(aes(factor(weekend),a),
                      stat = 'identity')

# if weekend 1, weekday0
data %>%
  mutate(dd = ifelse(weekend %in% c('토','일'),'weekend','weekday')) %>%
  group_by(dd) %>%
  summarise(a = mean(consume,na.rm = TRUE)) %>%
  ggplot() + geom_bar(aes(factor(dd),a),
                      stat = 'identity')

data$weekend <- ifelse(data$weekend %in% c('토','일'),'weekend','weekday') %>%
  factor(levels = c('weekday','weekend'))

# menu text
menu <- data.frame(aa = rm_round(data$menu[!is.na(data$consume)]),stringsAsFactors = FALSE)
repp <- data$consume[!is.na(data$consume)] %>% round()
menu <- lapply(1:length(repp),function(x)
  rep(menu[x,],repp[x]))
menu <- unlist(menu) %>% matrix(ncol = 1) %>%
  data.frame(aa = .,stringsAsFactors = FALSE)

exp <- c('단무지','누룽지','피클','더운야채','야채샐러드','무생채','귤','요구르트',
          '배추김치','두유','석박지','깍두기','우유','포기김치','쌀밥')
exp <- paste0(paste0(',',exp,collapse = '|'),'|',
              paste0(exp,',',collapse = '|'))
menu <- pbsapply(menu,str_remove_all,exp) %>%
  data.frame(.,stringsAsFactors = FALSE)

table(sapply(menu,str_count,','))
menu[which(str_count(menu$aa,',') == 0),]
menu <- separate(menu,aa,into = paste0('menu_',0:15),sep = ',')
menu <- pbapply(menu,2,function(x)
  str_replace_na(x) %>% str_remove_all(.,'NA|[:punct:]'))
colnames(menu) <- NULL
write.table(menu,'aprrr.csv',sep = ',',row.names = FALSE,
            col.names = FALSE,quote = FALSE)

aprr <- read.transactions('aprrr.csv',format = 'basket',
                          sep = ',',rm.duplicates = TRUE)
rule <- apriori(aprr,parameter=list(support = 0.001,
                                    confidence = 0.2))
plot(rule,method = 'graph')
rule_res <- inspect(rule)[,c(1,3)]
rule_res <- apply(rule_res,2,function(x)
  str_remove_all(x,'[{]|[}]') %>% 
  ifelse(endsWith(.,','),str_remove_all(.,','),.)) %>%
  data.frame()

rule_res <- separate(rule_res,lhs,sep = ',',
                     into = c('lhs_1','lhs_2'))
rule_res <- separate(rule_res,rhs,sep = ',',
                     into = c('rhs_1','rhs_2'))

rule_res <- apply(rule_res,2,function(x)
  str_replace_na(x,replacement = ','))

kk <- matrix(nrow = nrow(data),ncol = 25)
for(i in 1:nrow(rule_res)){
    kk[,i] <-
str_detect(data$menu,rule_res[i,1]) &
str_detect(data$menu,rule_res[i,2]) &
str_detect(data$menu,rule_res[i,3]) &
str_detect(data$menu,rule_res[i,4])
}

data$if_apriori <- as.factor(rowSums(kk))

# stock?
stock <- read.csv('005490.ks.csv')
stock$Date <- str_remove_all(stock$Date,'-')
table(stock$Date %in% data$date)
tem <- left_join(data[,c('date')],stock,by = c('date' = 'Date'))

pb <- progress_bar$new(total = length(which(is.na(tem$Open))))
for(i in which(is.na(tem$Open))){
  pb$tick()
  tem[i,2:7] <- tem[i-1,2:7]
}
data <- left_join(data,tem,by = c('date' = 'date'))

# weather bind
weather <- read_xlsx('weather.xlsx')[,c(2,3,4,6,7,13,16,20,35,36)]
weather[,1] <- sapply(weather[,1],str_remove_all,'-')
weather[,6][is.na(weather[,6])] <- 0
names(weather) <- c('date','avg_temp','min_temp','max_temp','max_temp_time',
                    'day_rain','max_wind_time','avg_wind','max_sun_time','max_sun')
table(data$date %in% weather$date)

# time to factor & na replace
weather[,c(5,7,9)] <- lapply(weather[,c(5,7,9)],function(x)
  ifelse(x %in% c(540:830,1150:1330,1730:2000),1,0) %>% as.factor())
apply(weather,2,function(x)table(is.na(x)))
weather[,c(2,8,10)] <- apply(weather[,c(2,8,10)],2,function(x)ifelse(is.na(x),0,x) %>%
                            as.vector()) %>% data.frame()

# join
data <- left_join(data,weather) %>% data.frame()

# formula
formu <- as.formula(paste0('consume ~ ',
                    paste0(names(data)[-c(1,3,4)],collapse = '+')))

# data split
ddd <- read_xlsx('pre.xlsx',skip = 2)
names(ddd) <- c('date','a','b','c','d')
ddd$date <- str_remove_all(ddd$date,'-')
test <- data[data$date %in% ddd$date,]
test <- unique(test)

set.seed(19961228)
train <- data[!is.na(data$consume),]
idx <- sample(1:nrow(train),0.8 * nrow(train))
train <- train[idx,]
valid <- train[-idx,]

# variable with tree?
tr <- tree(formu,data = train)
plot(tr)
text(tr)
 
pr_tree <- prune.tree(tr,k = 8)
plot(pr_tree)
text(pr_tree)

# linear regression
md_lm <- lm(formu,data = train)
pre_lm <- predict(md_lm,valid)
(rmse_lm <- RMSE(pre_lm,valid$consume))

md_step <- step(md_lm)
pre_step <- predict(md_step,valid)
(rmse_step <- RMSE(pre_step,valid$consume))

# h2o
library(h2o)
h2o.init(max_mem_size = '7G')

x = names(train)[-c(1,3,4)]
y = 'consume'

# h2o_random forest
# v1. all variable
h2o_rf <- h2o.randomForest(x = x,
                           y = y,
                           training_frame = as.h2o(train),
                           ntrees = 5000,
                           mtries = 3)
h2o_rf_pre <- h2o.predict(h2o_rf,newdata = as.h2o(valid))
h2o.performance(h2o_rf,newdata = as.h2o(valid))

# v2. variable with tree
xx = c('time','weekend','Adj.Close','Low','High')

h2o_rf_tr <- h2o.randomForest(x = xx,
                              y = y,
                              training_frame = as.h2o(train),
                              ntrees = 5000,
                              mtries = 3)
h2o.performance(h2o_rf_tr,newdata = as.h2o(valid))

# predict
aa <- h2o.predict(h2o_rf,newdata = as.h2o(test)) %>% as.data.frame()
result <- cbind(test[,c(1,2)],aa)
result <- spread(result,time,predict)
result[,2:5] <- apply(result[,2:5],2,round,1)
result[,1] <- with(result,paste0(str_sub(date,1,4),'-',str_sub(date,5,6),'-',str_sub(date,7,8)))
result <- result[,c(1,2,4,5,3)]
names(result) <- c('날짜','아침','점심(일반)','점심(양식)','저녁')

# write
write.xlsx(result,'제출결과_3차.xlsx',row.names = FALSE)
