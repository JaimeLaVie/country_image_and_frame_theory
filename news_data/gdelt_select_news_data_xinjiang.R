# 本程序在已爬虫到的新闻数据中筛选所需数据，并保存，是gdelt_get_web_content.R的后续程序。
# 本程序筛选新疆棉花、立陶宛事件相关新闻数据
rm(list = ls())
Sys.setlocale("LC_ALL", "English") # 出现在某些网页内容中的中文引号会导致“EOF within quoted string”的warning，并导致text <- unlist(text) %>% as.character()这一步中所有输出结果变为NA。加上本句可防止这一问题。

library(stringr)

setwd("E:\\")
media_time <- "ap_20210816_0831"
news_address <- paste("E:\\GDELT analysis\\news", media_time, sep = '\\')
target_address_x <- paste(paste("E:\\GDELT analysis\\news\\selected\\xinjiang_cotton", media_time, sep = '\\'), "_selected", sep = '')
target_address_l <- paste(paste("E:\\GDELT analysis\\news\\selected\\lithuania_taiwan", media_time, sep = '\\'), "_selected", sep = '')
keywords_x_1 <- c('xinjiang', 'uyghur', 'uighur')
keywords_x_2 <- c('cotton', 'genocide', 'forced labor', 'forced labour')
keywords_l_1 <- c('lithuania') # 其实已经包括了立陶宛人、立陶宛的Lithuanian
keywords_l_2 <- c('taiwan')

news_files <- list.files(news_address)
length_files <- length(news_files)
length_keywords_x_1 <- length(keywords_x_1)
length_keywords_x_2 <- length(keywords_x_2)
length_keywords_l_1 <- length(keywords_l_1)
length_keywords_l_2 <- length(keywords_l_2)

for (num in 1:length_files) {
  print (news_files[num])
  if(str_detect(news_files[num], "_0_") == TRUE) next
  possibleError <- tryCatch(
    text <- read.table(paste(news_address, news_files[num], sep = "\\"), fill = TRUE),
    error=function(e) e) #{
    # cat ('Error! Address: ', news_files[num], '\n')})
  if(inherits(possibleError, "error")){
    # cat ("Show you I can work")
    cat ('Error! Address: ', paste(news_address, news_files[num], sep = "\\"), '\n')
    next
  }
  text <- unlist(text) %>% as.character()
  if(length(text) == 0){
    cat ('Error! Argument is of length zero, no read outcome! Address: ', news_files[num], '\n')
    next
  }
  text <- c(Reduce('paste', text[1:length(text)-1]), text[length(text)])  #将除了网址以外的内容合并
  save_flag <- "n"
  for (j in 1:length_keywords_x_1){
    if (str_detect(tolower(text[1]), keywords_x_1[j]) == TRUE){
      for (k in 1:length_keywords_x_2){
        if (str_detect(tolower(text[1]), keywords_x_2[k]) == TRUE){
          cat ('Xinjiang keywords found!')
          save_flag <- "x"
          # cat (keywords[j], '\n', news_files[num], '\n')
          length_text <- length(text)
          text[length_text + 1] <- paste('keyword name: ', keywords_x_1[j], ' and ', keywords_x_2[k], ' ')
        }
      }
    }
  }
  for (j in 1:length_keywords_l_1){
    if (str_detect(tolower(text[1]), keywords_l_1[j]) == TRUE){
      for (k in 1:length_keywords_l_2){
        if (str_detect(tolower(text[1]), keywords_l_2[k]) == TRUE){
          cat ('Lithuania keywords found!')
          save_flag <- "l"
          # cat (keywords[j], '\n', news_files[num], '\n')
          length_text <- length(text)
          text[length_text + 1] <- paste('keyword name: ', keywords_l_1[j], ' and ', keywords_l_2[k], ' ')
        }
      }
    }
  }
  if (save_flag == "x"){
    write.table(text, file = paste(target_address_x, news_files[num], sep = '\\'), row.names = FALSE, col.names = FALSE)
  }
  if (save_flag == "l"){
    write.table(text, file = paste(target_address_l, news_files[num], sep = '\\'), row.names = FALSE, col.names = FALSE)
  }
}

