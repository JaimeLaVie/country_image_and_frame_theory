# 本程序实现美联社爬虫，是gdelt_select_news_data.R的前续程序。
rm(list = ls())

library(stringr)
library(rvest)

data_address <- "gdelt_data/"
target_address <- "news/ap/"
gdelt_files <- list.files(data_address)
length_files <- length(gdelt_files)
title_length_limit <- 150

for (num in 1:length_files){
  if (str_detect(gdelt_files[num], ".csv$") == TRUE){
    print (str_sub(gdelt_files[num], 7, 14))
    csv_file <- read.csv(paste(data_address, gdelt_files[num], sep = ""))
    length_csv_file <- length(csv_file$MonthYear)
    print (length_csv_file)
    url_complete <- ''
    for (i in 1:length_csv_file){
      # 显示进度
      if (i %% 200 == 0){
        cat (str_sub(gdelt_files[num], 7, 14), ": ", round(i/length_csv_file, 2)*100, '%', '\n')
      }
      # other_website <- 1
      url <- csv_file[i, 'SOURCEURL'] %>% as.character()
      # url <- 'https://www.bbc.com/news/uk-northern-ireland-foyle-west-50328767'
      # 选出AP新闻并剔除明显无关内容
      if (is.na(url) == FALSE){  #确保url不为空
        if (str_detect(url,'/apnews.com') == TRUE && str_detect(url,'/press-release/') == FALSE){ # && str_detect(url,'mediacentre/') == FALSE && str_detect(url,'/sport/') == FALSE && str_detect(url,'/future/') == FALSE && str_detect(url,'/culture/') == FALSE && str_detect(url,'/programmes/') == FALSE && str_detect(url,'/cbbc/') == FALSE){
          if (str_detect(url_complete, url) == FALSE){           # 防止重复收集同一个网页的内容
            url_complete <- paste(url_complete, url, sep = ' ')
            print ('Find one!')
            print (i)
            # 避免网址不存在，出现404错误
            possibleError <- tryCatch(
              web <- read_html(url),
              error=function(e) e) #{
              # print('HTTP Error 404, go next.')})
            if(inherits(possibleError, "error")){
              cat ('HTTP Error 404, go next.\n')
              next
            }
            # web <- read_html(url)
            # 前两种网页很可能已经失效，只有第三种及以下才有效
            # 第一种网页，如https://apnews.com/article/72ef539bb781285d8b7efe8325796e5a：
            title <- web %>% html_nodes('h1.Component-h1-0-2-60') %>% html_text()
            if (length(title) != 0){
              flag <- 1
              title <- title[1]     # 以免出现数行的title导致后续无法存储
              title <- substr(title, 1, title_length_limit) # 以免名称太长程序打不开txt文件
              title <- gsub("[^[:alnum:] ]", "", title)
              # 命名规则：日期_原文件中的行数_网页类别_新闻标题.txt
              title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), title, sep = '_'), sep = '_'), sep = '_')
              text <- web %>% html_nodes('div.Article p') %>% html_text()
            } else {
            # 第二种网页，如https://apnews.com/article/97eca13a37dc39925694d439f649a348：
              title <- web %>% html_nodes('div.headline-0-2-56') %>% html_text()
              if (length(title) != 0){
                flag <- 2
                title <- title[1]
                title <- substr(title, 1, title_length_limit)
                title <- gsub("[^[:alnum:] ]", "", title)
                title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), title, sep = '_'), sep = '_'), sep = '_')
                text <- web %>% html_nodes('p.Component-root-0-2-73') %>% html_text()
              } else {
            # 前两种网页很可能已经失效，只有以下才有效
            # 第三种网页，如https://apnews.com/article/rhode-island-coronavirus-pandemic-medical-schools-providence-education-2ecec7022b395fa076fb95f3239be51d：
                title <- web %>% html_nodes('h1.Component-heading-0-2-61') %>% html_text()
                if (length(title) != 0){
                  flag <- 3
                  title <- title[1]
                  title <- substr(title, 1, title_length_limit)
                  title <- gsub("[^[:alnum:] ]", "", title)
                  title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), title, sep = '_'), sep = '_'), sep = '_')
                  text <- web %>% html_nodes('p.Component-root-0-2-76') %>% html_text()
                  # 对部分网址而言上述text获取方法不正确
                  if (length(text) == 0){
                    text <- web %>% html_nodes('p.Component-root-0-2-87') %>% html_text()
                  }
                } else {
            # 第四种网页，如https://apnews.com/article/turkey-coronavirus-pandemic-migration-only-on-ap-greece-75f06c4a3c80f2fc1c64810083d6c0da：
                  title <- web %>% html_nodes('h1.headline-0-2-57') %>% html_text()
                  if (length(title) != 0){
                    flag <- 4
                    title <- title[1]
                    title <- substr(title, 1, title_length_limit)
                    title <- gsub("[^[:alnum:] ]", "", title)
                    title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), title, sep = '_'), sep = '_'), sep = '_')
                    text <- web %>% html_nodes('p.Component-root-0-2-74') %>% html_text()
                  } else {
            # 前四种网页很可能已经失效，只有以下才有效
            # 第五种网页，如https://apnews.com/article/health-coronavirus-pandemic-fe012572d7f38ff5e322f5926394d3ee：
                    title <- web %>% html_nodes('h1.Component-heading-0-2-52') %>% html_text()
                    if (length(title) != 0){
                      flag <- 5
                      title <- title[1]
                      title <- substr(title, 1, title_length_limit)
                      title <- gsub("[^[:alnum:] ]", "", title)
                      title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), title, sep = '_'), sep = '_'), sep = '_')
                      text <- web %>% html_nodes('p.Component-root-0-2-78') %>% html_text()
                      # 对部分网址而言上述text获取方法不正确
                      if (length(text) == 0){
                        text <- web %>% html_nodes('p.Component-root-0-2-67') %>% html_text()
                        if (length(text) == 0){
                          text <- web %>% html_nodes('p.Component-p-0-2-69') %>% html_text()
                        }
                      }
                     } else {
            # 第六种网页，如https://apnews.com/article/business-pierre-f45360a9accd413693c882891aa43831：
                       title <- web %>% html_nodes('h1.Component-heading-0-2-63') %>% html_text()
                       if (length(title) != 0){
                         flag <- 6
                         title <- title[1]
                         title <- substr(title, 1, title_length_limit)
                         title <- gsub("[^[:alnum:] ]", "", title)
                         title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), title, sep = '_'), sep = '_'), sep = '_')
                         text <- web %>% html_nodes('p.Component-root-0-2-78') %>% html_text()
                         if (length(text) == 0){
                           text <- web %>% html_nodes('p.Component-root-0-2-89') %>% html_text()
                           if (length(text) == 0){
                             text <- web %>% html_nodes('p.Component-root-0-2-67') %>% html_text()
                             if (length(text) == 0){
                               text <- web %>% html_nodes('p.Component-p-0-2-69') %>% html_text()
                             }
                           }
                         }
                       } else {
            # 第七种网页，如https://apnews.com/article/europe-health-coronavirus-pandemic-philanthropy-united-nations-181620f70b939bac42e942e1b7cc1925：
                         title <- web %>% html_nodes('h1.headline-0-2-48') %>% html_text()
                         if (length(title) != 0){
                           flag <- 7
                           title <- title[1]
                           title <- substr(title, 1, title_length_limit)
                           title <- gsub("[^[:alnum:] ]", "", title)
                           title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), title, sep = '_'), sep = '_'), sep = '_')
                           text <- web %>% html_nodes('p.Component-root-0-2-65') %>% html_text()
                           if (length(text) == 0){
                             text <- web %>% html_nodes('p.paragraph-0-2-31') %>% html_text()
                           }
                         } else {
            # 其它网页，记录下来
                           flag <- 0
                           title <- paste(str_sub(gdelt_files[num], 7, 14), paste(as.character(i), paste(as.character(flag), 'other_website', sep = '_'), sep = '_'), sep = '_')
                           text <- ''
                           }
                         }
                       }
                     }
                   }
                 }
               }
            #   }
            # }
            # # title <- gsub('/', '', title)
            # # title <- gsub('£', '', title)
            # # title <- gsub("[^[:alnum:] ]", "", title)
            length_text <- length(text)
            text[length_text + 1] <- url
            
            write.table (text, file = paste(target_address, paste(title, '.txt', sep = ''), sep = ''), row.names = FALSE, col.names = FALSE)
            cat ('Web type: ', flag, '\n')
          }
        }
      }
    }
  }
}

cat ('Finished!')