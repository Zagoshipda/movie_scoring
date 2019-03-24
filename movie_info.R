library(dplyr)
library(rvest)
library(stringr)


uri = "http://www.kobis.or.kr/kobis/business/stat/boxs/findYearlyBoxOfficeList.do?loadEnd=0&searchType=search&sSearchYearFrom="
years = c('2004', "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
nations = c("K", "F")

info_full <- NULL
daily_full <- NULL

for(year in years){
  for(nation in nations){
    url <- paste0(uri, year, "&sMultiMovieYn=&sRepNationCd=", nation, "&sWideAreaCd=")
    
    url %>%
      readLines(encoding="UTF-8") %>%
      .[str_detect(., "mstView")] -> tmp
    
    tmp %>%
      str_split("title=\"") %>%
      unlist %>%
      .[!str_detect(., "onclick")] %>%
      str_sub(1,-3) -> title
    
    tmp %>%
      str_sub(62, 69) -> code
    
    tmp1 = "http://www.kobis.or.kr/kobis/business/mast/mvie/searchMovieDtlXls.do?code="
    tmp2 = "&sType=box"
    code <- paste0(tmp1, code, tmp2)
    
    url %>%
      readLines(encoding = "UTF-8") %>%
      .[which(str_detect(., "td_openDt"))+1] %>%
      gsub("\t", "", .) -> opendt
    
    url %>%
      readLines(encoding = "UTF-8") %>%
      .[which(str_detect(., "td_audiAcc"))+1] %>%
      gsub("\t", "", .) %>%
      gsub(",",  "", .) %>%
      as.numeric -> score
    
    info <- data.frame(title, opendt, code, score, stringsAsFactors = F)
    
    rm(tmp, tmp1, tmp2, url, title, opendt, code)
    
    # 개별영화 일자별 관람객수 수집
    
    info <- cbind.data.frame(info, "100만"= NA, "200만"= NA, "300만"= NA, "400만"= NA, "500만"= NA, "600만"= NA, "700만"= NA, "800만"= NA,
                             "900만"= NA, "1000만"= NA, "1100만"= NA, "1200만"= NA, "1300만"= NA, "1400만"= NA, "1500만"= NA, "1600만"= NA,
                             "1700만"= NA)
    daily_total <- NULL
    for(index in 1:nrow(info)){
      info[index, 3] %>%
        read_html %>%
        html_table %>%
        .[[1]] %>%
        select("날짜", "스크린수", "관객수", "누적관객수") -> daily
      
      cbind.data.frame("영화" = info[index, 1], daily, "개봉일" = as.numeric(daily$날짜 == info[index, 2])) -> daily
      
      daily[,4] <- as.numeric(gsub(",", "", daily[,4]))
      daily[,5] <- as.numeric(gsub(",", "", daily[,5]))
      
      for(i in 2:nrow(daily)){
        if(daily[i-1, 6] > 0){
          daily[i, 6] <- daily[i-1, 6] + 1
        }
      }
      
      daily_total <- rbind(daily_total, daily)
      
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 999999){
          info[index, 5] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 1999999){
          info[index, 6] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 2999999){
          info[index, 7] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 3999999){
          info[index, 8] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 4999999){
          info[index, 9] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 5999999){
          info[index, 10] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 6999999){
          info[index, 11] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 7999999){
          info[index, 12] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 8999999){
          info[index, 13] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 9999999){
          info[index, 14] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 10999999){
          info[index, 15] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 11999999){
          info[index, 16] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 12999999){
          info[index, 17] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 13999999){
          info[index, 18] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 14999999){
          info[index, 19] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 15999999){
          info[index, 20] <- daily[i, 6]
          break
        }
      }
      for(i in 1:nrow(daily)){
        if(daily[i, 5] > 16999999){
          info[index, 21] <- daily[i, 6]
          break
        }
      }
      cat("\n", info[index, 1], index)
    }
    
    daily_full <- rbind(daily_full, daily_total)
    info_full <- rbind(info_full, info)
    cat("\n", year, "년도", nation, "영화 수집 완료")
  }
}

info_full <- cbind('연도' = rep(years, each = 50), info_full)
daily_full <- unique(daily_full)

write.csv(info_full, "movie_info.csv")
write.csv(daily_full, "movie_daily.csv")
