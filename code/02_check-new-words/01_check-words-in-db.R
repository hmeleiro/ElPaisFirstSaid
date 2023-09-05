
suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(RMariaDB)
  library(tidytext)
  library(purrr)
  library(httr2)
  library(rvest)
  library(stringr)
})

source("code/00_functions.R", encoding = "UTF-8")

message('Scraping elpais.com frontpage...')
articles <- 
  tryCatch({
    urls <- 
      scrap_today_frontpage() %>% 
      filter(
        grepl("/elpais\\.com", url),
        nchar(url) >= 60
      )
    
    map_df(urls$url, scrap_article)
  } ,
  error = function(e) {
    message(date, " ", e$message)
  })


message('Checking words in database...')
firstSaidWords <- map_df(1:nrow(articles), function(i) {
  article <- articles[i,]
  db_check(article)
})

message("Found ", nrow(firstSaidWords), " words that are not in the database.")


if(nrow(firstSaidWords) > 0 ) {
  
  firstSaidWords <-
    firstSaidWords %>% 
    filter(!duplicated(word))
  
  tryCatch({
    
    firstSaidWords <- 
      firstSaidWords %>% 
      mutate(context = gsub(pattern = "'", "''", x = context))
    
    elpaisdb <- connect()
    values <- sprintf("('%s', '%s', '%s')", firstSaidWords$word, firstSaidWords$url, firstSaidWords$context)
    values <- paste0(values, collapse = ", ")
    
    q <- sprintf("INSERT IGNORE INTO last_check(word, url, context) VALUES %s", values)
    res <- dbExecute(elpaisdb, q)
  },
  finally = dbDisconnect(elpaisdb)
  )
}
