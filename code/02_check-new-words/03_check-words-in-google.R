suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(RMariaDB)
  library(purrr)
  library(chromote)
  library(stringr)
  library(httr2)
})

source("code/00_functions.R", encoding = "UTF-8")

elpaisdb <- connect()
last_check <- dbReadTable(elpaisdb, "last_check")
queries_left <- dbReadTable(elpaisdb, "google_rate_limit") %>% pull()
dbDisconnect(elpaisdb)

last_check <- slice_head(last_check, n = queries_left)

if(nrow(last_check) > 0) {
  
  last_check <- 
    last_check %>% 
    mutate(
      google_check = map_lgl(word, google_check)
    )
  
  
  firstSaidWords <- 
    last_check %>% 
    filter(!google_check)
  
  
  tryCatch({
    elpaisdb <- connect()
    values <- paste0(last_check$word, collapse = "', '")
    q <- sprintf("DELETE FROM last_check WHERE word IN ('%s')", values)
    res <- dbExecute(elpaisdb, q)
  },
  finally = dbDisconnect(elpaisdb)
  )
  
  
  if(nrow(firstSaidWords) > 0) {
    
    # INSERT NEW WORDS IN THE first_said TABLE
    tryCatch({
      firstSaidWords <- firstSaidWords %>% 
        mutate(contex = gsub(pattern = "'", "''", x = context))
      
      elpaisdb <- connect()
      values <- sprintf("('%s', '%s', '%s')", firstSaidWords$word, firstSaidWords$url, firstSaidWords$context)
      values <- paste0(values, collapse = ", ")
      q <- sprintf("INSERT IGNORE INTO first_said(word, url, context) VALUES %s", values)
      dbExecute(elpaisdb, q)
    },
    finally = dbDisconnect(elpaisdb)
    )
  }
  
  
  tryCatch({
    elpaisdb <- connect()
    q <- sprintf("update google_rate_limit set daily_queries_left = %s;", queries_left - nrow(last_check))
    dbExecute(elpaisdb, q)
    dbDisconnect(elpaisdb)
  },
  finally = dbDisconnect(elpaisdb)
  )

}
