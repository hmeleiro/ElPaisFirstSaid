library(dplyr)
library(DBI)
library(RMariaDB)
library(tidytext)

source("code/00_functions.R", encoding = "UTF-8")

elpaisdb <- connect()
headings <- 
  tryCatch(
    dbReadTable(elpaisdb, "headings"),
    finally = dbDisconnect(elpaisdb)
  )

headings <- 
  headings %>% 
  filter(!duplicated(headings))

for (i in 131294:nrow(headings)) {
  
  url <- headings$url[i]
  
  article <- tryCatch(
    scrap_article(url),
    error = function(e) {
      message("Error scraping article ", i, ": ", e$message)
    }
  )
  
  if(!is.null(article)) {
    
    words <- 
      tryCatch(
        tokenize(article),
        error = function(e) {
          message("Error tokenizing article ", i, ": ", e$message)
        }
      )
    
    tryCatch({
      article <- article %>% 
        mutate(across(everything(), ~gsub(pattern = "'", "''", x = .)))
      
      elpaisdb <- connect()
      values <- sprintf("('%s', '%s', '%s', '%s')", article$title, article$subtitle, article$body, article$url)
      q <- sprintf("INSERT IGNORE INTO articles(title, subtitle, body, url) VALUES %s", values)
      dbExecute(elpaisdb, q)
    },
    error = function(e) { message("Error writing article ", i, " in the database: ", e$message)},
    finally = dbDisconnect(elpaisdb)
    )
    
    tryCatch({
      elpaisdb <- connect()
      values <- sprintf("('%s')", paste0(words, collapse = "'), ('"))
      q <- sprintf("INSERT IGNORE INTO words(word) VALUES %s", values)
      dbExecute(elpaisdb, q)
    },
    error = function(e) { message("Error writing words of article ", i, " in the database: ", e$message)},
    finally = dbDisconnect(elpaisdb)
    )
  }
  
  
  if(i == 1 | i %% 500 == 0) {
    message("Completed ", i, " / ", nrow(headings))
  }
}








