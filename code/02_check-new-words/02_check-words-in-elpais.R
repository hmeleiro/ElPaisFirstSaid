suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(RMariaDB)
  library(purrr)
  library(chromote)
  library(stringr)
})

source("code/00_functions.R", encoding = "UTF-8")

elpaisdb <- connect()
last_check <- dbReadTable(elpaisdb, "last_check")
dbDisconnect(elpaisdb)


if(nrow(last_check) > 0 ) {
  
  message('Checking words in elpais.com search engine...')
  
  session <- ChromoteSession$new()
  
  # CHECK IN THE elpais.com SEARCH ENGINE THE WORDS THAT WERE NOT FOUND IN THE DATABASE
  last_check <- 
    last_check %>% 
    mutate(elpaischeck = map_lgl(word, elpais_check, session = session))
  
  # FILTER THE WORDS THAT WERE NOT FOUN IN THE elpais.com SEARCH ENGINE
  # ALSO FILTER OUT WORDS WITH LESS THAN 3 CHARACTERS
  firstSaidWords <- 
    last_check %>% 
    filter(
      !elpaischeck,
      nchar(word) > 2,
      !str_detect(url, "/quadern/")
    )
  
  
  existingWords <- 
    last_check %>% 
    filter(!word %in% firstSaidWords$word)
  
  message("Found ", nrow(firstSaidWords), " first said words.")
  
  tryCatch({
    elpaisdb <- connect()
    values <- sprintf("('%s')", last_check$word)
    values <- paste0(values, collapse = ", ")
    q <- sprintf("INSERT IGNORE INTO words(word) VALUES %s", values)
    res <- dbExecute(elpaisdb, q)
  },
  finally = dbDisconnect(elpaisdb)
  )
  
  # if(nrow(firstSaidWords) > 0) {
  #   
  #   # INSERT NEW WORDS IN THE first_said TABLE
  #   tryCatch({
  #     elpaisdb <- connect()
  #     values <- sprintf("('%s', '%s', '%s')", firstSaidWords$word, firstSaidWords$url, firstSaidWords$context)
  #     values <- paste0(values, collapse = ", ")
  #     q <- sprintf("INSERT IGNORE INTO first_said(word, url, context) VALUES %s", values)
  #     dbExecute(elpaisdb, q)
  #   },
  #   finally = dbDisconnect(elpaisdb)
  #   )
  # }
  
  
  # DELETE ALL WORDS FROM last_check
  tryCatch({
    elpaisdb <- connect()
    # values <- paste0(last_check$word, collapse = "', '")
    values <- paste0(existingWords$word, collapse = "', '")
    q <- sprintf("DELETE FROM last_check WHERE word IN ('%s')", values)
    res <- dbExecute(elpaisdb, q)
  },
  finally = dbDisconnect(elpaisdb)
  )
  
  session$close()
  
}
