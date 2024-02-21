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
dbDisconnect(elpaisdb)

last_check <- last_check[!duplicated(last_check$word), ]


if(nrow(last_check) > 0) {

  for (i in 1:nrow(last_check)) {
    word <- last_check$word[i]
    context <- last_check$context[i]
    url <- last_check$url[i]
    check <- google_check(word)
    
    if(is.null(check)) {
      #  PARAR SCRIPT
      message("Google API rate limit reached. Stopping script.")
      quit(save="no")
    }


    tryCatch({
      elpaisdb <- connect()
      q <- sprintf("DELETE FROM last_check WHERE word = '%s'", word)
      res <- dbExecute(elpaisdb, q)
      },
      finally = dbDisconnect(elpaisdb)
      )

      if(!check) {
        tryCatch({

        context <- gsub("'", "''", context)
      
        elpaisdb <- connect()
        values <- sprintf("('%s', '%s', '%s')", word, url, context)
        values <- paste0(values, collapse = ", ")
        q <- sprintf("INSERT IGNORE INTO first_said (word, url, context) VALUES %s", values)
        dbExecute(elpaisdb, q)
        },
        finally = dbDisconnect(elpaisdb)
        )
      } else {
        tryCatch({
        elpaisdb <- connect()
        q <- sprintf("INSERT IGNORE INTO words (word) VALUES ('%s')", word)
        dbExecute(elpaisdb, q)
        },
        finally = dbDisconnect(elpaisdb)
        )
      }
  }
}
