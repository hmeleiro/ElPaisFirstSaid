library(dplyr)
library(DBI)
library(RMariaDB)

source("code/00_functions.R", encoding = "UTF-8")

from <- as.Date("2013-01-01")
dates <- seq.Date(from, Sys.Date(), 1)
i = 1
for (i in 1:length(dates)) {
  date <- dates[i]
  
  if(substr(date, 9, 10) == "01") {
    message(date)
  }
  
  elpaisdb <- dbConnect(RSQLite::SQLite(), "data/elpaisdb.sqlite")
  tryCatch({
    results <- scrap_frontpage(date) %>% 
      filter(
        grepl("/elpais\\.com", url),
        nchar(url) >= 60
      )
    dbWriteTable(elpaisdb, "headings", results, append = T)
  } ,
  error = function(e) {
    message(date, " ", e$message)
  }, 
  finally = dbDisconnect(elpaisdb)
  )
}


# DELETE DUPLICATES
message("Deleting duplicates...")
elpaisdb <- dbConnect(RSQLite::SQLite(), "data/elpaisdb.sqlite")

q <- "DELETE FROM headings
WHERE EXISTS (
  SELECT 1 FROM headings p2 
  where headings.url = p2.url AND 
  headings.title = p2.title AND 
  headings.rowid > p2.rowid
);"

tryCatch(
  dbExecute(elpaisdb, q),
  finally = dbDisconnect(elpaisdb)
)
