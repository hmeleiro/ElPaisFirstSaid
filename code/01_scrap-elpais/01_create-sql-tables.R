library(DBI)
library(RMariaDB)

source("code/00_functions.R", encoding = "UTF-8")

elpaisdb <- connect()


q <- "CREATE TABLE headings (
   id INTEGER PRIMARY KEY AUTOINCREMENT,
   title TEXT,
   url TEXT
);"

dbExecute(elpaisdb, q)


q <- "CREATE TABLE words (
   id INTEGER,
   word VARCHAR(255) PRIMARY KEY
);"

dbExecute(elpaisdb, q)

q <- "CREATE TABLE articles (
   title TEXT,
   subtitle TEXT,
   body TEXT,
   word VARCHAR(255) PRIMARY KEY
);"

dbExecute(elpaisdb, q)


q <- "CREATE TABLE last_check (
   word VARCHAR(255) PRIMARY KEY,
   url TEXT
);"

dbExecute(elpaisdb, q)


q <- "CREATE TABLE first_said (
   word VARCHAR(255) PRIMARY KEY,
   url TEXT
);"

dbExecute(elpaisdb, q)


q <- "CREATE TABLE google_rate_limit (
   daily_queries_left INT
);"

dbExecute(elpaisdb, q)

dbDisconnect(elpaisdb)
