library(DBI)
library(RMariaDB)

source("code/00_functions.R", encoding = "UTF-8")

elpaisdb <- connect()

q <- "update google_rate_limit set daily_queries_left = 100;"
dbExecute(elpaisdb, q)
dbDisconnect(elpaisdb)