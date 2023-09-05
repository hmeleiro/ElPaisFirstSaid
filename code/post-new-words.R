suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(RMariaDB)
  library(httr)
  library(stringr)
})

source("code/00_functions.R", encoding = "UTF-8")

elpaisdb <- connect()
first_said <- dbReadTable(elpaisdb, "first_said")
dbDisconnect(elpaisdb)


first_said <- 
  first_said %>% 
  mutate(
    n = nchar(context) + 24,
    context = if_else(n > 260, 
                      str_extract(context, sprintf('.{0,117}%s.{0,117}', word)),
                      context),
    n = nchar(context) + 24
  ) %>% 
  select(-n)

if(nrow(first_said) > 0) {
  
  api_key <- Sys.getenv("TW_FIRSTSAIDELPAIS_API_KEY")
  api_secret_key <- Sys.getenv("TW_FIRSTSAIDELPAIS_API_SECRET")
  api_access_token <- Sys.getenv("TW_FIRSTSAIDELPAIS_ACCESS_TOKEN")
  api_access_secret <- Sys.getenv("TW_FIRSTSAIDELPAIS_ACCESS_SECRET")
  
  word <- first_said[1,]
  tweet_info <- post_tweet(
    word$word, 
    "first_said_elpais",
    api_key, api_secret_key, api_access_token, api_access_secret
  )
  
  api_key <- Sys.getenv("TW_FIRSTSAIDELPAIS_CONTEXT_API_KEY")
  api_secret_key <- Sys.getenv("TW_FIRSTSAIDELPAIS_CONTEXT_API_SECRET")
  api_access_token <- Sys.getenv("TW_FIRSTSAIDELPAIS_CONTEXT_ACCESS_TOKEN")
  api_access_secret <- Sys.getenv("TW_FIRSTSAIDELPAIS_CONTEXT_ACCESS_SECRET")
  
  
  
  if(!is.na(word$context)) {
  context_tweet <- sprintf('"%s" publicado en: %s', word$context, word$url)
  } else {
    context_tweet <- sprintf('Publicado en: %s', word$url)
  }
  
  
  # Check if length of tweet is less than 260
  tweet_length <- 
   ( context_tweet %>% 
    str_remove(word$url) %>% 
    nchar()) + 23 # URLs take no more than 23 characters
  
  if(tweet_length > 260) {
    # TO DO: CUT CONTEXT IF TWEET IS LONGER THAN 260
    context_tweet <- sprintf('Publicado en: %s', word$url)
  }
  
  post_tweet(
    context_tweet,
    "FirstSaidElPais_context",
    api_key, api_secret_key, api_access_token, api_access_secret, 
    "reply" = list("in_reply_to_tweet_id" = tweet_info$data$id)
  )
  
  tryCatch({
    elpaisdb <- connect()
    values <- sprintf("('%s')", word$word)
    q <- sprintf("INSERT IGNORE INTO words(word) VALUES %s", values)
    dbExecute(elpaisdb, q)
    
    q <- sprintf("DELETE FROM first_said WHERE word = '%s'", word$word)
    res <- dbExecute(elpaisdb, q)
    
  },
  finally = dbDisconnect(elpaisdb)
  )
  
}
