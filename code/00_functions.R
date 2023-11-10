# connect <- function() {
#   elpaisdb <- dbConnect(RSQLite::SQLite(), "data/elpaisdb.sqlite")
# }

connect <- function() {
  require(RMariaDB)
  require(DBI)
  
  drv <- dbDriver("MariaDB")
  con <- dbConnect(
    drv = drv,
    username = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    host = Sys.getenv("DB_HOST"),
    port = "3306",
    dbname = Sys.getenv("DB_NAME")
  )
  return(con)
}



scrap_frontpage <- function(date) {
  require(httr2)
  require(dplyr)
  require(rvest)
  
  baseurl <- "https://elpais.com/hemeroteca/elpais/%s/m/portada.html"
  
  formated_date <- format(date, "%Y/%m/%d")
  url <- sprintf(baseurl, formated_date)
  
  resp <- 
    tryCatch(
      request(url) %>%
        req_perform(), 
      error = function(e) {
        # message(e$message, " Trying static.elpais.com")
        static_url <- gsub(".+//", "", url)
        static_url <- paste0("static.", static_url)
        request(static_url) %>%
          req_error(is_error = function(resp) FALSE) %>% 
          req_perform()
      }
    )
  
  if(!resp_is_error(resp)) {
    
    html <- resp %>% 
      resp_body_html()
    
    headings <- 
      html %>% 
      html_elements("h2")
    
    title <- headings %>% 
      html_text2()
    
    url <- headings %>% 
      html_element("a") %>% 
      html_attr(name = "href") %>% 
      if_else(substr(., 1, 1) == "/", paste0("http://elpais.com", .), .) %>%
      gsub("\\?.+", "", x = .)
    
    tibble(title, url)
    
  } else {
    NULL
  }
  
}

scrap_today_frontpage <- function() {
  require(httr2)
  require(dplyr)
  require(rvest)
  
  url <- "https://elpais.com"
  
  resp <- 
    tryCatch(
      request(url) %>%
        req_perform(), 
      error = function(e) {
        # message(e$message, " Trying static.elpais.com")
        static_url <- gsub(".+//", "", url)
        static_url <- paste0("static.", static_url)
        request(static_url) %>%
          req_error(is_error = function(resp) FALSE) %>% 
          req_perform()
      }
    )
  
  if(!resp_is_error(resp)) {
    
    html <- resp %>% 
      resp_body_html()
    
    headings <- 
      html %>% 
      html_elements("h2")
    
    title <- headings %>% 
      html_text2()
    
    url <- headings %>% 
      html_element("a") %>% 
      html_attr(name = "href") %>% 
      if_else(substr(., 1, 1) == "/", paste0("http://elpais.com", .), .) %>%
      gsub("\\?.+", "", x = .)
    
    tibble(title, url)
    
  } else {
    NULL
  }
  
}

scrap_article <- function(url) {
  require(httr2)
  require(dplyr)
  require(rvest)
  
  resp <- request(url) %>%
    req_error(is_error = function(resp) FALSE) %>% 
    req_perform()
  
  
  if(!resp_is_error(resp)) {
    
    html <- resp %>% 
      resp_body_html()
    
    title <- 
      html %>% 
      html_elements("h1") %>% 
      html_text() %>% 
      ifelse(length(.) == 0, NA, .)
    
    subtitle <- 
      html %>% 
      html_elements("h2") %>% 
      html_text() %>% 
      ifelse(length(.) == 0, NA, .)
    
    body <- 
      html %>% 
      html_elements("article p") %>% 
      html_text2(preserve_nbsp = T) 
    
    # Removes template text from embed instagram posts
    body <- body[!grepl("A post shared by", body)]
    
    body <- 
      body %>% 
      paste0(collapse = " ") %>% 
      ifelse(length(.) == 0, NA, .)
    
    
    tibble(title, subtitle, body, url)
    
  } else {
    NULL
  }
}

cleanString <- function(text) {
  # source: # https://gist.github.com/somerandomnerd/7529732
  # PROPER_NOUNS <- "(([A-Z]([a-z]+|\.+))+(\s[A-Z][a-z]+)+)|([A-Z]{2,})|([a-z][A-Z])[a-z]*[A-Z][a-z]*"
  PROPER_NOUNS <- "([A-Z]|[À-Ü])([A-Z]|[À-Ü]|[a-z]|[à-ü])+"
  PUNCTS_NUMS <- "[0-9]+|[[:punct:]]|\\(.*\\)"
  MULTIPLE_SPACES <- " +"
  URL <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  URL <- "[[:alpha:]]+[.](es|com|net|cat)"
  URL_TWITTER_PICS <- "pic\\.twitter\\.(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  TWITTER_HANDLES <- "#[A-ZÀ-Üa-zà-ü0-9_ñ]+|@[A-Za-z0-9_]+|@[A-Za-z0-9]+|\\w+(?:\\.\\w+)*/\\S+"
  ALL_UPPERCASE <- "(\\s|[[:punct:]])[[:upper:]]{2,}(\\s|[[:punct:]])"
  INVISIBLE_CHARS <- "[^\\P{C}\r\n]+"
  
  text %>% 
    gsub(pattern = INVISIBLE_CHARS, replacement = "", perl=TRUE) %>% 
    gsub(pattern = URL_TWITTER_PICS, replacement = " ") %>%
    gsub(pattern = URL, replacement = " ") %>%
    gsub(pattern = ALL_UPPERCASE, replacement = " ") %>%
    gsub(pattern = TWITTER_HANDLES, replacement = " ") %>%
    gsub(pattern = PUNCTS_NUMS, replacement = " ") %>% 
    gsub(pattern = MULTIPLE_SPACES, replacement = " ") %>%
    gsub(pattern = PROPER_NOUNS, replacement = "") %>% 
    gsub(pattern = MULTIPLE_SPACES, replacement = " ") 
  
}


tokenize <- function(article) {
  
  title_words <- 
    article %>% 
    unnest_tokens(context, title, token = "sentences", to_lower = F) %>% 
    mutate(clean_context = cleanString(context)) %>% 
    unnest_tokens(word, clean_context, drop = F) %>% 
    select(url, context, word) 
  
  body_words <- 
    article %>% 
    unnest_tokens(context, body, token = "sentences", to_lower = F) %>% 
    mutate(clean_context = cleanString(context)) %>% 
    unnest_tokens(word, clean_context, drop = F) %>% 
    select(url, context, word) 
  
  rbind(title_words, body_words) %>% 
    filter(
      !duplicated(word),
      nchar(word) > 2
    ) 
}

checkWordInDb <- function(word) {
  elpaisdb <- connect()
  check <- 
    tryCatch({
      q <- sprintf("select exists(select 1 from words where word = '%s' limit 1)", word)
      dbGetQuery(elpaisdb, q) %>% pull() %>% as.logical()
    },
    finally = dbDisconnect(elpaisdb)
    )
  
  return(check)
}

google_search <- function(query, ...) {
  require(dplyr)
  require(httr2)
  
  google_api_key <- Sys.getenv("GOOGLE_SEARCH_KEY")
  cx <- Sys.getenv("GOOGLE_SEARCH_ELPAIS_CX")
  
  baseurl <- "https://www.googleapis.com/customsearch/v1"
  resp <- request(baseurl) %>%
    req_url_query(
      key = google_api_key, 
      cx = cx, 
      q = query,
      exactTerms = query
      # ...
    ) %>% 
    req_error(is_error = function(resp) FALSE) %>% 
    req_perform()
  
  resp_body_json(resp) 
}

google_check <- function(word) {
  results <- google_search(word)
  check <- as.numeric(results$searchInformation$totalResults) > 1
  Sys.sleep(1)
  return(check)
}


elpais_check <- function(query) {
  require(httr)
  url <- sprintf("https://elpais.com/pf/api/v3/content/fetch/enp-search-results?query={%%22q%%22:%%22%s%%22,%%22page%%22:1,%%22limit%%22:20,%%22language%%22:%%22es%%22}&_website=el-pais", query)
  x <- GET(url)
  
  if(!http_error(x$status_code)) {
    res <- content(x)
    return(res$numResults > 1)
  } else {
    return(F)
  }
  
  Sys.sleep(1.5)
}


db_check <- function(article) {

  words <- 
    tryCatch(
      tokenize(article),
      error = function(e) {
        message("Error tokenizing article: ", e$message)
      }
    )
  
  # Database check
  dbchecks <- map_lgl(words$word, checkWordInDb)
  words <- words[!dbchecks,]

  if(nrow(words) > 0) {
    firstSaidWords <- mutate(words, url = article$url)
  } else {
    firstSaidWords <- NULL
  } 
  
  return(firstSaidWords)
} 



post_tweet <- function(tweet, app_name, api_key, api_secret_key, api_access_token, api_access_secret, ...) {
  require(httr)
  require(dplyr)
  baseurl = "https://api.twitter.com"
  endpoint <- "/2/tweets"
  app <- oauth_app(app_name, key = api_key, secret = api_secret_key)
  
  url <- httr::modify_url(baseurl, path = endpoint)
  info <- httr::oauth_signature(url, method = "POST", app = app, 
                                token = api_access_token, token_secret = api_access_secret)
  header_oauth <- httr::oauth_header(info)
  
  body <- list("text" = tweet, ...)
  resp <- httr::POST(url, header_oauth, body = body, encode = "json")
  resp %>% 
    httr::content(as = "parsed")
  
}


