# -------------------------------------------------------------------------
# Title: reads_scrape.R
# Purpose: Collect and transform Goodreads and author WikiData
# Author: Jacob Allen
# Created: May 30, 2022
# Modified: August 3, 2022
# -------------------------------------------------------------------------

# Install and load packages -----------------------------------------------

# install.packages("dpylr")
# install.packages("janitor")
# install.packages("gsubfn")
# install.packages("rvest")
# install.packages("stringr")
# install.packages("purrr")
# install.packages("lubridate")
# install.packages("stringi")
library(dplyr)
library(janitor)
library(gsubfn)
library(rvest)
library(stringr)
library(purrr)
library(lubridate)
library(stringi)

# Scrape book data from Goodreads overview page ---------------------------

home_url1 <- "https://www.goodreads.com/review/list/109079834-jacob-allen?order=d" # replace URL
home_url2 <- "&ref=nav_mybooks&shelf=read&sort=date_started&utf8=%E2%9C%93&per_page=20"

# Collect books read count and divide by 20 to get page count for iteration
page1 <- str_c(home_url1, "&page=", 1, home_url2)

book_ct <- 
  html_session(page1) %>% 
  read_html() %>% 
  html_nodes("#header .greyText") %>% 
  html_text(trim = TRUE) %>% 
  str_extract("(\\d+(\\.\\d+)?)") %>% 
  as.integer()

# Get book count from previous scrape, if it exists.
get_previous_read_ct <- function(x) {
  out <- tryCatch(
      {
      readRDS(x) %>% 
      select(c(title, author)) %>% 
      n_distinct()
      },
    error = function(e) {
      return(0)
      },
    warning = function(w) {
      return(0)
      }
  )
}

previous_ct <- get_previous_read_ct("...\\reads.rds")

gr_page_ct <- ceiling(((book_ct - previous_ct) / 20))

# Function to get books and book info from overview page 
get_book_data <- function(x) {
  cat(x, "\n") # prints page number
  home_url <- session_jump_to(my_session, str_c(home_url1, "&page=", x, home_url2))
  html <- read_html(home_url)

  # Book title
  title <- html %>% 
    html_nodes(".title a") %>% 
    html_text(trim = TRUE) %>% 
    str_replace("\n        ", " ")
  
  # Author of book
  author <- html %>% 
    html_nodes(".author a") %>% 
    html_text(trim = TRUE)
  
  # Average book rating
  avg_rating <- html %>% 
    html_nodes(".avg_rating") %>% 
    html_text(trim = TRUE) %>%
    tail(-1) %>% 
    str_extract("(\\d+(\\.\\d+)?)") %>% 
    as.numeric()
  
  # My rating of book
  my_rating <- html %>% 
    html_nodes(".rating") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_extract("it was amazing|really liked it|liked it|it was ok|didn't like it") %>% 
    str_replace("it was amazing", "5") %>% 
    str_replace("really liked it", "4") %>% 
    str_replace("liked it", "3") %>% 
    str_replace("it was ok", "2") %>% 
    str_replace("didn't like it", "1") %>% 
    as.numeric()
  
  # My review of book
  review <- html %>% 
    html_nodes("tr .review") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_trim() %>% 
    str_replace(".*\\n ", "") %>% 
    str_replace(".*\\n", "") %>% 
    str_replace(".*\\n", "") %>%
    str_replace("\\n  ...more", "") %>% 
    str_trim()
  
  # Date I began reading book
  date_start <- html %>% 
    html_nodes(".date_started") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_replace("date started\n    \n            \n                  \n      ", "")
  
  # Date I finished reading book
  date_finish <- html %>% 
    html_nodes(".date_read") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_replace("date read\n    \n            \n                  \n      ", "")
  
  # Number of pages in book
  pages <- html %>% 
    html_nodes(".num_pages") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_replace(",","") %>% 
    str_extract("(\\d+(\\.\\d+)?)") %>% 
    as.numeric()
  
  # Number of ratings book has (used to determine popularity)
  ratings <- html %>% 
    html_nodes(".num_ratings") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_replace(",","") %>% 
    str_extract("(\\d+(\\.\\d+)?)") %>% 
    as.numeric()
  
  # Year book was published
  yr_pub <- html %>% 
    html_nodes(".date_pub") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_replace("unknown", "NA") %>% 
    str_sub(., -4, -1) %>% 
    as.numeric()
  
  # Number of times I have read book
  read_ct <- html %>% 
    html_nodes(".read_count") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>% 
    str_extract("(\\d+(\\.\\d+)?)") %>% 
    as.numeric()
  
  # Book isbn
  isbn <- html %>% 
    html_nodes(".isbn") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>%
    str_extract("(\\d+(\\.\\d+)?)")
  
  # Book isbn 13
  isbn13 <- html %>% 
    html_nodes(".isbn13") %>% 
    html_text(trim = TRUE) %>% 
    tail(-1) %>%
    str_replace("isbn13", "") %>% 
    str_extract("(\\d+(\\.\\d+)?)")

  # URL of book page (used for further data collection below)
  book_url <- html %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    discard(!str_detect(., "^/book/show")) %>% 
    na.omit() %>% 
    unique()

  return(tibble(
    title = title,
    book_url = book_url,
    author = author,
    my_rating = my_rating,
    avg_rating = avg_rating,
    review = review,
    date_start = date_start,
    date_finish = date_finish,
    pages = pages,
    read_ct = read_ct,
    ratings = ratings,
    yr_pub = yr_pub,
    isbn = isbn,
    isbn13 = isbn13
    ))
}

book1 <- c(1:gr_page_ct) %>% 
  map_dfr(get_book_data)

# Transform dataframe -----------------------------------------------------

book1 <- book1 %>%   
  mutate(author_query = gsub(" ", "+", author),
         author_query = gsub(",", "", author_query),
         author_query = stri_trans_general(author_query, "Latin-ASCII"),
         avg_rating_rounded = round(avg_rating, digits = 0),
         my_rating_vs_gr = case_when(
           my_rating > avg_rating_rounded ~ "higher",
           my_rating < avg_rating_rounded ~ "lower",
           my_rating == avg_rating_rounded ~ "equal")
         )

# Scrape book data from Goodreads book page -------------------------------

# Create function to get description from book page
get_description <- function(x){
  cat(x, "\n")
  url <- str_c("https://www.goodreads.com", x)
  read_html(url) %>%
    html_node("#descriptionContainer") %>%
    html_text(trim = TRUE)
}

# Create function to get genre from book page
get_genre <- function(x){
  cat(x, "\n")
  url <- paste0("https://www.goodreads.com", x)
  read_html(url) %>%
    html_nodes(".elementList:nth-child(1) .left .bookPageGenreLink") %>%
    html_text(trim = TRUE) %>%
    pluck(1, 1)
}

# Create function to get ratings count from book page
get_ratings <- function(x){
  cat(x, "\n")
  url <- paste0("https://www.goodreads.com", x)
  read_html(url) %>%
    html_node(".gr-hyperlink:nth-child(7)") %>%
    html_text(trim = TRUE) %>%
    str_replace(",", "") %>% 
    str_extract("(\\d+(\\.\\d+)?)")
}

# Function to get data
  # While loops retry scrape if no values initially returned 
get_book_data2 <- function(x) {

  book_genre <- "NULL"
  genre_attempt <- 0
  
  while(book_genre == "NULL" && genre_attempt < 4) {
    genre_attempt <- genre_attempt + 1
    print(paste("genre attempt:", genre_attempt))
    try(
      book_genre <- x %>%
        map(get_genre)
    )
  }
  
  book_desc <- NA
  desc_attempt <- 0
  
  while(is.na(book_desc) && desc_attempt < 4) {
    desc_attempt <- desc_attempt + 1
    print(paste("description attempt:", desc_attempt))
    try(
      book_desc <- x %>%
        map_chr(get_description)
    )
  }

  return(tibble(
    book_url = as.character(x),
    book_genre = as.character(book_genre),
    book_desc = as.character(book_desc)
  ))
}

# Apply function to author list and save output as dataframe
  # Added delay to prevent hitting Goodreads too hard
book2 <- as.vector(book1$book_url) %>% 
  map_dfr(slowly(get_book_data2, rate = rate_delay(1)))

# Merge book1 and book2
book <- book1 %>% left_join(
  book2,
  by = "book_url"
)

# Scrape and transform author data from WikiData --------------------------

get_author_data <- function(x) {
  search_url <- str_c("https://www.wikidata.org/w/api.php?action=query&list=search&format=json&srsearch=", x)
  cat(x, "\n")
  author_id <- read_html(search_url) %>% 
    html_text() %>% 
    str_extract("(?<=title\":\")\\w+") # only grabbing first match
  
  # Fill in NA for authors that don't have a WikiData page
  if (is.na(author_id)) {
    return(tibble(
      author_query = x,
      author_id = "NA",
      author_sex = "NA",
      author_country = "NA",
      author_description = "NA"))
  } else {
   # Run the below for authors that do hava WikiData page 
    author_html <- read_html(str_c("https://www.wikidata.org/wiki/", author_id)) 
    
    # Author sex
    author_sex <- author_html %>% 
      html_nodes("#P21 .wikibase-snakview-body") %>% 
      html_text(trim = TRUE) %>% 
      pluck(1,1)
    
    # Author birth country
    author_country <- author_html %>% 
      html_nodes("#P27 .wikibase-snakview-body") %>% 
      html_text(trim = TRUE) %>% 
      pluck(1,1)

    # Collecting this to validate information collected is for intended author
    author_description <- author_html %>% 
      html_nodes(".wikibase-entitytermsforlanguageview-en .wikibase-descriptionview-text") %>% 
      html_text(trim = TRUE) 
    
    return(tibble(
      author_query = as.character(x), # converting all values to character to prevent mismatch type error with map_dfr
      author_id = as.character(author_id),
      author_sex = as.character(author_sex),
      author_country = as.character(author_country),
      author_description = as.character(author_description)))
  }
}

# Get distinct list of authors

author_list <- as.vector((unique(book$author_query)))

# Apply function to author list and save output as dataframe

author <- map_dfr(author_list, get_author_data)

# Join author data to book data -------------------------------------------

reads <- book %>% 
  left_join(author,
            by = "author_query")

# Handling re-reads -------------------------------------------------------

read_2x <- reads %>%
  filter(read_ct == 2 | read_ct == 3) %>%  
  mutate(
    date_start = str_replace_all(date_start, "\n", ""),
    date_start = trimws(str_extract(date_start, "\\s\\s(.*)")),
    date_start = ifelse(read_ct == 2, date_start, trimws(str_extract(date_start, "^(.*? ){3}"))),
    date_finish = str_replace_all(date_finish, "\n", ""),
    date_finish = trimws(str_extract(date_finish, "\\s\\s(.*)")),
    date_finish = ifelse(read_ct == 2, date_finish, trimws(str_extract(date_finish, "^(.*? ){3}"))),
    )

read_3x <- reads %>%
  filter(read_ct == 3) %>%  
  mutate(
    date_start = str_replace_all(date_start, "\n", ""),
    date_start = trimws(str_extract(date_start, "\\s\\s(.*)")),
    date_start = trimws(str_extract(date_start, "\\s\\s(.*)")),
    date_finish = str_replace_all(date_finish, "\n", ""),
    date_finish = trimws(str_extract(date_finish, "\\s\\s(.*)")),
    date_finish = trimws(str_extract(date_finish, "\\s\\s(.*)"))
  )

# Not handling for read count over 3
read_x <- reads %>% 
  filter(read_ct > 1) %>% 
  mutate(
    date_start = str_replace_all(date_start, "\n", ""),
    date_start = trimws(str_extract(date_start, "^(.*? ){3}")),
    date_finish = str_replace_all(date_finish, "\n", ""),
    date_finish = trimws(str_extract(date_finish, "^(.*? ){3}")),
  )

reads <- reads %>%
  filter(read_ct == 1 | read_ct > 3)

new <- rbind(reads,
               read_2x,
               read_3x,
               read_x)

# Reorder factor levels for month
new$date_finish_month <-  factor(new$date_finish_month, 
                                   levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Merge new with existing, if it exists -----------------------------------

get_final <- function(x) {
  out <- tryCatch(
      {
        rbind(new, readRDS(x)
          )
      },
      error = function(e) {
        return(final)
      },
      warning = function(w) {
        return(final)
      }
    )
    return(out)
  }

final <- get_final("...\\reads.rds") %>% 
  distinct(book_url, date_start, .keep_all = TRUE)

# Write csv and rds file --------------------------------------------------

write.csv(final,
          "...\\reads.csv",
          row.names = FALSE)

saveRDS(final,
        "...\\reads.rds")
