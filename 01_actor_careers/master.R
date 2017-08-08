## Psuedo Code

  # For each actor / director in my top 500 lists:
  
    # Go to their rotten tomatoes page +++
    
    # Extract the list of films they're credited for +++
    
    # For each of these films:
    
      # Check films table to see if it exists
      
      # If not already in films table:
      
        # Go to this movie's rotten tomatoes page +++
        
        # Scrape the following data points: +++
        
          # Year, Title, Rotten tomatoes score, revenue, director, top 6 actors +++

library(readr)
library(rvest)
library(magrittr)
library(glue)
library(lubridate)
library(purrr)
library(dplyr)

options(stringsAsFactors = FALSE)

actors = 
  read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/actors.csv") %>%
  mutate(dob = ymd(19900101))

directors = 
  read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/directors.csv") %>%
  mutate(dob = ymd(19900101))

films = data.frame(
  title = character(),
  url = character(),
  year = integer(),
  reviews = integer(),
  av_rating = double(),
  tomatometer = integer(),
  audience = integer(),
  box_office = integer(),
  director = character(),
  actor_1 = character(),
  actor_2 = character(),
  actor_3 = character(),
  actor_4 = character(),
  actor_5 = character(),
  actor_6 = character()
)

start = Sys.time()
for (row in 1:1) {
  
  cat(paste(glue("Actor: {actor} || DOB: {details$dob} || Number of URLS: {nrow(details$urls)}"),"\n"))  

  actor = actors$Name[row]
  
  details = get_actor_details(actor)

  # Add DOB to actor / director table
  actors[row,"dob"] = details$dob

  # Get URLS
  urls = details$url %>% distinct(url_extension)

  # DRY = DON'T REPEAT YOURSELF
  if ( nrow(films) > 0) {
    number_1 = nrow(urls)
    urls = 
      urls %>%
      anti_join(films$url)
    number_2 = nrow(urls)

    cat(paste(glue("Saved {number_1-number_2} scrapes!", "\n")))
  }

  # Multithread this mfer
  for (i in 1:nrow(urls)) {
    cat(paste(glue("Looking at film {urls$url_extension[i]}"), "\n"))
    this_film = parse_film(urls$url_extension[i])
    films = rbind(films, this_film)
  }

  # film_details = parse_film(url_extension) 
    # %>% # This is a nicely named list
    # enframe() %>% # Tibble function to convert named lists to DF - creates list columns tho
    # spread(name, value) %>%
    # dmap(unlist) # Purrr function to unnest the list columns

}
stop = Sys.time()



start = Sys.time()
for (row in 1:1) {
  
  cat(paste(glue("Actor: {actor} || DOB: {details$dob} || Number of URLS: {nrow(details$urls)}"),"\n"))  

  actor = actors$Name[row]
  
  details = get_actor_details(actor)

  # Add DOB to actor / director table
  actors[row,"dob"] = details$dob

  # Get URLS
  urls = details$url %>% distinct(url_extension)

  # DRY = DON'T REPEAT YOURSELF
  if ( nrow(films) > 0) {
    number_1 = nrow(urls)
    urls = 
      urls %>%
      anti_join(films$url)
    number_2 = nrow(urls)

    cat(paste(glue("Saved {number_1-number_2} scrapes!", "\n")))
  }

  data = foreach(url = urls$url_extension, .combine='rbind') %do% parse_film(url)

  # # Multithread this mfer
  # for (i in 1:nrow(urls)) {
  #   cat(paste(glue("Looking at film {urls$url_extension[i]}"), "\n"))
  #   this_film = parse_film(urls$url_extension[i])
  #   films = rbind(films, this_film)
  # }

}
stop = Sys.time()