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
library(parallel)

options(stringsAsFactors = FALSE)

actors = read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/actors.csv")

directors = read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/directors.csv")

film_folk = actors %>%
  union_all(directors) %>%
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

start_time = Sys.time()

for (row in 1:nrow(film_folk)) {
  
  person = film_folk$Name[row]
  
  # Call the get_actor_details function
  details = get_bio(person)
  
  if (length(details$url)== 1 && is.na(details$urls) || nrow(details$urls) == 0) { next }
  
  # Log the actor details to the console
  cat(glue("Actor: {person} || DOB: {details$dob} || Number of URLS: {nrow(details$urls)}"),"\n") 
  
  # Add DOB to actor / director table
  film_folk[row,"dob"] = details$dob

  # Get URLS
  urls = details$url %>% distinct(url_extension)
  
  # DRY = DON'T REPEAT YOURSELF
  if ( nrow(films) > 0) {
    # Remove movies we've already scraped
    starting_rows = nrow(urls)
    urls = 
      urls %>%
      anti_join(films %>% select(url), by = c("url_extension" = "url"))
    
    if(starting_rows != nrow(urls)) {
      cat(glue("Already recorded {starting_rows - nrow(urls)} films!"), "\n")
    }
    
    if (nrow(urls)== 0) {next}
  }

  # Initialise the cluster
  film_list = as.list(urls$url_extension)
  no_cores = max(detectCores() - 2,1)
  cl = makeCluster(no_cores)
  
  # Multithread the film detail extraction
  results = parLapply(cl, film_list, parse_film)

  # Close the cluster
  stopCluster(cl)
 
  to_df = do.call(rbind.data.frame, results)
  
  films = rbind(films, to_df)
  
}

end_time = Sys.time()

write_csv(films, "C:/Users/Tom Bishop/Desktop/full_film_data.csv")

