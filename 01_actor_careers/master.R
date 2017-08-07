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

actors = read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/actors.csv")
directors = read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/directors.csv")

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


for (row in 1:nrow(films)) {
  
  actor = actors$Name[row]
  
  print(actor)
  
  
  
  
  
}
