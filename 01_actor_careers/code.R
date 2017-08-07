library(readr)

actors = read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/actors.csv")
directors = read_csv("https://raw.githubusercontent.com/TMBish/Stratton/master/data/directors.csv")

films = data.frame(
  title = character(),
  url = character(),
  year = integer(),
  tomato_meter = integer(),
  box_office = integer(),
  director = character(),
  actor_1 = character(),
  actor_2 = character(),
  actor_3 = character(),
  actor_4 = character(),
  actor_5 = character(),
  actor_6 = character()
)



get_film_urls = function(actor) {
  
  require(rvest)
  require(stringr)
  require(dplyr)
  
  actor_string = str_replace_all(str_replace_all(tolower(actor), " ", "_"),"[\\-\\.]"," ")
  
  celeb_url = sprintf("https://www.rottentomatoes.com/celebrity/%s/", actor_string)

  films = tryCatch(
    {
      
      film_tables = tryCatch({
        #Most URLS look like "Tom_Bishop"  
        celeb_url %>%
          read_html() %>%
          html_nodes(xpath = "//*[@id='filmographyTbl'][1]") %>%
          html_table()        
      }, error = function(e) {
        #Some URLS randomly look like "Tom-Bishop"
        person_string = str_replace_all(str_replace_all(tolower(person),"[\\-\\.]"," "), " ", "-")
        celeb_url = sprintf("https://www.rottentomatoes.com/celebrity/%s/", person_string)        
        return(
          celeb_url %>%
            read_html() %>%
            html_nodes(xpath = "//*[@id='filmographyTbl']") %>%
            html_table()
        )  
      })
  
}



get_tomatoes = function(actor) {
  
  require(rvest)
  require(stringr)
  require(dplyr)
  
  actor_string = str_replace_all(str_replace_all(tolower(actor), " ", "_"),"[\\-\\.]"," ")
  
  celeb_url = sprintf("https://www.rottentomatoes.com/celebrity/%s/", actor_string)
  
  
  # Function to decode rotten tomatoes credit string into english
  #   only interested in actor, director, or actor / director roles
  assign_role = function(credit_string) {
    
    # Remove produce, screenwriter credits
    credit_string = str_replace_all(credit_string, "(?i)producer|screenwriter|(executive producer)"  ,"")
    
    if (grepl("director", credit_string, ignore.case = TRUE)){
      
      aug = str_replace_all(credit_string, "(?i)director", "")
      
      if (grepl("(actor)|[a-z]{3,}", aug, ignore.case = TRUE)) {
        
        return("Actor / Director")
        
      } else {
        
        return("Director")
        
      }
      
    } else if (grepl("(actor)|[a-z]{3,}", credit_string, ignore.case = TRUE)) {
      
      return("Actor")
      
    } else {
      
      # Must be a producer or some shit
      return("Null")
      
    }
    
  }
  
  # Scraping the filmography table using rvest
  # got the Xpath / CSS id of the table
  films = tryCatch(
    {
      
      film_tables = tryCatch({
        #Most URLS look like "Tom_Bishop"  
        celeb_url %>%
          read_html() %>%
          html_nodes(xpath = "//*[@id='filmographyTbl']") %>%
          html_table()        
      }, error = function(e) {
        #Some URLS randomly look like "Tom-Bishop"
        person_string = str_replace_all(str_replace_all(tolower(person),"[\\-\\.]"," "), " ", "-")
        celeb_url = sprintf("https://www.rottentomatoes.com/celebrity/%s/", person_string)        
        return(
          celeb_url %>%
            read_html() %>%
            html_nodes(xpath = "//*[@id='filmographyTbl']") %>%
            html_table()
        )  
      })
      
      
      films = film_tables[[1]]
      
      names(films) = tolower(names(films))
      
      films$role = lapply(films$credit, assign_role)
      
      films = 
        films %>%
        mutate(
          tomato_meter = str_replace_all(rating, "\\%", "")
        ) %>%
        select(-credit) %>%
        filter(
          rating != "No Score Yet",
          role != "Null"
        ) %>%
        mutate(
          rating = as.integer(rating),
          actor = person)
      
      return(films)
      
    }, error = function(e) {
      message("Actor or director not found")  
      return(NA)
    }
  )
  
  return(films)
  
}


get_tomatoes(actors[1,1])


## Psuedo Code

# For each actor / director in my top 500 lists:

  # Go to their rotten tomatoes page

  # Extract the list of films they're credited for
  
  # For each of these films:
    
    # Check films table to see if it exists
  
    # If not already in films table:
    
        # Go to this movie's rotten tomatoes page
  
        # Scrape the following data points:
  
          # Year, Title, Rotten tomatoes score, revenue, director, top 6 actors




test = 
  celeb_url %>%
  read_html() %>%
  html_nodes(xpath = "//table[@id='filmographyTbl'][1]/tbody/tr/td/a[contains(@class, 'articleLink')]/@href") %>%
  html_text()


%>%
  html_table()        

