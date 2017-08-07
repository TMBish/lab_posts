
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




parse_film = function(url_extension) {
  
  # url_extension = "m/departed/"
  
  output = list()
  
  output$url = url_extension
  
  # Raw HTML Page ------------------------------------------------------------------
  page_raw = 
    glue("https://www.rottentomatoes.com/{url_extension}") %>%
    read_html()
  
  # Stats ------------------------------------------------------------------
  title = page_raw %>% xml_nodes(xpath = "//h1[@data-type='title']") %>% html_text() %>% extract(1)
  
  output$year = title %>% str_extract("(?<=\\()\\d+(?=\\))") %>% as.integer()
  output$title = title %>% str_trim() %>% str_replace("\\(\\d+\\)","") %>% str_trim()
    
  metrics = page_raw %>%
    xml_nodes(xpath = "//*[@id='scoreStats']/div[contains(@class, 'superPageFontColor')]") %>%
    html_text() %>%
    magrittr::extract(1:4) %>%
    gsub("\\s", "", .) %>%
    str_split("\\:", simplify = TRUE)
  
  reviews = metrics[match("ReviewsCounted", metrics[,1]),2] %>% as.integer()
  output$reviews = reviews
  
  goods = metrics[match("Fresh", metrics[,1]),2] %>% as.integer()
  
  bads = metrics[match("Rotten", metrics[,1]),2] %>% as.integer()
  
  output$tomatometer = round((goods / reviews) * 100,0)
  
  output$av_rating = metrics[match("AverageRating", metrics[,1]),2] %>% str_extract(".+(?=\\/)") %>% as.double()
  
  # Audience Score ------------------------------------------------------------------
  output$audience = page_raw %>%
    xml_nodes(xpath = "//div[contains(@class,'audience-score')]") %>%
    html_text() %>%
    str_extract("\\d+") %>%
    as.integer()
  
  # Box Office ------------------------------------------------------------------
  output$box_office = page_raw %>%
    xml_nodes(xpath = "//div[.='Box Office: ']/following-sibling::div") %>%
    html_text() %>%
    str_replace_all("[\\,\\$]", "") %>%
    as.integer()
  
  # Director ------------------------------------------------------------------
  output$director = page_raw %>%
    xml_nodes(xpath = "//div[.='Directed By: ']/following-sibling::div") %>%
    html_text() %>%
    str_trim()
  
  # Cast ------------------------------------------------------------------
  cast = page_raw %>%
    xml_nodes(xpath = "//div[contains(@class, 'castSection')]/div/div/a/span/text()") %>%
    html_text() %>%
    str_trim() %>%
    extract(1:6)
  
  output[paste("actor",seq(1,6), sep="_")] = cast
 
  return(output) 
}
