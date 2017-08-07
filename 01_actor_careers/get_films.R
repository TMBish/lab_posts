get_film = function(actor) {
  
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
      
    })