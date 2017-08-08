get_actor_details = function(actor) {
  
  require(rvest)
  require(stringr)
  require(dplyr)
  require(magrittr)
  require(lubridate)
  
  # Format URL
  actor_string = str_replace_all(str_replace_all(tolower(actor), " ", "_"),"[\\-\\.]"," ")
  celeb_url = sprintf("https://www.rottentomatoes.com/celebrity/%s/", actor_string)

  output_object = tryCatch({
      
      output = list()

      # Try a couple of different URL formats as RT are inconsistent
      html_raw = tryCatch({
        
          #Most URLS look like "Tom_Bishop"  
          celeb_url %>%read_html()

      }, error = function(e) {

          #Some URLS randomly look like "Tom-Bishop"
          person_string = str_replace_all(str_replace_all(tolower(person),"[\\-\\.]"," "), " ", "-")
          celeb_url = sprintf("https://www.rottentomatoes.com/celebrity/%s/", person_string)        
          
          return(celeb_url %>% read_html())  
      })

      film_table = 
        html_raw %>% 
        html_nodes(xpath = "//*[@id='filmographyTbl'][1]") %>% 
        html_table() %>%
        extract2(1)

      film_links = 
        html_raw %>%
        html_nodes(xpath = "//table[@id='filmographyTbl'][1]/tbody/tr/td/a[contains(@class, 'articleLink')]/@href") %>%
        html_text()

      film_table =
        film_table %>%
        mutate(url_extension = film_links)

      names(film_table) = tolower(names(film_table))
            
      urls = 
        film_table %>%
        filter(rating != "No Score Yet") %>%
        select(url_extension)
      
      output$urls = urls

      output$dob = 
          html_raw %>%
          html_nodes(xpath =  "//div[contains(@class, 'celeb_bio_row')]/time/text()") %>%
          html_text() %>%
          mdy()

      return(output)

    }, error = function(e) {

      return(list("urls" = NA, "dob" = NA))

    })

  return(output_object)

}