parse_film = function(url_extension) {
  
  # url_extension = "m/departed/"
  output = list()
  
  output$url = url_extension
  
  # Raw HTML Page ------------------------------------------------------------------
  page_raw = 
    glue("https://www.rottentomatoes.com/{url_extension}") %>%
    read_html()
  
  # Stats ------------------------------------------------------------------
  title = page_raw %>%
   xml_nodes(xpath = "//h1[@data-type='title']") %>%
   html_text() %>%
   magrittr::extract(1)
  
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
    magrittr::extract(1:6) # Collides with Tidyr's extract function
  
  output[paste("actor",seq(1,6), sep="_")] = cast
  
  output = lapply(output, function(x){ if(length(x)==0) {NA} else {x}})

  return(output) 

}
