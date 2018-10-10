chart_scores = function(tom, hannah) {
  
  Tom = lapply(1:tom, function(x) {
    tibble(Name = 1, Rating = x)
  }) %>% bind_rows() %>% mutate(Max = max(Rating))
  
  Hannah = lapply(1:hannah, function(x) {
    tibble(Name = 2, Rating = x)
  }) %>% bind_rows() %>% mutate(Max = max(Rating))
  
  df = Tom %>% union(Hannah)
  
  
  hchart(df,
         "scatter", 
         hcaes(x = Rating, y = Name), 
         color = "#F7C919",
         name = "Overall Rating"
  ) %>%
  hc_yAxis(
    min = 1, max = 2, lineWidth = 0,
    categories = c("","Tom", "Hannah"),
    breaks = list(from = 1, to = 2, breakSize = 1),
    title = list(text = "")
  ) %>% 
  #hc_chart(height = 250) %>%
  hc_xAxis(
    min = 0, max = 10, tickWidth = 0,
    lineWidth = 0.5, 
    lineColor = "#d3d3d3",
    gridLineColor = "#d3d3d3",
    labels = list(),
    title = list(text = "")
  ) %>%
  hc_plotOptions(
    scatter = list(
      marker = list(
        symbol = "circle",
        lineWidth = 0,
        radius = 8
      )
    )
  ) %>%
  hc_tooltip(
    shape = "square",
    formatter = JS("function(){return('<b> Overall Rating: </b>' + this.point.Max + '/10')}")
  ) %>%
  hc_size(height = "150px")
  
}

chart_radar = function(movie, acting = 5, dialogue = 5, story = 5, entertainment = 5, cinematography = 5) {
  
  labs <- c("Cinematography", "Acting", "Dialogue", "Story", "Entertainment")
  
  scores <- list(
    movie = c(cinematography, acting, dialogue, story, entertainment)
  )
  
  chartJSRadar(scores = scores,
               labs = labs,
               maxScale = 10,
               showLegend = FALSE,
               polyAlpha = 0.1,
               labelSize=10, 
               height = "300px",
               width = "300px",
               scaleStepWidth = 5,
               colMatrix = matrix(c(247,201,25))
  )
  
}

compile_review = function(yaml_chunk, edition) {
  
  film_title = yaml_chunk %>% names()
  yaml_body = yaml_chunk %>% pluck(film_title)

  # Pick of Week Text
  pow_text = ifelse(
    yaml_body$`pick-of-the-week`,
    '<span class="label upper outline error"> PICK OF THE WEEK </span>',
    ''
  )

  div(class = "film-review",
    
    div(class = "film-title",
      
      h1(film_title)

    ),

    div(class="film-review-left",
           
      # Image
      tags$img(src = glue("/img/films/{edition}/{yaml_body$image}")),

      # Tags
      HTML(
        glue(
        '<span class="label upper outline warning"> {toupper(yaml_body$genre)} </span> {pow_text}'
        )
      )

      # Reviewer
      HTML(glue("<b> {yaml_body$reviewer} : </b>")),
      
      # Review Text
      markdown::markdownToHTML(
        text = yaml_body$`review-text` , 
        fragment.only = TRUE
      ) %>% HTML()

    ),
    
    div(class="film-review-right",
      
      div(class = "review-component",
        HTML(glue("<b> Similar to: </b> {yaml_body$`similar-film`}"))
      ),
  
      # Rating Overall
      div(class="ratings review-component",
        chart_scores(tom = yaml_body$ratings$overall$tom, hannah = yaml_body$ratings$overall$hannah)
      ),
      
      # Rating Radar
      div(class = "review-component",
        chart_radar(
          film_title, 
          acting = yaml_body$ratings$components$acting, 
          dialogue = yaml_body$ratings$components$dialogue, 
          story = yaml_body$ratings$components$story, 
          entertainment = yaml_body$ratings$components$entertainment, 
          cinematography = yaml_body$ratings$components$cinematography
        )
      )
      
    )
    
  )
  
}
