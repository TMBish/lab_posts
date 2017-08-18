library(readr)
library(dplyr)
library(highcharter)
library(ggplot2)
library(tidyr)
library(gender)
library(stringr)
library(lubridate)

# Data --------------------------------------------------------------------
films = read_csv("https://raw.githubusercontent.com/TMBish/lab_posts/master/01_actor_careers/00_full_film_data.csv") %>%
  filter(!is.na(title))


film_folk = read_csv("https://raw.githubusercontent.com/TMBish/lab_posts/master/01_actor_careers/00_actors_directors.csv")
film_folk[film_folk$dob < as.Date("1880-01-01"), "dob"] = as.Date("1880-01-01")

# Grep out first name and dob year for the gender function
film_folk = 
  film_folk %>%
  mutate(
    first_name = str_extract(name, "(?i)[a-z]+(?=\\s)"),
    dob_year = year(dob)
  )
  
# Distinct name, dob combos and predict gender
genders = 
  film_folk %>% 
  distinct(first_name, dob_year) %>%
  gender_df(name_col = "first_name", year_col = "dob_year") %>%
  select("first_name" = name, gender, "dob_year" = year_min)

film_folk = 
  film_folk %>%
  left_join(genders)

 # The deal with directors? --------------------------------------------------------------------
dir_data =
  films %>% 
  select("name" = director) %>%
  inner_join(film_folk) %>%
  group_by(gender) %>%
  summarise(
  	films = n()
  ) %>%
  filter(!is.na(gender))

# Most are men Yep

 # Volumes By Age --------------------------------------------------------------------

volume_data =
  films %>% 
  select(title, year, tomatometer, director, actor_1:actor_6) %>%
  gather("role", "name", director, actor_1:actor_6) %>%
  mutate(role = ifelse(role == "director", "Director", "Actor")) %>%
  inner_join(film_folk) %>%
  mutate(
    age_at_production = year - dob_year
  ) %>%
  filter(between(age_at_production, 10,100)) %>%
  select(name, gender, dob_year, age_at_production, year, role, tomatometer)


	# Ggplot
	var = "Director"
	temp = 
		volume_data %>%
		filter(
			role == var,
			!is.na(gender)
		)


	ggplot(data = temp) +
		geom_bar(aes(x = age_at_production)) +
		facet_grid(gender ~ .)

	# Highcharts
	var = "Actor"
	temp = 
		volume_data %>%
		filter(
			role == var,
			!is.na(gender)
		) %>%
		group_by(gender, age_at_production) %>%
		summarise(films = n()) %>%
		group_by(gender) %>%
		mutate(percentage = films / sum(films))

	chart = hchart(temp, "column", hcaes(x = age_at_production, y = percentage, group = gender)) %>%
  		hc_add_theme(tmbish)

  	# Including directors as a category
	temp = 
		volume_data %>%
		filter(
			!is.na(gender)
		) %>%
		mutate(
			category = case_when(
				role == "Director" ~ "director",
				gender == "male" ~ "male_actor",
				TRUE ~ "female_actor"
			)
		) %>% 
		group_by(category, age_at_production) %>%
		summarise(films = n()) %>%
		group_by(category) %>%
		mutate(percentage = films / sum(films))

	chart = hchart(temp, "spline", hcaes(x = age_at_production, y = percentage, group = category)) %>%
  		hc_add_theme(tmbish)

  	# PDF
  	chart_data = 
   temp %>%
    select(-films) %>%
    spread(category, percentage)

	chart = 
	  highchart() %>%
	  hc_add_theme(tmbish) %>%
	  hc_xAxis(
	    categories = chart_data$age_at_production,
	    title = list(text = "Age at Production")
	  ) %>%
	  hc_yAxis(
	    labels = list(formatter = JS("function(){return(Math.round(this.value * 1000) / 10 + '%')}"))
	  ) %>%
	  hc_title(text = "Female actors") %>%
	  hc_subtitle(text = "Comparing male actors, (male) directors and female actors") %>%
	  hc_add_series(name = "Director", type = "spline", data = chart_data$director) %>%
	  hc_add_series(name = "Male Actor", type = "spline", data = chart_data$male_actor) %>%
	  hc_add_series(
	    name = "Female Actor",
	    type = "area",
	    data = chart_data$female_actor,
	    zIndex = -10,
	    marker = list(enabled = FALSE),
	    fillOpacity = 0.5
	  )

	# CDF
	chart_data = 
	   temp %>%
	    select(-films) %>%
	    spread(category, percentage) %>%
	    mutate(
	      director = cumsum(ifelse(!is.na(director), director, 0)),
	      female_actor = cumsum(female_actor),
	      male_actor = cumsum(male_actor)
	    )

	chart = 
	  highchart() %>%
	  hc_add_theme(tmbish) %>%
	  hc_xAxis(
	    categories = chart_data$age_at_production,
	    title = list(text = "Age at Production")
	  ) %>%
	  hc_yAxis(
	    labels = list(formatter = JS("function(){return(Math.round(this.value * 1000) / 10 + '%')}")),
	    max = 1.1
	  ) %>%
	  hc_title(text = "Female actors") %>%
	  hc_subtitle(text = "Comparing male actors, (male) directors and female actors") %>%
	  hc_add_series(name = "Director", type = "spline", data = chart_data$director) %>%
	  hc_add_series(name = "Male Actor", type = "spline", data = chart_data$male_actor) %>%
	  hc_add_series(
	    name = "Female Actor",
	    type = "spline",
	    data = chart_data$female_actor,
	    zIndex = -10,
	    marker = list(enabled = FALSE),
	    fillOpacity = 0.5
	  )

 # Ratings By Age --------------------------------------------------------------------

rating_data =
  films %>% 
  select(title, year, tomatometer, director, actor_1:actor_3) %>%
  gather("role", "name", director, actor_1:actor_3) %>%
  inner_join(film_folk) %>%
  filter(!is.na(gender)) %>%
  mutate(
    age_at_production = year - dob_year,
    category = case_when(
			role == "director" ~ "director",
			gender == "male" ~ "male_actor",
			TRUE ~ "female_actor"
	)
  ) %>%
  filter(between(age_at_production, 15,85)) %>%
  select(name, gender, age_at_production, category, tomatometer) %>%
  group_by(name) %>% 
  mutate(
  	average_tomato  = mean(tomatometer),
  	films = n()
  ) %>% ungroup() %>%
  filter(films >= 5) %>%
  mutate(
  	index = tomatometer / average_tomato) %>%
  group_by(category, age_at_production) %>%
  summarise(rating = mean(index)) %>%
  spread(category, rating)

# LOESS Trend Line
loess_fit = loess(value ~ age_at_production , 
                  data = rating_data %>% gather("role", "value", -age_at_production)
                  # %>% group_by(age_at_production) %>% summarise(value = mean(value, na.rm = TRUE))
                  )
	
predictions = loess_fit %>% predict(seq(15,85))

chart = 
  highchart() %>%
  hc_add_theme(tmbish) %>%
  hc_chart(
    animation = list(duration = 2000)
  ) %>%
  hc_xAxis(
    categories = rating_data$age_at_production,
    title = list(text = "Age at Production")
  ) %>%
  hc_yAxis(
    title = list(text = "Probablity Density"),
    min = 0.6
  ) %>%
  hc_title(text = "Female actors") %>%
  hc_subtitle(text = "Comparing male actors, (male) directors and female actors") %>%
  hc_add_series(
    name = "Director",
    type = "scatter",
    data = rating_data$director,
    marker = list(radius = 3, symbol = "circle")
    ) %>%
  hc_add_series(
    name = "Male Actor",
    type = "scatter",
    data = rating_data$male_actor,
    marker = list(radius = 3, symbol = "circle")
    ) %>%
  hc_add_series(
    name = "Female Actor",
    type = "scatter",
    data = rating_data$female_actor,
    marker = list(radius = 3, symbol = "circle")
    
    ) %>%
  hc_add_series(
    name = "Trend",
    type = "spline",
    data = predictions,
    color = "#000000",
    marker = list(enabled = FALSE)
  )



