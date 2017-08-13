
library(tidyr)

data =
  films %>% 
  select(title, year, tomatometer, director, actor_1:actor_3) %>%
  gather("role", "name", director, actor_1:actor_3) %>%
  mutate(role = ifelse(role == "director", "Director", "Actor")) %>%
  inner_join(film_folk) %>%
  mutate(
    age_at_production = year - dob_year
  ) %>%
  filter(between(age_at_production, 10,100)) 

write_csv(data, "C:/Users/Tom Bishop/Desktop/test.csv")


chart_data = 
  data %>%
  mutate(age_at_production = as.character(age_at_production)) %>%
  group_by(age_at_production) %>%
  summarise(films = n())

test = hchart(chart_data, "column", hcaes(x = age_at_production, y = films)) %>%
  hc_add_theme(tmbish)


chart_data = 
  data %>%
  mutate(age_at_production = as.character(age_at_production)) %>%
  group_by(age_at_production, role) %>%
  summarise(films = n()) %>%
  spread(role, films)

test = hchart(chart_data, "column", hcaes(x = age_at_production, y = films, group = role)) %>%
  hc_add_theme(tmbish)

actor = density(chart_data$Actor[!is.na(chart_data$Actor)])
director = density(chart_data$Director[!is.na(chart_data$Director)])

highchart() %>% 
  hcdensity(actor) %>%
  hc_add



test = 
  films %>%
  inner_join(film_folk, by = c("director" =  "name")) %>%
  mutate(
    age_at_production = year - dob_year
  ) %>%
  filter(age_at_production > 20) %>%
  mutate(age_bucket = )

chart_data = 
  test %>%
  mutate(age_bucket = paste(round(age_at_production/10) *10, "-", round(age_at_production/10) *10 + 10)) %>%
  select(tomatometer, age_bucket)
  

library(highcharter)
test_chart = 
  hchart(chart_data, "scatter", hcaes(x = age_at_production, y = tomatometer)) %>%
  hc_theme(tmbish)

test_chart = 
  hcboxplot(x = chart_data$tomatometer, var = chart_data$age_bucket) %>%
  hc_add_theme(tmbish)

highchart() %>%
  hc_yAxis_multiples(
    list(top = "0%", height = "70%"),
    list(top = "70%", height = "30%")
  ) %>%
  hc_add_series_boxplot(x = chart_data$tomatometer, by = chart_data$age_bucket, yAxis = 0) %>%
  hc_add_theme(tmbish) %>%
  # hc_yAxis(min = 0, max = 100)