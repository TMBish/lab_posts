library(tidyverse)
library(googlesheets)
library(lubridate)
library(purrr)
library(dplyr)

# Get Data ----------------------------------------------------------------

# Base filter was causing conflict
filter <- dplyr::filter
lag <- dplyr::lag
lead <- dplyr::lead

# Extract the google sheets keys from the urls
work_sheet_key = extract_key_from_url("https://docs.google.com/spreadsheets/d/1E80AoNSK2m_rfLWsL4qr41-3mP7pUEeuy3CG7J3--zs/edit#gid=0")
gymhome_sheet_key = extract_key_from_url("https://docs.google.com/spreadsheets/d/1dJWyirpVY7eq7WPdMlnEvvVIH957sEYMLYWK6e_MJCs/edit#gid=0")

# Read in data
prep_gsheets = function(key) {
  
  key %>%
  gs_key() %>%
  # Don't read first row as column headers
  gs_read(col_names = FALSE) %>%
  setNames(c("action", "date_time", "location")) %>%
  mutate(
    date_time = 
      # It seems dates are logged at US Mountain time for some reason...
      parse_date_time(date_time, orders = "%B %d %Y %I:%M %p", tz = "US/Mountain") %>% 
      # Convert to Melb time
      with_tz("Australia/Melbourne")
  ) %>%
  return()
}

# Map prep gsheets function across the keys and combine tables
raw_df = c(work_sheet_key, gymhome_sheet_key) %>%
  map(prep_gsheets) %>%
  bind_rows()


# Restructure ----------------------------------------------------------------

# Add cols and filter
df = raw_df %>% 
  # Due to hard date cut-off need to add a first home entry
  union_all(tibble(action = "entered", date_time = as_datetime("2017-06-12 17:00:00") %>%  with_tz("Australia/Melbourne"), location = "Home")) %>%
  mutate(
    date = as_date(date_time),
    wday = wday(date, label = TRUE)
  ) %>%
  filter(date >= '2017-06-12')

# Cleaning Data ----------------------------------------------------------------

# Function to get median travel times
get_median_travel = function(dt, second_loc = "Gym") {
  
  dt %>%
    arrange(date_time, desc(location)) %>%
    mutate(
      travel = case_when(
        (location=="Home" & action == "exited") & (lead(location) == second_loc & lead(action) == "entered") ~ lead(date_time)-date_time,
        (location=="Home" & action == "entered") & (lag(location) == second_loc & lead(action) == "exited") ~ date_time-lag(date_time)
      )
    ) %>%
    summarise(
      travel_time = median(travel %>% as.numeric(), na.rm = TRUE)
    ) %>%
    pull() %>% 
    round() %>%
    return()
  
}

# Function to pad missing entry / exit records
pad_missing_entries = function(dt, second_loc = "Gym", mediant) {
  
  # For Gym or Work (XXX) - we can add some missing entries because I live very close to the gym.
    # Case 1: home (exit) -> XXX (exit) | there's a missing XXX entry right after leaving home
    # Case 2: XXX (enter) -> home (enter) | there's a missing XXX exit right before getting back home
    # Case 3: XXX (exit) -> home (exit) | there's a missing home enter right after XXX exit
    # Case 4: home (enter) -> XXX(enter) | there's a missing home exit right before the XXX entry
  missing_records = dt %>%
    arrange(date_time, desc(location)) %>%
    mutate(
      add_type = case_when(
        # Case 1
        (location=="Home" & action == "exited") & (lead(location) == second_loc & lead(action) == "exited") ~ "add_enter_loc",
        # Case 2
        (location=="Home" & action == "entered") & (lag(location) == second_loc & lag(action) == "entered") ~ "add_exit_loc",
        # Case 3
        (location== second_loc & action == "exited") & (lead(location) == "Home" & lead(action) == "exited") ~ "add_enter_home",
        # Case 4
        (location== second_loc & action == "entered") & (lag(location) == "Home" & lag(action) == "entered") ~ "add_exit_home"
      )
    ) %>%
    filter(!is.na(add_type)) %>%
    # Re-purpose these records to add the missing records
    mutate(
      action = case_when(
        add_type %>% str_detect("enter")  ~ "entered",
        add_type %>% str_detect("exit") ~ "exited"
      ),
      location = case_when(
        add_type %>% str_detect("loc") ~ second_loc,
        add_type %>% str_detect("home") ~ "Home"
      ),
      date_time = case_when(
        add_type %in% c("add_exit_loc", "add_exit_home") ~ date_time-seconds(mediant),
        add_type %in% c("add_enter_loc", "add_enter_home") ~ date_time+seconds(mediant)
      )
    ) %>%
    select(-add_type)
  
  # Pad original dataframe
  dt %>%
    union_all(missing_records) %>%
    return()
  
}


 
# Pad gym entries
median_gym_travel_time = get_median_travel(df, "Gym") # 240 seconds
df = pad_gym_entries(df, median_gym_travel_time)

# Pad work entries
median_work_travel_time = get_median_travel(df, "Work") # 1,920 seconds
df = pad_gym_entries(df, median_gym_travel_time)

# Checking if there exists a corresponding entry / exit entry for each action
df = df %>%
  group_by(location) %>%
  arrange(date_time) %>%
  mutate(
    comp_check = case_when(
      location == "Home" ~ TRUE,
      action == "entered" ~ ifelse(lead(action) == "exited", TRUE, FALSE),
      action == "exited" ~ ifelse(lead(action) == "entered", TRUE, FALSE),
      TRUE ~ TRUE
    )
  ) %>%
  group_by(date) %>%
  mutate(
    comp_check = all(comp_check)
  ) %>%
  ungroup() %>%
  filter(comp_check) %>%
  select(-comp_check)



