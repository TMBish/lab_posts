library(tidyverse)
library(googlesheets)
library(lubridate)

# Get Data ----------------------------------------------------------------

# Extract the google sheets keys from the urls
work_sheet_key = extract_key_from_url("https://docs.google.com/spreadsheets/d/1E80AoNSK2m_rfLWsL4qr41-3mP7pUEeuy3CG7J3--zs/edit#gid=0")
gymhome_sheet_key = extract_key_from_url("https://docs.google.com/spreadsheets/d/1dJWyirpVY7eq7WPdMlnEvvVIH957sEYMLYWK6e_MJCs/edit#gid=0")

# Read in data
prep_gsheets = function(key) {
  df = key %>%
    gs_key() %>%
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

# Map prep gsheets across the keys and combine tables
df = c(work_sheet_key, gymhome_sheet_key) %>%
  map(prep_gsheets) %>%
  bind_rows()


# Restructure ----------------------------------------------------------------

# Remove the first row
df = df[-1,]

# Add cols
df = df %>% 
  mutate(
    date = as_date(date_time),
    wday = wday(date),
    hour = hour(date_time)
  )


# Give each event an ID and spread entry / exit time
df %>%
  group_by(action) %>%
  mutate(event_id = dense_rank(date_time)) %>%
  ungroup() %>%
  spread(action, date_time)
  

