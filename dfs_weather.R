library(weatherData);library(pbapply)
data("USAirportWeatherStations")
baseball_cities <- read_csv("abr_to_city.csv")
Stations_df <- USAirportWeatherStations %>% 
  inner_join(baseball_cities, by = c("Station" = "Station", "State" = "State"))


#Get Weather for all baseball cities
weather_df <- pblapply(Stations_df$airportCode, function(x) getWeatherForDate(station_id = x, start_date = mdy("04/02/2017"), end_date = Sys.Date(), station_type = "airportCode", opt_all_columns = T, opt_verbose = F)[,-2])
names(weather_df) <- Stations_df$Opp
weather_df <- do.call(rbind, weather_df)
weather_df$team <- rownames(weather_df)
weather_df$team <- str_extract(toupper(weather_df$team), pattern = "^[A-Z]+")
rownames(weather_df) <- NULL
weather_df$Date <- ymd(weather_df$Date)
remove_cols <- !str_detect(colnames(weather_df), "(Max|Min|Events)")
weather_df <- weather_df[,remove_cols]

batters_weather_df <- batters %>% 
  mutate(station_to_take = ifelse(is.na(home_away), Tm, Opp), game_date = ymd(game_date)) %>% 
  left_join(weather_df, by = c("station_to_take" = "team", "game_date" = "Date"))