library(plyr);library(dplyr);library(stringr);library(rvest);library(lubridate);library(readr);library(tidyr);library(xts);library(dygraphs);library(lazyeval);library(RColorBrewer);library(shiny);library(shinythemes);library(weatherData)
server <- function(input, output){
     batters <- read.csv("batter_gamelogs_2017_colheads.csv", stringsAsFactors = F)
     pitchers <- read.csv("pitcher_gamelogs_2017_colheads.csv", stringsAsFactors = F)
     player_info <- read.csv("player_info_clean.csv", stringsAsFactors = F)
     baseball_cities <- read_csv("abr_to_city.csv")
     data("USAirportWeatherStations")

     opening_day_week = week(mdy("4/02/2017")) - 1 # need the url for downloading and then change it to 2017
     opening_day <- mdy("04/02/2017")
     batters$game_date <- ymd(batters$game_date)
     
     ####
     #GET ALL FUNCTIONS
     ####
     
     #EXTRACT DRAFT KINGS DAILY INFO INTO A DATAFRAME
     get_dk_foo <- function(link){
          dk_daily_df <- read.csv(link, header=T, stringsAsFactors = F) %>% 
               separate(GameInfo, into = c("everythingelse", "game_time"), sep = " ", remove = T) %>% 
               separate(everythingelse, into = c("away", "home"), sep = "@", remove = T) %>% 
               mutate(away = toupper(away)) %>% 
               mutate(home = toupper(home)) %>% 
               mutate(teamAbbrev = toupper(teamAbbrev)) %>% 
               mutate(Position = str_replace(str_extract(Position, "^.{1,2}"), pattern = "/", "")) %>% 
               mutate(Opp = ifelse(teamAbbrev == home, away, home))
     }
     
     
     player_compare <- function(player_list, baseball_stat, compare_type = "season"){
          stopifnot(is.character(player_list))
          
          freq_stats <- c("PA", "AB", "R", "H", "2B", "3B", "HR", "RBI", "BB", "IBB", "SO", "HBP", "SH", "SF", "ROE", "GDP", "SB", "CS", "BOP", "DFS.DK.", "DFS.FD.")
          ratio_stats <- c("BA", "OBP", "SLG", "OPS")
          
          if(baseball_stat %in% ratio_stats){
               if(compare_type == "season"){
                    tmp <- batters %>% 
                         left_join(select(player_info, player_id, player_name), by = "player_id") %>% 
                         filter(toupper(player_name) %in%  toupper(player_list)) %>% 
                         group_by(player_id) %>% 
                         mutate(cnt = n()) %>% 
                         mutate_(metric = interp(~cummean(x), x = as.name(baseball_stat))) %>%
                         filter(!is.na(metric)) %>% 
                         ungroup() %>% 
                         select(player_name, game_date, metric)
               }
               else{
                    tmp <- batters %>% 
                         left_join(select(player_info, player_id, player_name), by = "player_id") %>% 
                         filter(toupper(player_name) %in%  toupper(player_list)) %>% 
                         rowwise() %>% 
                         mutate(time_period = ifelse(compare_type == "weekly", week(game_date), day(game_date))) %>% 
                         ungroup() %>% 
                         group_by(player_id, time_period) %>%
                         mutate(metric = ifelse(baseball_stat == "BA", sum(H)/sum(AB),
                                                ifelse(baseball_stat == "OBP", (sum(H)+sum(BB)+sum(HBP))/sum(PA),
                                                       ifelse(baseball_stat == "SLG", sum(TB) / sum(PA),
                                                              ifelse(baseball_stat == "OPS", ((sum(H)+sum(BB)+sum(HBP))/sum(PA)) + (sum(TB) / sum(PA)), 0))))) %>% 
                         mutate(cnt = n()) %>% 
                         filter(!is.na(metric)) %>% 
                         ungroup() %>% 
                         select(player_name, game_date, metric)
               }
          }
          if(baseball_stat %in% freq_stats){
               if(compare_type == "season"){
                    tmp <- batters %>% 
                         left_join(select(player_info, player_id, player_name), by = "player_id") %>% 
                         filter(toupper(player_name) %in%  toupper(player_list)) %>% 
                         group_by(player_id) %>% 
                         mutate(cnt = n()) %>% 
                         mutate_(metric = interp(~cumsum(x), x = as.name(baseball_stat))) %>%
                         filter(!is.na(metric)) %>% 
                         ungroup() %>% 
                         select(player_name, game_date, metric)
               }
               else{
                    tmp <- batters %>% 
                         left_join(select(player_info, player_id, player_name), by = "player_id") %>% 
                         filter(toupper(player_name) %in%  toupper(player_list)) %>% 
                         rowwise() %>% 
                         mutate(time_period = ifelse(compare_type == "weekly", week(game_date), game_date)) %>% 
                         ungroup() %>% 
                         group_by(player_id, time_period) %>% 
                         mutate(cnt = n()) %>% 
                         mutate_(metric = interp(~cumsum(x), x = as.name(baseball_stat))) %>%
                         filter(!is.na(metric)) %>% 
                         ungroup() %>% 
                         select(player_name, game_date, metric)
               }
               
          }
          colnames(tmp) <- c("player_name", "game_date", baseball_stat)
          
          freq_type = ifelse(compare_type == "weekly", 52, 365)
          tmp_ts <- lapply(split(tmp, tmp$player_name), function(x) xts(x[[baseball_stat]], order.by = x$game_date, frequency = freq_type))
          names(tmp_ts) <- unique(tmp$player_name)
          tmp_ts_df <- do.call(cbind, tmp_ts)
          colnames(tmp_ts_df) <- str_replace_all(colnames(tmp_ts_df), "[.]", " ")
          
          dygraph(tmp_ts_df[,1:ncol(tmp_ts_df)], main = paste("Comparison of",baseball_stat," for ", paste(player_list, collapse = " & "))) %>% 
               dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"), stepPlot = F, axisLineWidth = 2, axisLabelFontSize = 12, axisLineColor = "green") %>% 
               dyHighlight(highlightCircleSize = 5, 
                           highlightSeriesBackgroundAlpha = 0.8,
                           hideOnMouseOut = F) %>% 
               dyAxis("y", label = baseball_stat, drawGrid = F) %>% 
               dyLegend(show = "follow", hideOnMouseOut = F) %>% 
               dyRangeSelector()
          
     }
     
     # Name Match Function
     jaccard_sim <- function(a, b){
          a_split = unlist(strsplit(tolower(a), NULL))
          b_split = unlist(strsplit(tolower(b), NULL))
          a_pairs = paste0(a_split, a_split[-1])[-length(a_split)]
          b_pairs = paste0(b_split, b_split[-1])[-length(b_split)]
          numerator = length(intersect(a_pairs, b_pairs))
          denominator = length(union(a_pairs, b_pairs))    
          return(numerator/denominator)
     }
     
     jaccard_best_match <- function(a, b = player_info$player_name){
          scores = sapply(b, function(x) jaccard_sim(a = a, b = x))
          return(as.character(b[which.max(scores)]))
     }
     
     
     #OPTIMIZE DRAFT KINGS LINEUP
     get_dk_lineup <- function(data, salary_constraint = 50000, n_lineups = 10){
          require(lpSolve)
          require(stringr)
          require(dummies)
          require(plyr)
          require(dplyr)
          f_obj <- data$proj
          Position_matrix <- dummy(data$Position)
          colnames(Position_matrix) = c("1B", "2B", "3B", "C", "OF", "P", "SS")
          f_constraints <- t(cbind(Salary = data$Salary, Position_matrix))
          colnames(f_constraints) <- toupper(data$Name)
          f_dir <- rep(0, nrow(f_constraints)); f_dir[1] <- '<='
          f_rhs <- rep(0, nrow(f_constraints)); f_rhs[1] <- salary_constraint
          f_dir[2:nrow(f_constraints)] <- c("=", "=", "=", "=", "=", "=", "=", "=")
          f_rhs[2:nrow(f_constraints)] <- c(1,1,1,1,3,2,1,10)
          opt <- lp("max", objective.in = f_obj, const.mat = f_constraints, const.dir = f_dir, const.rhs = f_rhs, all.bin = T,num.bin.solns = 1)
          picks <- data[which(opt$solution==1),] %>% 
               arrange(desc(Salary))
          return(picks)
     }
     get_starters <- function(total_games){
          dailylineup_link <- "http://www.rotowire.com/baseball/daily_lineups.htm"
          dailylineup_page <- read_html(dailylineup_link)
          starter_list <- c()
          for(i in 1:total_games){
               for(j in 1:10){
                    dailylineup_df <- dailylineup_page %>% 
                         html_nodes(css = sprintf("body > div:nth-child(3) > div:nth-child(%d) > div:nth-child(%d) > div.span15.dlineups-mainbox", j,i)) %>% 
                         html_text()
                    player_regex <- paste0("(", paste0(player_info$player_name, collapse = "|"), ")")
                    starter_list <- append(starter_list, unlist(str_extract_all(string = dailylineup_df, pattern = player_regex)))
               }
               
          }
          return(unique(starter_list))
     }
     
     #Tailor batter projections based on the pitcher they are expected to face that day.
     get_player_projection <- function(playersname, tmp2){
          cat(playersname); cat(' ')
          get_player_id <- function(playername){
               return(unlist(filter(player_info, player_name == playername) %>% select(player_id)))
          }
          id = get_player_id(playersname)
          pitcher_vs_batter_link <- sprintf("http://www.baseball-reference.com/play-index/batter_vs_pitcher.cgi?batter=%s", id)
          pitcher_vs_batter_page <- try(read_html(pitcher_vs_batter_link))
          
          pitcher_vs_batter_df <- pitcher_vs_batter_page %>% 
               html_nodes(xpath = '//*[@id="ajax_result_table"]') %>% 
               html_table() %>% 
               as.data.frame %>%
               filter(Name != "Name")
          for(i in 2:ncol(pitcher_vs_batter_df)){
               pitcher_vs_batter_df[,i] <- as.numeric(pitcher_vs_batter_df[,i])
          }
          batter_pitcher_stats <- pitcher_vs_batter_df %>% 
               filter(PA >= 10) %>% 
               mutate(h_avg = H/sum(H)) %>% 
               mutate(hr_avg = HR/sum(HR)) %>% 
               mutate(rbi_avg = RBI/sum(RBI)) %>% 
               mutate(hbp_avg = HBP/sum(HBP)) %>%
               select(h_avg, hr_avg, rbi_avg, hbp_avg)
          if(nrow(batter_pitcher_stats) == 0){
               batter_today <- tmp2 %>% 
                    inner_join(filter(player_info, player_id == id) %>% select(player_name), by = c("Name" = "player_name")) %>% 
                    mutate(proj = AvgPointsPerGame)
               return(batter_today$proj)
          }
          batter_pitcher_stats[batter_pitcher_stats > .1]  <- .1
          batter_pitcher_stats$Name <- unlist(filter(pitcher_vs_batter_df, PA >= 10) %>% select(Name))
          
          batter_today <- tmp2 %>% 
               inner_join(filter(player_info, player_id == id) %>% select(player_name), by = c("Name" = "player_name"))
          pitcher_today <- batter_pitcher_stats %>% 
               inner_join(tmp2, by = "Name") %>% 
               filter(Opp == batter_today$teamAbbrev) %>% 
               select(h_avg, hr_avg, rbi_avg, hbp_avg)
          
          batter_today$proj <- ifelse(nrow(pitcher_today) == 1, batter_today$AvgPointsPerGame + sum(batter_today$AvgPointsPerGame*pitcher_today), batter_today$AvgPointsPerGame)
          
          return(batter_today$proj)
     }
     
     #Get each batter's normalized variance in fantasy points scored with respect to their position     
     league_pos_dfs_df <- batters %>%
          select(Pos, game_date, `DFS.DK.`, `DFS.FD.`) %>% 
          mutate(Pos = str_extract(Pos, pattern = ".{1,2}")) %>%
          mutate(Pos = str_replace_all(Pos, "(LF)|(CF)|(RF)", "OF")) %>% 
          filter(Pos %in% c("C", "1B", "2B", "3B", "SS", "OF")) %>%
          group_by(Pos, game_date) %>% 
          summarise(league_dk_sum = sum(`DFS.DK.`, na.rm=T), league_dk_sdd = sd(`DFS.DK.`, na.rm=T), league_fd_sum = sum(`DFS.FD.`, na.rm=T), league_fd_sdd = sd(`DFS.FD.`, na.rm=T), league_dk_sdd = sd(`DFS.FD.`, na.rm=T), GP = n()) %>% 
          group_by(Pos) %>% 
          mutate(league_dk_avg = cumsum(league_dk_sum) / cumsum(GP)) %>% 
          mutate(league_dk_sd = cummean(league_dk_sdd)) %>%
          mutate(league_fd_avg = cumsum(league_fd_sum) / cumsum(GP)) %>% 
          mutate(league_fd_sd = cummean(league_fd_sdd)) %>% 
          ungroup() %>% 
          select(Pos, game_date, league_dk_avg, league_dk_sd, league_dk_sd, league_fd_avg, league_fd_sd)
     
     batter_risk_df <- batters %>%
          select(player_id, game_date, Pos,`DFS.DK.`, `DFS.FD.`) %>% 
          mutate(Pos = str_extract(Pos, pattern = ".{1,2}")) %>%
          mutate(Pos = str_replace_all(Pos, "(LF)|(CF)|(RF)", "OF")) %>% 
          filter(Pos %in% c("C", "1B", "2B", "3B", "SS", "OF")) %>%
          group_by(player_id, Pos) %>% 
          mutate(player_dk_avg = cummean(`DFS.DK.`)) %>% 
          mutate(player_fd_avg = cummean(`DFS.FD.`)) %>% 
          left_join(league_pos_dfs_df, by = c("Pos" = "Pos", "game_date" = "game_date")) %>% 
          mutate(player_dk_scale = (player_dk_avg - league_dk_avg) / (league_dk_sd)) %>% 
          mutate(player_dk_scale_pnorm = pnorm(player_dk_scale)) %>% 
          mutate(player_fd_scale = (player_fd_avg - league_fd_avg) / (league_fd_sd)) %>% 
          mutate(player_fd_scale_pnorm = pnorm(player_fd_scale)) %>% 
          left_join(select(player_info, player_id, player_name), by = "player_id") %>% 
          group_by(player_id) %>% 
          filter(game_date == max(game_date))
     
     ##Predictions using a Neural Network
     nnet_predict <- function(df, dv, seed_no){
       require(neuralnet)
       for(i in 1:1){
         df <- df[,sapply(df, is.numeric)]
         keep_cols <- c("DFS.DK.", "PA", "BOP", "Mean_TemperatureF", "MeanDew_PointF", "Mean_Humidity", "Mean_VisibilityMiles", "Mean_Wind_SpeedMPH", "CloudCover", "WindDirDegrees")
         df <- df[,colnames(df) %in% keep_cols]
         df[is.na(df)] <- 0
         if(!all(sapply(df, is.numeric)) | nrow(df) < 20){
           break
         }
         set.seed(seed_no)
         #scale the data
         dv_col <- df[,colnames(df) %in% dv]
         
         input_layers <- sum(colnames(df) != dv)
         hidden_layers <- (floor(input_layers*(2/3))/2) + c(-1, 1)
         maxs <- apply(df, 2, max) 
         mins <- apply(df, 2, min)
         scaled <- as.data.frame(scale(df, center = mins, scale = maxs - mins))
         scaled[is.na(scaled)] <- 0
         
         index <- 1:(nrow(scaled)-1)
         train_ <- scaled[index,]
         n <- names(train_)
         test_ <- scaled[-index,]
         f <- as.formula(paste(sprintf("%s ~", dv), paste(n[!n %in% dv], collapse = " + ")))
         nn <- neuralnet(f, data=train_, hidden = hidden_layers, linear.output=T)
         
         pr.nn <- neuralnet::compute(nn, test_[,!colnames(test_) %in% dv])
         pr.nn <- pr.nn$net.result*(max(dv_col)-min(dv_col))+min(dv_col)
         return_me <- pr.nn
         
         return(return_me)
       }
     }
     
     
     ####
     #BEGIN SHINY OUTPUT
     ####
     output$PlatformChosen <- renderText({
          paste("You chose", input$'Dominant Platforms')
     })
     
     output$Optimized2 <- renderTable({
          risk_tolerance_df <- data.frame()
          if(input$'risk_tol' == "Low Risk"){
               risk_tolerance_df <- filter(batter_risk_df, player_dk_scale_pnorm <= .70 &  player_dk_scale_pnorm >= .35)
          }
          else if(input$'risk_tol' == "Medium Risk"){
               risk_tolerance_df <- filter(batter_risk_df, player_dk_scale_pnorm <= .80 & player_dk_scale_pnorm >= .25)
          }
          else{
               risk_tolerance_df <- batter_risk_df
          }
          oppo_not <- unlist(strsplit(input$'oppo_not', split = ", "))
          oppo_not <- as.list(oppo_not)
          
          tmp <- get_dk_foo(input$'dk_file')
          tmp$Position[tmp$Position == "SP" | tmp$Position == "RP"] <- "P"
          tmp$proj <- tmp$AvgPointsPerGame
          
          batters_pa <- batters %>% 
               group_by(player_id) %>% 
               summarise(PA_tot = sum(PA)) %>% 
               inner_join(select(player_info, player_id, player_name), by = "player_id") %>% 
               select(player_name, PA_tot) %>% 
               filter(PA_tot >= input$"min_pa")
          
          if(input$is_live == T){
            # when live...
            players_starting <- get_starters(14)
            tmp2 <- filter(tmp, toupper(Name) %in% toupper(players_starting)) %>%
                 filter(Position == "P" | (toupper(Name) %in% toupper(batters_pa$player_name)))
          }
          else{
            tmp2 <- tmp %>% 
              filter(Position == "P" | (toupper(Name) %in% toupper(batters_pa$player_name)))
          }
          
          if(input$is_weather == T){
            output$please_wait <- renderText({
              "Please wait a few minutes while we extract the most current weather information..."
            })
            
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
            
            batters_list <- split(batters_weather_df, batters_weather_df$player_id)
            
            tmp_predict <- sapply(batters_list, function(x) mean(nnet_predict(df = x, dv = "DFS.DK.", seed_no = 350)))
            prediction_df <- data.frame()
            for(i in 1:length(batters_list)){
              a <- cbind.data.frame(batters_list[[i]][nrow(batters_list[[i]]),], proj = tmp_predict[i])
              prediction_df <- rbind.data.frame(prediction_df, a)
            }
            
            prediction_df <- prediction_df %>% 
              filter(abs(DFS.DK. - proj) < 6) %>% 
              select(player_id, proj) %>% 
              inner_join(select(player_info, player_id, player_name), by = "player_id") %>% 
              select(player_name, proj)
            
            a <- tmp %>%
              filter(Position == "P" | (toupper(Name) %in% toupper(batters_pa$player_name))) %>% 
              anti_join(select(prediction_df, player_name), by = c("Name" = "player_name"))
            
            b <- tmp %>% 
              filter(Position == "P" | (toupper(Name) %in% toupper(batters_pa$player_name))) %>% 
              select(-proj) %>% 
              inner_join(prediction_df, by = c("Name" = "player_name"))
            
            tmp2 <- rbind.data.frame(a,b)
          }
          
          if(input$"use_pitch_bat" == T){
               tmp2$proj <- sapply(tmp2$Name, function(q) get_player_projection(playersname = q, tmp2 = tmp2))
          }
          
          tmp3 <- tmp2 %>% 
               filter(!Opp %in% oppo_not) %>% 
               filter(toupper(Name) %in% unique(toupper(risk_tolerance_df$player_name)))
          
          if(length(unique(tmp3$Position)) != 7){
            tmp3 <- tmp2
          }
          optimizedtable2 <- get_dk_lineup(tmp3)
          
          if(nrow(optimizedtable2) == 0){
               tmp3 <- tmp2 %>% 
                    filter(!Opp %in% oppo_not)
               optimizedtable2 <- get_dk_lineup(tmp3)
          }
          select(optimizedtable2, -AvgPointsPerGame, -teamAbbrev)
     })
     output$right_name <- renderText({
          players_name1 <- unlist(strsplit(input$'players_name', split = ", "))
          players_name_match1 <- sapply(players_name1, jaccard_best_match)
          paste("You may mean: ", players_name_match1)
     })
     output$Compare1 <- renderDygraph({
          players_name <- unlist(strsplit(input$'players_name', split = ", "))
          baseball_stat <- unlist(strsplit(input$'baseball_stat', split = ", "))
          players_name_match <- sapply(players_name, jaccard_best_match)
          plot_1 <- player_compare(players_name_match, baseball_stat, compare_type = input$'compare_type')
          plot_1
     })
}