library(pbapply)
batters <- read.csv("batter_gamelogs_2017_colheads.csv", stringsAsFactors = F)
batters_list <- split(batters_weather_df, batters$player_id)

keep_cols <- c("DFS.DK.", "PA", "BOP", "Mean_TemperatureF", "MeanDew_PointF", "Mean_Humidity", "Mean_VisibilityMiles", "Mean_Wind_SpeedMPH", "CloudCover", "WindDirDegrees")

nnet_cv <- function(df, dv, train_pct, k, seed_no){
  require(neuralnet)
  df <- df[,sapply(df, is.numeric)]
  keep_cols <- c("DFS.DK.", "PA", "BOP", "Mean_TemperatureF", "MeanDew_PointF", "Mean_Humidity", "Mean_VisibilityMiles", "Mean_Wind_SpeedMPH", "CloudCover", "WindDirDegrees")
  df <- df[,colnames(df) %in% keep_cols]
  df[is.na(df)] <- 0
  if(!all(sapply(df, is.numeric)) | nrow(df) < 20){
    return(NA)
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
  cv.error <- NULL
  test_pct <- 1-train_pct
  pb <- txtProgressBar(min = 0, max = k, initial = 0)
  
  for(i in 1:k){
    index <- 1:(floor(nrow(scaled))-i)
    train_ <- scaled[index,]
    n <- names(train_)
    test_ <- scaled[-index,]
    f <- as.formula(paste(sprintf("%s ~", dv), paste(n[!n %in% dv], collapse = " + ")))
    nn <- neuralnet(f, data=train_, hidden = hidden_layers, linear.output=T)
    
    pr.nn <- neuralnet::compute(nn, test_[,!colnames(test_) %in% dv])
    pr.nn <- pr.nn$net.result*(max(dv_col)-min(dv_col))+min(dv_col)
    test.r <- (test_[,!colnames(test_) %in% dv])*(max(dv_col)-min(dv_col))+min(dv_col)
    
    cv.error[i] <- sum((test.r - pr.nn)^2)/nrow(test_)
    
    setTxtProgressBar(pb,i)
  }
  return(cv.error)
}

# tmp <- nnet_cv(batters_list[[1]], dv = "DFS.DK.", train_pct = 0.9, k = 10, seed_no = 450)
tmp <- pbsapply(batters_list, function(x) mean(nnet_cv(df = x, dv = "DFS.DK.", train_pct = 1, k = 10, seed_no = 450)))
tmp[order(tmp)][1:100]

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

tmp_predict <- pbsapply(batters_list, function(x) mean(nnet_predict(df = x, dv = "DFS.DK.", seed_no = 350)))
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
