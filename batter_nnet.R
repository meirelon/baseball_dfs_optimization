library(pbapply)
batters <- read.csv("batter_gamelogs_2017_colheads.csv", stringsAsFactors = F)
batters_list <- split(batters, batters$player_id)

nnet_cv <- function(df, dv, train_pct, k, seed_no){
  require(neuralnet)
  df <- df[,sapply(df, is.numeric)]
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
