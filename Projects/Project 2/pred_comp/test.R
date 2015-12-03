# load packages -----------------------------------------------------

library(stringr)
library(downloader)
library(dplyr)

# course setup ------------------------------------------------------

team_dirs <- dir("project2_submissions/anthea/", full.names = TRUE)

n_team <- length(team_dirs)

teams <- dir("project2_submissions/anthea/") %>%
        str_extract(".*\\(") %>%
        str_replace(" \\(", "")

rmse_team <- data.frame(team = teams, rmse = rep(NA, n_team))

# load new prediction data ------------------------------------------

load("movies_pred.RData")

movies_pred = droplevels(movies_pred[movies_pred$mpaa_rating != "NC-17",])

#movies_pred$log_imdb_num_votes = log(movies_pred$imdb_num_votes)

for(i in 1:n_team){
  print(i)
  cur_dir = file.path(team_dirs[i], "Submission attachment(s)")
  print(cur_dir)
  R_files = dir(cur_dir,"\\.R$",full.names = TRUE)
  load("../data/movies.Rdata")
  load("inference.RData")
  
  if(length(R_files) > 1){stop(print(i))}
  
  try( source(R_files) )
  if(exists("mlr")){
    pred = predict(mlr, newdata = movies_pred)
    diff_sq = (pred - movies_pred$audience_score)^2
    n = length(diff_sq)
    sum_diff_sq = sum(diff_sq)
    rmse_team[i, 2] = sqrt(sum_diff_sq / n)
  } else {
    rmse_team[i, 2] = NA
  }
  to_remove = setdiff(ls(), c("movies_pred", "n_team", "rmse_team", "team_dirs"))
  rm(list=to_remove)
}

rmse_team[which.min(rmse_team$rmse),]

rmse_team[order(rmse_team$rmse),]
