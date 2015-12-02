#### Load Data
download("https://stat.duke.edu/~mc301/data/movies.Rdata", destfile="movies.Rdata")
load("movies.Rdata")

#### Comparison of Genre and Audience Score
plot(movies$genre, movies$audience_score)

#### Comparison of Genre and Movie Runtime
plot(movies$genre, movies$runtime)

#### Summary of Movies’ Genre and Runtime
summary(movies$genre, movies$runtime)

#### Relationship Between Action Movies’ Runtime and Audience Score
action <- subset(movies, genre == "Action & Adventure")

#### Linear Model
action_lm <- lm(action$audience_score ~ action$runtime)
summary(action_lm)

#### Conditions for Linear Model
plot(action_lm$residuals ~ action$runtime)
plot(action_lm$residuals)
qqnorm(action_lm$residuals)
hist(action_lm$residuals)


#### Abline for Action
plot(action$runtime, action$audience_score)
abline(action_lm)
cor(action$runtime, action$audience_score)

#### Relationship Between Comedy Movies’ Runtime and Audience Score
comedy <- subset(movies, genre == "Comedy")


#### Linear Model
comedy_lm <- lm(comedy$audience_score ~ comedy$runtime)
summary(comedy_lm)


#### Conditions for Linear Model
plot(comedy_lm$residuals ~ comedy$runtime)
plot(comedy_lm$residuals)
qqnorm(comedy_lm$residuals)
hist(comedy_lm$residuals)

#### Abline for Comedy
plot(comedy$runtime, comedy$audience_score)
abline(comedy_lm)
cor(comedy$runtime, comedy$audience_score)

#### Relationship Between Drama Movies’ Runtime and Audience Score
drama <- subset(movies, genre == "Drama")

#### Linear Model
drama_lm <- lm(drama$audience_score ~ drama$runtime)
summary(drama_lm)


#### Conditions for Linear Model
plot(drama_lm$residuals ~ drama$runtime)
plot(drama_lm$residuals)
qqnorm(drama_lm$residuals)
hist(drama_lm$residuals)

#### Abline for Drama
plot(drama$runtime, drama$audience_score)
abline(drama_lm)
cor(drama$runtime, drama$audience_score)


#### Relationship Between Mystery Movies’ Runtime and Audience Score
mystery <- subset(movies, genre == "Mystery & Suspense")

#### Linear Model
mystery_lm <- lm(mystery$audience_score ~ mystery$runtime)
summary(mystery_lm)

#### Abline for Mystery
plot(mystery$runtime, mystery$audience_score)
abline(mystery_lm)
cor(mystery$runtime, mystery$audience_score)

#### Conditions for Linear Model
plot(mystery_lm$residuals ~ mystery$runtime)
plot(mystery_lm$residuals)
qqnorm(mystery_lm$residuals)
hist(mystery_lm$residuals)

#### Relationship Between Horror Movies’ Runtime and Audience Score
horror <- subset(movies, genre == "Horror")


#### Linear Model
horror_lm <- lm(horror$audience_score ~ horror$runtime)
summary(horror_lm)

#### Conditions for Linear Model
plot(horror_lm$residuals ~ horror$runtime)
plot(horror_lm$residuals)
qqnorm(horror_lm$residuals)
hist(horror_lm$residuals)

#### Abline for Horror
plot(horror$runtime, horror$audience_score)
abline(horror_lm)
cor(horror$runtime, horror$audience_score)


#### Relationship Between “Other” Movies’ Runtime and Audience Score
other <- subset(movies, genre == "Other")


#### Linear Model
other_lm <- lm(other$audience_score ~ other$runtime)
summary(other_lm)

#### Conditions for Linear Model
plot(other_lm$residuals ~ other$runtime)
plot(other_lm$residuals)
qqnorm(other_lm$residuals)
hist(other_lm$residuals)

#### Abline for Other
plot(other$runtime, other$audience_score)
abline(other_lm)
cor(other$runtime, other$audience_score)


#### Interaction Between Runtime and Genre in Multiple Linear Regression
mlr_interaction <- lm(audience_score ~ genre*runtime, data = movies)
summary(mlr_interaction)

#### Starting Model for Backwards Elimination
mlr_start <- lm(audience_score ~ genre*runtime + genre + runtime + critics_score + critics_rating + top200_box + type + year + mpaa_rating + imdb_num_votes + best_pic_nom + best_pic_win + best_actor_win + best_actress_win + best_dir_win, data = movies)

#### Final Model
mlr <- lm(audience_score ~ genre*runtime + genre + runtime + critics_score + critics_rating + top200_box + type + year + mpaa_rating + imdb_num_votes + best_pic_win, data = movies)
summary(mlr)

#### Creating New Data Point for Lazarus Effect Movie
newdata = data.frame(genre = "Horror", runtime = 83, critics_score = 14, critics_rating= "Rotten", top200_box = "no", type = "Feature Film", year = 2015, mpaa_rating = "PG-13", imdb_num_votes = 3872, best_pic_win = "no")

#### Prediction Interval
predict(mlr, newdata, interval = "prediction")


