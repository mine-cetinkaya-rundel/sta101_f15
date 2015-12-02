# added by mcr
download("https://stat.duke.edu/~mc301/data/movies.Rdata", destfile="movies.Rdata")
download("https://stat.duke.edu/~mc301/R/inference.RData", destfile = "inference.RData")
load("inference.RData")
###
load("movies.RData")

boxplot(movies$audience_score ~ movies$mpaa_rating)
boxplot(movies$audience_score ~ movies$year)
mosaicplot(movies$genre ~ movies$mpaa_rating)
plot(movies$mpaa_rating ~ movies$year)
boxplot(movies$runtime ~ movies$mpaa_rating)
boxplot(movies$critics_score ~ movies$mpaa_rating)

inference(y = movies$audience_score, x = movies$mpaa_rating, est = "mean", type = "ht", null = 0, alternative = "greater", method = "theoretical")

# ... if you do anything prior to running the regression ...

lm(audience_score ~ type + genre + runtime + year + mpaa_rating + imdb_num_votes + critics_score + critics_rating + best_pic_nom + best_pic_win + best_actor_win + best_actress_win + best_dir_win + top200_box, data = movies)
# drop best_dir_win

mlr = lm(audience_score ~ type + genre + runtime + year + mpaa_rating + imdb_num_votes + critics_score + critics_rating + best_pic_nom + best_pic_win + best_actor_win + best_actress_win + top200_box, data = movies)
#drop best_pic_nom

mlr = lm(audience_score ~ type + genre + runtime + year + mpaa_rating + imdb_num_votes + critics_score + critics_rating + best_pic_win + best_actor_win + best_actress_win + top200_box, data = movies)
#drop best_actor_win

mlr = lm(audience_score ~ type + genre + runtime + year + mpaa_rating + imdb_num_votes + critics_score + critics_rating + best_pic_win + best_actress_win + top200_box, data = movies)
#drop best_actress_win

mlr = lm(audience_score ~ type + genre + runtime + year + mpaa_rating + imdb_num_votes + critics_score + best_pic_win + top200_box, data = movies)
#drop best_pic_win

mlr = lm(audience_score ~ type + genre + runtime + year + mpaa_rating + imdb_num_votes + critics_score + top200_box, data = movies)
#drop mpaa_rating

mlr = lm(audience_score ~ type + genre + runtime + year + imdb_num_votes + critics_score + top200_box, data = movies)
#drop top200_box

mlr = lm(audience_score ~ type + genre + runtime + year + imdb_num_votes + critics_score, data = movies)
# drop year

mlr = lm(audience_score ~ type + genre + runtime + imdb_num_votes + critics_score, data = movies)
#don't drop!

mlr = lm(audience_score ~ type + genre + runtime + imdb_num_votes + critics_score, data = movies)

summary(mlr)
plot(mlr$residuals ~ movies$audience_score)
abline(h = 0, lty = 3)
hist(mlr$residuals)
qqnorm(mlr$residuals)
qqline(mlr$residuals)
# END YOUR CODE