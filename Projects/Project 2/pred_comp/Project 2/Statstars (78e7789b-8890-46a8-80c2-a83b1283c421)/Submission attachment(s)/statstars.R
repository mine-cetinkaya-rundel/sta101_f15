download("https://stat.duke.edu/~mc301/data/movies.Rdata", destfile="movies.Rdata")
load("movies.Rdata")

#summary data:
plot(movies$audience_score ~ movies$critics_score) 
cor(movies$critics_score, movies$audience_score)
slr = lm(movies$audience_score ~ movies$critics_score)
summary(slr)
abline(lm(movies$audience_score ~ movies$critics_score))

#exploratory data analysis:
plot(movies$audience_score ~ movies$genre) 
plot(movies$audience_score ~ movies$runtime)
cor(movies$runtime, movies$audience_score)
plot(movies$audience_score ~ movies$critics_score) 
cor(movies$critics_score, movies$audience_score)

#hypothesis test:
a = movies$audience_score
b = movies$critics_score
t.test(a,b, paired=TRUE)

#"best" model:
m_full <- lm(audience_score ~ type + genre + runtime + year + mpaa_rating + imdb_num_votes + 
               critics_score + critics_rating + best_pic_nom + best_pic_win + 
               best_actor_win  + best_actress_win + best_dir_win + top200_box, data = movies)
mlr <- lm(audience_score ~ genre + type + year + runtime + mpaa_rating + imdb_num_votes + 
              critics_score + critics_rating + top200_box, data = movies)
hist(m_full$residuals)  
qqnorm(m_full$residuals)
qqline(m_full$residuals)
plot(m_full$residuals ~ m_full$fitted) 
plot(m_full$residuals) 

#prediction:
mlr <- lm(audience_score ~ genre + type + year + runtime + mpaa_rating + imdb_num_votes + 
                critics_score + critics_rating + top200_box, data = movies)
amsnip = data.frame(genre = "Drama", type = "Feature Film", year = 2015, runtime = 134, 
                    mpaa_rating = "R", imdb_num_votes = 168715, critics_score = 72, 
                    critics_rating = "Certified Fresh", top200_box = "yes")
predict(mlr, newdata = amsnip)
predict(mlr, newdata = amsnip, interval = "predict")

summary(mlr)

