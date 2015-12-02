download("https://stat.duke.edu/~mc301/data/movies.Rdata", destfile="movies.Rdata")
load("movies.Rdata")

#### EDA: 

# Look at scatter plot of critics v audience scores 
plot(movies$audience_score ~ movies$critics_score)
criticaudience <- lm(audience_score ~ critics_score, data = movies)
summary(criticaudience)
summary(movies$critics_score)
summary(movies$audience_score)
hist(movies$audience_score)

# Create subsets for each genre
actionandadventure <-subset(movies, genre == "Action & Adventure")
comedy <- subset(movies, genre == "Comedy")
drama <- subset (movies, genre == "Drama")
horror <- subset(movies, genre == "Horror")
mysteryandsuspense <- subset(movies, genre == "Mystery & Suspense")
other <- subset(movies, genre == "Other")

# Univariate summary critics score in each genre
summary(actionandadventure$critics_score)
summary(comedy$critics_score)
summary(drama$critics_score)
summary(horror$critics_score)
summary(mysteryandsuspense$critics_score)
summary(other$critics_score)

# Univariate summary audience score in each genre
summary(actionandadventure$audience_score)
summary(comedy$audience_score)
summary(drama$audience_score)
summary(horror$audience_score)
summary(mysteryandsuspense$audience_score)
summary(other$audience_score)

#Bivariate linear models critics v audience scores in each genre
criticaudienceactionandadventure <- lm(audience_score ~ critics_score, data = actionandadventure)
criticaudiencecomedy <- lm(audience_score ~ critics_score, data = comedy)
criticaudiencedrama <- lm(audience_score ~ critics_score, data = drama)
criticaudiencehorror <- lm(audience_score ~ critics_score, data = horror)
criticaudiencemysteryandsuspense <- lm(audience_score ~ critics_score, data = mysteryandsuspense)
criticaudienceother <- lm(audience_score ~ critics_score, data = other)

#Bivariate scatter plots critics v audience scores in each genre
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
Action_and_Adventure <- add.alpha("black",.5)
Comedy <- add.alpha("red",.5)
Drama <- add.alpha("green",.5)
Horror <- add.alpha("blue", .5)
Mystery_and_Suspense <- add.alpha("darkorchid", .5)
Other <- add.alpha("gray48", .5)
plot(movies$audience_score,movies$critics_score,pch=19,col=movies$genre)
legend('bottomright',levels(movies$genre),col=c("black","red","green", "blue", "darkorchid", "gray48"),pch=19)
Action_and_Adventure <- lm(actionandadventure$audience_score ~ actionandadventure$critics_score)
abline(Action_and_Adventure)
Comedy <- lm(comedy$audience_score ~ comedy$critics_score)
abline(Comedy, col = "red")
Drama <- lm(drama$audience_score ~ drama$critics_score)
abline(Drama, col = "green")
Horror <- lm(horror$audience_score ~ horror$critics_score)
abline(Horror, col = "blue")
Mystery_and_Suspense <- lm(mysteryandsuspense$audience_score ~ mysteryandsuspense$critics_score)
abline(Mystery_and_Suspense, col = "darkorchid")
Other <- lm(other$audience_score ~ other$critics_score)
abline(Other, col = "gray48")

# Bivariate side-by-side boxplots of critics minus audience score
movies$diff = movies$critics_score - movies$audience_score
boxplot(movies$diff ~ movies$genre)

#### Inference: 

# Check conditions for each genre

# Action and Adventure

plot(criticaudienceactionandadventure$residuals ~ actionandadventure$critics_score)
abline(h=0, lty=3)
hist(criticaudienceactionandadventure$residuals)
qqnorm(criticaudienceactionandadventure$residuals)
qqline(criticaudienceactionandadventure$residuals)
plot(criticaudienceactionandadventure$residuals ~ criticaudienceactionandadventure$fitted)
plot(abs(criticaudienceactionandadventure$residuals) ~ criticaudienceactionandadventure$fitted)

# Comedy 

plot(criticaudiencecomedy$residuals ~ comedy$critics_score)
abline(h=0, lty=3)
hist(criticaudiencecomedy$residuals)
qqnorm(criticaudiencecomedy$residuals)
qqline(criticaudiencecomedy$residuals)
plot(criticaudiencecomedy$residuals ~ criticaudiencecomedy$fitted)
plot(abs(criticaudiencecomedy$residuals) ~ criticaudiencecomedy$fitted)

# Drama

plot(criticaudiencedrama$residuals ~ drama$critics_score)
abline(h=0, lty=3)
hist(criticaudiencedrama$residuals)
qqnorm(criticaudiencedrama$residuals)
qqline(criticaudiencedrama$residuals)
plot(criticaudiencedrama$residuals ~ criticaudiencedrama$fitted)
plot(abs(criticaudiencedrama$residuals) ~ criticaudiencedrama$fitted)

# Horror

plot(criticaudiencehorror$residuals ~ horror$critics_score)
abline(h=0, lty=3)
hist(criticaudiencehorror$residuals)
qqnorm(criticaudiencehorror$residuals)
qqline(criticaudiencehorror$residuals)
plot(criticaudiencehorror$residuals ~ criticaudiencehorror$fitted)
plot(abs(criticaudiencehorror$residuals) ~ criticaudiencehorror$fitted)

# Mystery and Suspense 

plot(criticaudiencemysteryandsuspense$residuals ~ mysteryandsuspense$critics_score)
abline(h=0, lty=3)
hist(criticaudiencemysteryandsuspense$residuals)
qqnorm(criticaudiencemysteryandsuspense$residuals)
qqline(criticaudiencemysteryandsuspense$residuals)
plot(criticaudiencemysteryandsuspense$residuals ~ criticaudiencemysteryandsuspense$fitted)
plot(abs(criticaudiencemysteryandsuspense$residuals) ~ criticaudiencemysteryandsuspense$fitted)

# Other

plot(criticaudienceother$residuals ~ other$critics_score)
abline(h=0, lty=3)
hist(criticaudienceother$residuals)
qqnorm(criticaudienceother$residuals)
qqline(criticaudienceother$residuals)
plot(criticaudienceother$residuals ~ criticaudienceother$fitted)
plot(abs(criticaudienceother$residuals) ~ criticaudienceother$fitted)

# Inference Action and Adventure 

summary(criticaudienceactionandadventure)

# Inference Comedy

summary(criticaudiencecomedy)

# Inference Drama

summary(criticaudiencedrama)

# Inference Horror 

summary(criticaudiencehorror)

# Inference Mystery and Suspense

summary(criticaudiencemysteryandsuspense)

# Inference Other

summary(criticaudienceother)

# Relative P-values are: 
# Action and adventure: 2.34 x 10-09
# Comedy: 1.26x10-05
# Drama: <2x10-16
# Horror: 0.00166
# Mystery and suspense: 1.81x10-11
# Other: 3.35x10-11

#### The "Best" Model: 

mlr = lm(audience_score ~ type + genre + runtime + year + mpaa_rating + critics_score + critics_rating + top200_box, data=movies)
hist(mlr$residuals)
qqnorm(mlr$residuals)
qqline(mlr$residuals)
plot(mlr$residuals ~ mlr$fitted)
abline(h=0, lty=3)
plot(abs(mlr$residuals) ~ mlr$fitted)
summary(mlr)

#### Prediction: 

cinderella = data.frame("title" = "Cinderella", "audience_score" = 84, "type" = "Feature Film", "genre" = "Drama", "runtime" = 105, "year" = 2015, "mpaa_rating" = "PG", "critics_score" = 85, "critics_rating" = "Certified Fresh", "top200_box" = "yes")
predict(mlr, cinderella)
predict(mlr, cinderella, interval = "prediction")