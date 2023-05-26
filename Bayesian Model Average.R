library(ggplot2)
library(dplyr)
library(statsr)
library(BAS)
library(psych)

#1 load data
movies<-get(load("movies.Rdata"))


#2 Data Manipulation
#mutate creating new variables from existing datasets (add 5 couloms yes or no )
data_movies<-movies %>% mutate(feature_film = factor(ifelse(title_type=="Feature Film",c("yes"), c("no")),levels=c("no","yes"))) %>%
  mutate(drama = factor(ifelse(genre=="Drama", c("yes"), c("no")),levels=c("no","yes")))%>%
  mutate(mpaa_rating_R =factor(ifelse(mpaa_rating=="R", c("yes"), c("no")),levels=c("no","yes")))%>%
  mutate(oscar_season = factor(ifelse(thtr_rel_month%in% 10:12, c("yes"), c("no")),levels=c("no","yes")))%>%
  mutate(summer_season = factor(ifelse(thtr_rel_month %in% 5:8, c("yes"), c("no")),levels=c("no","yes")))



#3 EDA
describe(data_movies$audience_score)

summary(data_movies$feature_film)
summary(data_movies$drama)
summary(data_movies$oscar_season)
summary(data_movies$summer_season)

#agg 4 couloms we made with audiance score 
aggr1<-aggregate(audience_score ~ feature_film, data_movies,mean)

aggr2<-aggregate(audience_score ~ drama, data_movies,mean)
aggr2

aggr3<-aggregate(audience_score ~ oscar_season, data_movies,mean)
aggr3

aggr4<-aggregate(audience_score ~ summer_season, data_movies,mean)
aggr4




#We see that the average audience score for feature film is lower than other type films
##and the credible interval proves the difference of means.
aggr1
boxplot(audience_score~feature_film,data=data_movies)
bayes_inference(y = audience_score, x = feature_film, mu_0=0 , data = data_movies, statistic = "mean", type = "ci", method="theoretical")
## Post. mean   = 20.5



#We see that average score for drama film is higher than other genre films
aggr2
boxplot(audience_score~drama,data=data_movies)
bayes_inference(y = audience_score, x = drama, data = data_movies, statistic = "mean", type = "ci",mu_0 = 0, method = "theoretical")


#We see that there is very small difference in the average audience score for films released during the oscar season
aggr3
boxplot(audience_score~oscar_season,data=data_movies)
bayes_inference(y = audience_score, x = oscar_season, data = data_movies, statistic = "mean", type = "ci",mu_0 = 0,method = "theoretical")




#4 modeling 
data_movies = na.omit(data_movies)

#proir as BIC
#BMA:Bayesian model average:
movies_bma = bas.lm(audience_score ~ feature_film+drama+runtime+mpaa_rating_R+thtr_rel_year+oscar_season+summer_season+imdb_rating+imdb_num_votes+critics_score+best_pic_nom+best_pic_win+best_actor_win+best_actress_win+best_dir_win+top200_box, data = data_movies, n.models=2^16, prior = "BIC", modelprior = uniform(), initprobs="eplogp")
summary(movies_bma)


plot(movies_bma,which=1,add.smooth=F) #The residuals plot , 2- cummulative , 3 compixty
#The residuals plot of BMA models show that there are outliers.





#5 prediction 
#La La Land" film 
data_movies[nrow(data_movies)+1,] = list(title="La La Land",title_type="Feature Film", genre="Drama",runtime=128, mpaa_rating="PG-13",studio="Lionsgate Films",thtr_rel_year=2016,thtr_rel_month=12, thtr_rel_day=9,dvd_rel_year=2017, dvd_rel_month=4,dvd_rel_day=25,imdb_rating=8.1, imdb_num_votes=310814, critics_rating="Certified Fresh",critics_score=93, audience_rating="Spilled",audience_score=81,best_pic_nom="yes", best_pic_win="yes", best_actor_win="yes",best_actress_win="yes", best_dir_win="yes", top200_box="yes", director="Damien Chazelle", actor1="Ryan Gosling", actor2="Emma Stone", actor3="Amiee Conn",actor4="Terry Walters",actor5="Thom Shelton",imdb_url="http://www.imdb.com/title/tt3783958", rt_url="https://www.rottentomatoes.com/m/la_la_land", feature_film="yes", drama="yes", mpaa_rating_R="no", oscar_season="yes",summer_season="no")
tail(data_movies)


movies.BMA=predict(movies_bma,newdata=data_movies[nrow(data_movies),],prediction=TRUE, se.fit=TRUE, estimator="BMA")
confint(movies.BMA)




