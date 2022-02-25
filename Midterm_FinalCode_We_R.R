#_______________________________________________________________________________
#                           Data Exploration
#_______________________________________________________________________________
movies <- read.csv('films.csv')
attach(movies)
names(movies)
View(movies)
head(movies)

# Find data type
sapply(movies, class)

# Find total number of rows
nrow(movies)

# Clean data
# for the movies whose titles have not been loaded correctly
# for e.g. movie with tile '11.14' has been loaded as '0.46805555555555556'

movies[1,]['title'] <- "11:14"
movies[2,]['title'] <- "9"
movies[3,]['title'] <- "21"
movies[4,]['title'] <- "42"
movies[5,]['title'] <- "54"
movies[6,]['title'] <- "300"
movies[7,]['title'] <- "1941"
movies[8,]['title'] <- "2012"

attach(movies)
View(movies)

# Summary statistics of all numerical variables
summary(budget_in_millions)
boxplot(budget_in_millions, col="cornflowerblue")
hist(budget_in_millions, col="lawngreen")
skewness(budget_in_millions)

summary(month_of_release)
boxplot(month_of_release, col="cornflowerblue")
hist(month_of_release, col="lawngreen")
skewness(month_of_release)

summary(year_of_release)
boxplot(year_of_release, col="cornflowerblue")
hist(year_of_release, col="lawngreen")
skewness(year_of_release)

summary(duration_in_hours)
boxplot(duration_in_hours, col="cornflowerblue")
hist(duration_in_hours, col="lawngreen")
skewness(duration_in_hours)

summary(total_number_languages)
boxplot(total_number_languages, col="cornflowerblue")
hist(total_number_languages, col="lawngreen")
skewness(total_number_languages)

summary(total_number_of_actors)
boxplot(total_number_of_actors, col="cornflowerblue")
hist(total_number_of_actors, col="lawngreen")
skewness(total_number_of_actors)

summary(total_number_of_directors)
boxplot(total_number_of_directors, col="cornflowerblue")
hist(total_number_of_directors, col="lawngreen")
skewness(total_number_of_directors)

summary(total_number_of_producers)
boxplot(total_number_of_producers, col="cornflowerblue")
hist(total_number_of_producers, col="lawngreen")
skewness(total_number_of_producers)

summary(total_number_of_production_companies)
boxplot(total_number_of_production_companies, col="cornflowerblue")
hist(total_number_of_production_companies, col="lawngreen")
skewness(total_number_of_production_companies)

summary(total_number_of_production_countries)
boxplot(total_number_of_production_countries, col="cornflowerblue")
hist(total_number_of_production_countries, col="lawngreen")
skewness(total_number_of_production_countries)

# detect outliers in numerical variables
# budget_in_millions
reg1 = lm(imdb_score~budget_in_millions)
summary(reg1)
#install.packages("car")
require(car)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~budget_in_millions, data=movies2)
summary(reg1_noout)

# month_of_release
reg1 = lm(imdb_score~month_of_release)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~month_of_release, data=movies2)
summary(reg1_noout)

#year_of_release
reg1 = lm(imdb_score~year_of_release)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895, 2310,2718,2045,526), ]
reg1_noout=lm(imdb_score~year_of_release, data=movies2)
summary(reg1_noout)

#duration_in_hours
reg1 = lm(imdb_score~duration_in_hours)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~duration_in_hours, data=movies2)
summary(reg1_noout)

#total_number_languages
reg1 = lm(imdb_score~total_number_languages)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~total_number_languages, data=movies2)
summary(reg1_noout)

#total_number_of_actors
reg1 = lm(imdb_score~total_number_of_actors)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895, 2045), ]
reg1_noout=lm(imdb_score~total_number_of_actors, data=movies2)
summary(reg1_noout)

#total_number_of_directors
reg1 = lm(imdb_score~total_number_of_directors)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~total_number_of_directors, data=movies2)
summary(reg1_noout)

#total_number_of_producers
reg1 = lm(imdb_score~total_number_of_producers)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~total_number_of_producers, data=movies2)
summary(reg1_noout)

#total_number_of_production_companies
reg1 = lm(imdb_score~total_number_of_production_companies)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~total_number_of_production_companies, data=movies2)
summary(reg1_noout)

#total_number_of_production_countries
reg1 = lm(imdb_score~total_number_of_production_countries)
summary(reg1)
outlierTest(reg1)
movies2=movies[-c(633,895), ]
reg1_noout=lm(imdb_score~total_number_of_production_countries, data=movies2)
summary(reg1_noout)

#summary stats of categorical variables

count(main_lang)
barplot(prop.table(table(main_lang)))

count(main_actor1_name)
barplot(prop.table(table(main_actor1_name)))

count(main_actor2_name)
barplot(prop.table(table(main_actor2_name)))

count(main_actor3_name)
barplot(prop.table(table(main_actor3_name)))

count(main_producer_name)
barplot(prop.table(table(main_producer_name)))

count(main_director_name)
barplot(prop.table(table(main_director_name)))

count(editor_name)
barplot(prop.table(table(editor_name)))

count(main_production_company)
barplot(prop.table(table(main_production_company)))

count(main_production_country)
barplot(prop.table(table(main_production_country)))


# summary stats of dummy variables

#barplot(table(genre_action))

# make a dictionary of all the movie genre and their count (of 1s)
#count(genre_action)[2, ]$freq
#count(genre_adventure)
#count(genre_animation)
#count(genre_biography)
#count(genre_comedy)
#count(genre_crime)
#count(genre_documentary)
#count(genre_drama)
#count(genre_family)
#count(genre_fantasy)
#count(genre_filmnoir)
#count(genre_history)
#count(genre_horror)
#count(genre_musical)
#count(genre_mystery)
#count(genre_realitytv)
#count(genre_romance)
#count(genre_scifi)
#count(genre_shortfilm)
#count(genre_sport)
#count(genre_thriller)
#count(genre_war)
#count(genre_western)
install.packages('plyr')
library(plyr)

# finding the number of each genres
genre_type_full <- c("genre_action", "genre_adventure", "genre_animation", "genre_biography", "genre_comedy", "genre_crime", "genre_documentary", "genre_drama", "genre_family", "genre_fantasy", 
                     "genre_filmnoir", "genre_history", "genre_horror", "genre_musical", "genre_mystery", "genre_realitytv", "genre_romance", "genre_scifi", "genre_shortfilm", "genre_sport", 
                     "genre_thriller", "genre_war", "genre_western")
genre_type <- c("action", "advent", "anim", "biogr", "comedy", "crime", "doc", "drama", "family", "fantasy", 
                "filmnoir", "history", "horror", "musical", "mystery", "realitytv", "romance", "scifi", "shortfilm", "sport", 
                "thriller", "war", "western")
genre_type_count <- c(count(genre_action)[2, ]$freq, count(genre_adventure)[2,]$freq, count(genre_animation)[2,]$freq, count(genre_biography)[2,]$freq, count(genre_comedy)[2,]$freq, 
                      count(genre_crime)[2, ]$freq, count(genre_documentary)[2, ]$freq, count(genre_drama)[2, ]$freq, count(genre_family)[2, ]$freq, count(genre_fantasy)[2, ]$freq,
                      count(genre_filmnoir)[2, ]$freq, count(genre_history)[2, ]$freq, count(genre_horror)[2, ]$freq, count(genre_musical)[2, ]$freq, count(genre_mystery)[2, ]$freq, 
                      count(genre_realitytv)[2, ]$freq, count(genre_romance)[2, ]$freq, count(genre_scifi)[2, ]$freq, count(genre_shortfilm)[2, ]$freq, count(genre_sport)[2, ]$freq,
                      count(genre_thriller)[2, ]$freq, count(genre_war)[2, ]$freq, count(genre_western)[2, ]$freq)
names(genre_type_count) <- genre_type

#_______________________________________________________________________________
#                           NVC Test - Heteroskedasticity
#_______________________________________________________________________________

#year_of_release----------------------------------------------------------------
year_lm = lm(imdb_score~year_of_release)
plot(predict(year_lm), residuals(year_lm))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(year_lm) 

#month_of_release---------------------------------------------------------------
dotw_lm = lm(imdb_score~month_of_release)
plot(predict(dotw_lm), residuals(dotw_lm))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(dotw_lm) 

#main_lang_new1-----------------------------------------------------------------
lang_class_lm = lm(imdb_score~main_lang_new1)
plot(predict(lang_class_lm), residuals(lang_class_lm))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(lang_class_lm)


#budget_in_millions-------------------------------------------------------------
duration_in_hours_lm = lm(imdb_score~ budget_in_millions )
plot(predict(duration_in_hours_lm), residuals(duration_in_hours))
abline(0,0, lwd=3, lty=2, col="blue")
ncvTest(duration_in_hours_lm)

#total_number_of_actors---------------------------------------------------------
actors_tot_lm = lm(imdb_score~total_number_of_actors)
plot(predict(actors_tot_lm), residuals(actors_tot_lm))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(actors_tot_lm) 

#duration_in_hours--------------------------------------------------------------
duration_in_hours_lm = lm(imdb_score~duration_in_hours)
plot(predict(duration_in_hours_lm), residuals(duration_in_hours))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(duration_in_hours_lm)

#total_number_of_production_countries-------------------------------------------
prod_country_tot_lm = lm(imdb_score~total_number_of_production_countries)
plot(predict(prod_country_tot_lm), residuals(prod_country_tot_lm))
abline(0,0, lwd=2, lty=3, col="red")
ncvTest(prod_country_tot_lm) 

#total_number_of_directors------------------------------------------------------
directors = lm(imdb_score~total_number_of_directors)
plot(predict(directors_tot_lm), residuals(directors_tot_lm))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(directors_tot_lm) 

#total_number_of_producers------------------------------------------------------
producers = lm(imdb_score~total_number_of_producers)
plot(predict(producers_tot_lm), residuals(producers_tot_lm))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(producers_tot_lm) 

#main_production_country--------------------------------------------------------
main_prod_country_lm = lm(imdb_score~main_production_country)
plot(predict(main_prod_country_lm), residuals(main_prod_country_lm))
abline(0,0, lwd=3, lty=2, col="red")
ncvTest(main_prod_country_lm) 

#_______________________________________________________________________________
#                           Colliniearity - Correlation
#_______________________________________________________________________________
install.packages('reshape2')
attach(films)
library(dplyr)
library(ggplot2)
library(reshape2)

sub=dplyr::select(films,budget_in_millions,month_of_release,year_of_release,
                  duration_in_hours,total_number_languages,total_number_of_actors,
                  total_number_of_directors,total_number_of_producers,
                  total_number_of_production_companies,total_number_of_production_countries)
cormat = round(cor(sub),2)

melted_cormat= melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)}
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)}
upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "orange", mid = "white", midpoint = 0, limit = c(-1,1), 
                       space = "Lab", name="Pearson\nCorrelation")+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
  coord_fixed()

reorder_cormat <- function(cormat){dd <- as.dist((1-cormat)/2)
hc <- hclust(dd)
cormat <-cormat[hc$order, hc$order]}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)


melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "green", high = "orange", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+ coord_fixed()
#print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        panel.grid.major = element_blank(),panel.border = element_blank(),
        panel.background = element_blank(),axis.ticks = element_blank(),
        legend.justification = c(1, 0),legend.position = c(0.5, 0.7),legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,title.position = "top", title.hjust = 0.5))


#_______________________________________________________________________________
#                           Testing Model Fitting
#_______________________________________________________________________________
plot(budget_in_millions, imdb_score)
plot(month_of_release, imdb_score)
plot(year_of_release, imdb_score)
plot(duration_in_hours, imdb_score)
plot(total_number_languages, imdb_score)
plot(total_number_of_actors, imdb_score)
plot(total_number_of_directors, imdb_score)
plot(total_number_of_producers, imdb_score)
plot(total_number_of_production_companies, imdb_score)
plot(genre_action, imdb_score)
plot(genre_adventure, imdb_score)
plot(genre_adventure, imdb_score)
plot(genre_animation, imdb_score)
plot(genre_biography, imdb_score)
plot(genre_comedy, imdb_score)
plot(genre_crime, imdb_score)
plot(genre_documentary, imdb_score)
plot(genre_drama, imdb_score)
plot(genre_family, imdb_score)
plot(genre_fantasy, imdb_score)
plot(genre_filmnoir, imdb_score)
plot(genre_history, imdb_score)
plot(genre_horror, imdb_score)
plot(genre_musical, imdb_score)
plot(genre_mystery, imdb_score)
plot(genre_realitytv, imdb_score)
plot(genre_romance, imdb_score)
plot(genre_scifi, imdb_score)
plot(genre_shortfilm, imdb_score)
plot(genre_sport, imdb_score)
plot(genre_thriller, imdb_score)
plot(genre_war, imdb_score)
plot(genre_western, imdb_score)
plot(main_actor1_is_female, imdb_score)
plot(main_actor2_is_female, imdb_score)
plot(main_actor3_is_female, imdb_score)
plot(main_director_is_female, imdb_score)
plot(main_production_country, imdb_score)
plot(main_lang, imdb_score)
plot(main_production_country , imdb_score)

library(car)
residualPlots(reg_VIF)

#non-constant variance test
ncvTest(reg_VIF)
summary(reg_VIF)

#correcting heteroskedasticity
#install.packages("lmtest")
#install.packages("plm")
require(lmtest)
require(plm)

coeftest(reg_VIF, vcov=vcovHC(reg_VIF, type='HC1'))

## Run simple linear regressions between Y and each predictor xi
reg1=lm(imdb_score~budget_in_millions)
residualPlots(reg1) # non-linearity
summary(reg1) # r^2=0.0044,***
reg2=lm(imdb_score~month_of_release)
residualPlots(reg2) # linear
summary(reg2) #r^2=0.015, ***
reg3=lm(imdb_score~year_of_release)
residualPlots(reg3) # non-linearity
summary(reg3) # r^2=0.07639 ***
reg4=lm(imdb_score~duration_in_hours)
residualPlots(reg4) # non-linear
summary(reg4) #r^2=0.13, ***
reg5=lm(imdb_score~main_lang)
summary(reg5)
reg6=lm(imdb_score~total_number_languages)
residualPlots(reg6) # non-linear
summary(reg6) #r^2=0.006655, ***
reg7=lm(imdb_score~genre_action)
summary(reg7)
reg8=lm(imdb_score~genre_adventure)
summary(reg8)
reg9=lm(imdb_score~genre_animation)
summary(reg9)
reg10=lm(imdb_score~genre_biography)
summary(reg10)
reg11=lm(imdb_score~genre_comedy)
summary(reg11)
reg12=lm(imdb_score~genre_crime)
summary(reg12)
reg13=lm(imdb_score~genre_documentary)
summary(reg13)
reg14=lm(imdb_score~genre_drama)
summary(reg14)
reg15=lm(imdb_score~genre_family)
summary(reg15)
reg16=lm(imdb_score~genre_fantasy)
summary(reg16)
reg17=lm(imdb_score~genre_filmnoir)
summary(reg17)
reg18=lm(imdb_score~genre_history)
summary(reg18)
reg19=lm(imdb_score~genre_horror)
summary(reg19)
reg20=lm(imdb_score~genre_musical)
summary(reg20)
reg21=lm(imdb_score~genre_music)
summary(reg21)
reg22=lm(imdb_score~genre_mystery)
summary(reg22)
reg23=lm(imdb_score~genre_realitytv)
summary(reg23)
reg24=lm(imdb_score~genre_romance)
summary(reg24)
reg25=lm(imdb_score~genre_scifi)
summary(reg25)
reg26=lm(imdb_score~genre_shortfilm)
summary(reg26)
reg27=lm(imdb_score~genre_sport)
summary(reg27)
reg28=lm(imdb_score~genre_thriller)
summary(reg28)
reg29=lm(imdb_score~genre_war)
summary(reg29)
reg30=lm(imdb_score~genre_western)
summary(reg30)
reg31=lm(imdb_score~main_actor1_name)
summary(reg31)
reg34=lm(imdb_score~main_actor1_is_female)
summary(reg34)
reg35=lm(imdb_score~main_actor2_is_female)
summary(reg35)
reg36=lm(imdb_score~main_actor3_is_female)
summary(reg36)
reg37=lm(imdb_score~total_number_of_actors)
summary(reg37)
reg39=lm(imdb_score~main_director_is_female)
summary(reg39)
reg40=lm(imdb_score~total_number_of_directors)
residualPlots(reg40)
summary(reg40) 
reg42=lm(imdb_score~total_number_of_producers)
residualPlots(reg42)
summary(reg42) 
reg45=lm(imdb_score~total_number_of_production_companies)
summary(reg45)
reg47=lm(imdb_score~total_number_of_production_countries)
summary(reg47)


#_______________________________________________________________________________
#                           Testing Model Fitting
#_______________________________________________________________________________
library(car)
library(caTools)
library(splines)
library(methods)
library(boot)

# residual plot for each numeric predictor--------------------------------------
reg = lm(imdb_score~budget_in_millions+month_of_release+year_of_release+duration_in_hours+
           total_number_languages+total_number_of_actors+total_number_of_directors+
           total_number_of_producers+total_number_of_production_companies+
           total_number_of_production_countries)
residualPlots(reg)

# Try polynomial regression with different degree
# we only choose the predictors not satisfying linear assumption
# for budget_in_millions:
acc = vector()
for (i in 2:16) {
  reg1 = lm(imdb_score~poly(budget_in_millions,i),data=movies)
  acc = append(acc,summary(reg1)$r.squared)
}
which.max(acc)
max(acc)
### Validation test set
mse_list = rep(NA,15)
for (i in 2:16) {
  mse = rep(NA,30)
  for (j in 1:30){
    sample=sample.split(movies$imdb_score, SplitRatio=0.5)
    train=subset(movies, sample==TRUE)
    test=subset(movies, sample==FALSE)
    fit=lm(imdb_score~poly(budget_in_millions,i), data=movies)
    test$pred=predict(fit, test)
    test$res=(test$imdb_score-test$pred)
    test$res_sq=(test$res)^2
    mse[j]=mean(test$res_sq)}
  mse_list[i-1] = mean(mse)
}
mse_list
which.min(mse_list)
### K-fold
mse_klist=rep(NA,15)
for (i in 2:16) {
  fit=glm(imdb_score~poly(budget_in_millions,i), data=movies)
  mse_klist[i-1]=cv.glm(movies, fit, K=54)$delta[1] }
which.min(mse_klist) 
min(mse_klist)

# for year_of_release:
acc2 = vector()
for (i in 2:16) {
  reg2 = lm(imdb_score~poly(year_of_release,i),data=movies)
  acc2 = append(acc2,summary(reg2)$r.squared)
}
which.max(acc2)
max(acc2)
### Validation test set
mse_list = rep(NA,15)
for (i in 2:16) {
  mse = rep(NA,30)
  for (j in 1:30){
    sample=sample.split(movies$imdb_score, SplitRatio=0.5)
    train=subset(movies, sample==TRUE)
    test=subset(movies, sample==FALSE)
    fit=lm(imdb_score~poly(year_of_release,i), data=movies)
    test$pred=predict(fit, test)
    test$res=(test$imdb_score-test$pred)
    test$res_sq=(test$res)^2
    mse[j]=mean(test$res_sq)}
  mse_list[i-1] = mean(mse)
}
mse_list
which.min(mse_list)
min(mse_list)
### K-fold
mse_klist=rep(NA,15)
for (i in 2:16) {
  fit=glm(imdb_score~poly(year_of_release,i), data=movies)
  mse_klist[i-1]=cv.glm(movies, fit, K=54)$delta[1] }
which.min(mse_klist) 
min(mse_klist)

# for duration_in_hour:
acc3 = vector()
for (i in 2:16) {
  reg3 = lm(imdb_score~poly(duration_in_hours,i),data=movies)
  acc3 = append(acc3,summary(reg3)$r.squared)
}
which.max(acc3)
max(acc3)
### Validation test set
mse_list = rep(NA,15)
for (i in 2:16) {
  mse = rep(NA,30)
  for (j in 1:30){
    sample=sample.split(movies$imdb_score, SplitRatio=0.5)
    train=subset(movies, sample==TRUE)
    test=subset(movies, sample==FALSE)
    fit=lm(imdb_score~poly(duration_in_hours,i), data=movies)
    test$pred=predict(fit, test)
    test$res=(test$imdb_score-test$pred)
    test$res_sq=(test$res)^2
    mse[j]=mean(test$res_sq)}
  mse_list[i-1] = mean(mse)
}
mse_list
which.min(mse_list)
min(mse_list)
### K-fold
mse_klist=rep(NA,15)
for (i in 2:16) {
  fit=glm(imdb_score~poly(duration_in_hours,i), data=movies)
  mse_klist[i-1]=cv.glm(movies, fit, K=54)$delta[1] }
which.min(mse_klist) 
min(mse_klist)

# for total_number_of_directors:
acc5 = vector()
for (i in 2:7){ # unique points = 7
  reg5 = lm(imdb_score~poly(total_number_of_directors,i),data=movies)
  acc5 = append(acc5,summary(reg5)$r.squared)
}
acc5
which.max(acc5)
max(acc5)
### Validation test set
mse_list5 = rep(NA,6)
for (i in 2:7) {
  mse = rep(NA,30)
  for (j in 1:30){
    sample=sample.split(movies$imdb_score, SplitRatio=0.5)
    train=subset(movies, sample==TRUE)
    test=subset(movies, sample==FALSE)
    fit=lm(imdb_score~poly(total_number_of_directors,i), data=movies)
    test$pred=predict(fit, test)
    test$res=(test$imdb_score-test$pred)
    test$res_sq=(test$res)^2
    mse[j]=mean(test$res_sq)}
  mse_list5[i-1] = mean(mse)
}
mse_list5
which.min(mse_list5)
min(mse_list5)
### K-fold
mse_klist5=vector()
for (i in 2:6) { # unique points = 6 or 5
  fit=glm(imdb_score~poly(total_number_of_directors,i), data=movies)
  mse_klist5 = append(mse_klist5,cv.glm(movies, fit, K=54)$delta[1]) }
mse_klist5
which.min(mse_klist5) 
min(mse_klist5)

# for total_number_of_producers:
acc6 = vector()
for (i in 2:12){ # unique points = 12
  reg6 = lm(imdb_score~poly(total_number_of_producers,i),data=movies)
  acc6 = append(acc6,summary(reg6)$r.squared)
}
acc6
which.max(acc6)
max(acc6)
### Validation test set
mse_list6 = rep(NA,11)
for (i in 2:12) {
  mse = rep(NA,30)
  for (j in 1:30){
    sample=sample.split(movies$imdb_score, SplitRatio=0.5)
    train=subset(movies, sample==TRUE)
    test=subset(movies, sample==FALSE)
    fit=lm(imdb_score~poly(total_number_of_producers,i), data=movies)
    test$pred=predict(fit, test)
    test$res=(test$imdb_score-test$pred)
    test$res_sq=(test$res)^2
    mse[j]=mean(test$res_sq)}
  mse_list6[i-1] = mean(mse)
}
mse_list6
which.min(mse_list6)
min(mse_list6)
### K-fold
mse_klist6=vector()
for (i in 2:11) { # unique points = 11 or 10
  fit=glm(imdb_score~poly(total_number_of_producers,i), data=movies)
  mse_klist6 = append(mse_klist6,cv.glm(movies, fit, K=54)$delta[1]) }
mse_klist6
which.min(mse_klist6)
min(mse_klist6)

# for total_number_of_production_companies:
acc7 = vector()
for (i in 2:15){ # unique points = 15
  reg7 = lm(imdb_score~poly(total_number_of_production_companies,i),data=movies)
  acc7 = append(acc7,summary(reg7)$r.squared)
}
acc7
which.max(acc7)
max(acc7)
### Validation test set
mse_list7 = rep(NA,14)
for (i in 2:15) {
  mse = rep(NA,30)
  for (j in 1:30){
    sample=sample.split(movies$imdb_score, SplitRatio=0.5)
    train=subset(movies, sample==TRUE)
    test=subset(movies, sample==FALSE)
    fit=lm(imdb_score~poly(total_number_of_production_companies,i), data=movies)
    test$pred=predict(fit, test)
    test$res=(test$imdb_score-test$pred)
    test$res_sq=(test$res)^2
    mse[j]=mean(test$res_sq)}
  mse_list7[i-1] = mean(mse)
}
mse_list7
which.min(mse_list7)
min(mse_list7)
### K-fold
mse_klist7=vector()
for (i in 2:14) { # unique points = 14 or 13
  fit=glm(imdb_score~poly(total_number_of_production_companies,i), data=movies)
  mse_klist7 = append(mse_klist7,cv.glm(movies, fit, K=54)$delta[1]) }
mse_klist7
which.min(mse_klist7) 
min(mse_klist7)

# for total_number_of_production_countries:
acc8 = vector()
for (i in 2:7){ # unique points = 7
  reg8 = lm(imdb_score~poly(total_number_of_production_countries,i),data=movies)
  acc8 = append(acc8,summary(reg8)$r.squared)
}
acc8
which.max(acc8)
max(acc8)
### Validation test set
mse_list8 = rep(NA,6)
for (i in 2:7) {
  mse = rep(NA,30)
  for (j in 1:30){
    sample=sample.split(movies$imdb_score, SplitRatio=0.5)
    train=subset(movies, sample==TRUE)
    test=subset(movies, sample==FALSE)
    fit=lm(imdb_score~poly(total_number_of_production_countries,i), data=movies)
    test$pred=predict(fit, test)
    test$res=(test$imdb_score-test$pred)
    test$res_sq=(test$res)^2
    mse[j]=mean(test$res_sq)}
  mse_list8[i-1] = mean(mse)
}
mse_list8
which.min(mse_list8)
min(mse_list8)
### K-fold
mse_klist8=vector()
for (i in 2:6) { # unique points = 6 or 5
  fit=glm(imdb_score~poly(total_number_of_production_countries,i), data=movies)
  mse_klist8 = append(mse_klist8,cv.glm(movies, fit, K=54)$delta[1]) }
mse_klist8
which.min(mse_klist8) 
min(mse_klist8)
#_______________________________________________________________________________
#                             Spline Regression
#_______________________________________________________________________________
# However eventually we decide not to use spline in our regression model
library(splines)
# budget_in_millions
# 3 knots
qlist = vector()
for (i in 1:3){
  qlist = append(qlist,quantile(budget_in_millions,i*0.25))
}
# spline regression
splacc1 = vector()
for (j in 2:16){
  eqlspl = lm(imdb_score~bs(budget_in_millions,knots=qlist,degree=j))
  splacc1 = append(splacc1,summary(eqlspl)$r.squared)
}
splacc1
which.max(splacc1)
max(splacc1)
# K-fold
sklist1=vector()
for (j in 2:16) { 
  fit=glm(imdb_score~bs(total_number_of_production_countries,knots=qlist,degree=j))
  sklist1 = append(sklist1,cv.glm(movies, fit, K=10)$delta[1])}
sklist1 # all results are NaN for some reason we don't know why
which.min(sklist1) 
min(sklist1)
# we then tried 4 knots to see if anything changes
# 4 knots
qlist = vector()
for (i in 1:4){
  qlist = append(qlist,quantile(budget_in_millions,i*0.2))
}
# spline regression
splacc1 = vector()
for (j in 2:16){
  eqlspl = lm(imdb_score~bs(budget_in_millions,knots=qlist,degree=j))
  splacc1 = append(splacc1,summary(eqlspl)$r.squared)
}
splacc1 # it still gives us the NaN output
which.max(splacc1)
max(splacc1)

# do we really need spline for this dataset?
# we then tried plotting out the spline with scatter plot
library(ggplot2)
k1= quantile(duration_in_hours,.20) 
k2= quantile(duration_in_hours,.40) 
k3= quantile(duration_in_hours,.60) 
k4= quantile(duration_in_hours,.80)
plot=ggplot(movies, aes(y=imdb_score, x=duration_in_hours))
scatter= geom_point(color="grey") 
spline_1= geom_smooth(method = "lm", formula = y~bs(x,knots=c(k1,k2,k3,k4), degree=3))
plot+scatter+spline_1
# polymomial with scatter plot
linear_reg = lm(imdb_score~poly(duration_in_hours,6))
summary(linear_reg)
ln=geom_smooth(method = "lm", formula = y~poly(x,6))
plot+scatter+ln
# we barely see any differences between the line shape of spline and polynomial
# Because:
# spline regression is hard to do kfold validation
# we even asked TA for help on kfold validation for spline 
# but both TA and us still couldn't solve the issue
# also we see minimum difference between spline and polynomial
# therefore we decide not to use spline in our regression model

#_______________________________________________________________________________
#                    Model Construction - For numerical Predictors
#_______________________________________________________________________________
# we start building up our model by adding predictors one by one
# we begin with numeric predictors
# Because the poly degree given by r^2 and validation test is too high
# Therefore we choose the poly degree based on kfold validation test

# since we cannot know the importance of predictors before the model is created
# here are some of the important predictors based on our intuition:
# (also you can see our reasoning for the ranking in the report)
# 1. budget_in_millions
# 2. year_of_release
# 3. duration_in_hours
# 4. month_of_release
# 5. total_number_of_production_companies
# 6. total_number_of_actors
# 7. total_number_of_producers
# 8. total_number_of_languages
# 9. total_number_of_countries
# 10.total_number_of_directors

# 1. budget_in_millions
We_R = lm(imdb_score~poly(budget_in_millions,4))
summary(We_R)$adj.r.square # adjusted r^2 = 0.05244
# 2. year_of_release
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3))
summary(We_R)$adj.r.square # adjusted r^2 = 0.1023 (up) (keep)
# 3. duration_in_hours
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6))
summary(We_R)$adj.r.square # adjusted r^2 = 0.2492 (up) (keep)
# 4. month_of_release 
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release)
summary(We_R)$adj.r.square # adjusted r^2 = 0.2543 (up) (drop) doesnt increase much
# 5. total_number_of_production_companies
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release+
            poly(total_number_of_production_companies,2))
summary(We_R)$adj.r.square # adjusted r^2 = 0.2560 (up) (drop?)
# 6. total_number_of_actors
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release+
            poly(total_number_of_production_companies,2)+
            poly(total_number_of_actors,2))
summary(We_R)$adj.r.square # adjusted r^2 - 0.2804 (up) (keep)
# 7. total_number_of_producers
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release+
            poly(total_number_of_production_companies,2)+
            poly(total_number_of_actors,2)+
            poly(total_number_of_producers,3))
summary(We_R)$adj.r.square # adjusted r^2 = 0.2863 (up) (drop)
# 8. total_number_languages
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release+
            poly(total_number_of_production_companies,2)+
            poly(total_number_of_actors,2)+
            poly(total_number_of_producers,3)+
            total_number_languages)
summary(We_R)$adj.r.square # adjusted r^2 = 0.2868 (up) (drop)
# 9. total_number_of_countries
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release+
            poly(total_number_of_production_companies,2)+
            poly(total_number_of_actors,2)+
            poly(total_number_of_producers,3)+
            total_number_languages+
            poly(total_number_of_production_countries,3))
summary(We_R)$adj.r.square # adjusted r^2 = 0.2886 (up)(drop)
# 10.total_number_of_directors
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release+
            poly(total_number_of_production_companies,2)+
            poly(total_number_of_actors,2)+
            poly(total_number_of_producers,3)+
            total_number_languages+
            poly(total_number_of_production_countries,3)+
            poly(total_number_of_directors,2))
summary(We_R)$adj.r.square # adjusted r^2 = 0.2939 (up)(drop)
summary(We_R)

# let's take out budget_in_millions and see how adjusted r^2 will affect our model
# if the change is small then we should drop budget_in_millions
We_R = lm(imdb_score~poly(year_of_release,3)
          +poly(duration_in_hours,6)+month_of_release+
            poly(total_number_of_production_companies,2)+
            poly(total_number_of_actors,2)+
            poly(total_number_of_producers,3)+
            total_number_languages+
            poly(total_number_of_production_countries,3)+
            poly(total_number_of_directors,2))
summary(We_R) # adjusted r^2 decreased 0.05
# so we should keep budget_in_millions

# select all the predictors that make significant contribution to r^2
We_R = lm(imdb_score~poly(budget_in_millions,4)+
            poly(year_of_release,3)+
            poly(duration_in_hours,6)+
            poly(total_number_of_actors,2))
summary(We_R) # adjusted r^2 = 0.2741
# we see that duration_of_hours at degree = 5 and 6 has less significance than others
# we then perform an anova test
We_R2 = lm(imdb_score~poly(budget_in_millions,4)+
            poly(year_of_release,3)+
            poly(duration_in_hours,2)+
            poly(total_number_of_actors,2))
We_R3 = lm(imdb_score~poly(budget_in_millions,4)+
            poly(year_of_release,3)+
            poly(duration_in_hours,3)+
            poly(total_number_of_actors,2))
We_R4 = lm(imdb_score~poly(budget_in_millions,4)+
            poly(year_of_release,3)+
            poly(duration_in_hours,4)+
            poly(total_number_of_actors,2))
We_R5 = lm(imdb_score~poly(budget_in_millions,4)+
            poly(year_of_release,3)+
            poly(duration_in_hours,5)+
            poly(total_number_of_actors,2))
We_R6 = lm(imdb_score~poly(budget_in_millions,4)+
            poly(year_of_release,3)+
            poly(duration_in_hours,6)+
            poly(total_number_of_actors,2))
anova(We_R2, We_R3, We_R4, We_R5, We_R6)
#Appendix graph
# based on the anova test we see that degree 5 or 6 have p>0.05
# therefore we change the poly degree of duration to 4
We_R = lm(imdb_score~poly(budget_in_millions,4)+
            poly(year_of_release,3)+
            poly(duration_in_hours,4)+
            poly(total_number_of_actors,2))
summary(We_R)
# the adjusted r^2 doesn't change much
# and the p value of each predictor indicates significance
# let's do a validation test
# k-fold test
fit=glm(imdb_score~poly(budget_in_millions,4)+
          poly(year_of_release,3)+
          poly(duration_in_hours,4)+
          poly(total_number_of_actors,2)+
          genre_drama+
          genre_horror+
          genre_comedy+
          genre_action
          , data=movies)
mse = cv.glm(movies, fit, K=54)$delta[1]
mse # = 0.6825, doesn't look bad
# So this is the model based on numeric predictors.

# Playing with the polynomials degrees for all non-linear numeric predictors
# to validate the polynomial degress that we use above 
films <- read.csv('films.csv')
library(boot)
smin <- 100000000
comb <- vector()
for (a1 in 2:6) {
  for (a2 in 2:6){
    for (a3 in 2:6){
      for (a4 in 2:6){
        for (a5 in 2:6){
          fit=glm(imdb_score~month_of_release+poly(budget_in_millions,a1)+
                    poly(year_of_release,a2)+poly(duration_in_hours,a3)+
                    poly(total_number_of_actors,a4)+poly(total_number_of_producers,a5),
                  data=films)
          sm <- cv.glm(films,fit,K=20)$delta[1]
          if (sm < smin){
            smin <- sm
            comb <- c(a1,a2,a3,a4,a5)
          }
        }
      }
    }
  }
}
print(smin)
print(comb) #43423

#_______________________________________________________________________________
#     Treating Categorical Variables: main_production_country and main_lang
#_______________________________________________________________________________
# 1.create another column to rename the observations that are not US,UK,France,German,Canada
#   for main_production_country as 'other'
#   and generate dummy variables for main_production_country
# 2.create another column to rename the observations that are not English,Francais for main_lang as 'other' 
#   and generate dummy variables for main_lang

films$main_production_country_new1 = films$main_production_country_new
attach(films)

for (i in seq(1,2953)) {
  if (films$main_production_country[i] == 'United States of America') {
    films$main_production_country_new1[i] <- 'United States of America'
  }
  else if (films$main_production_country[i] == 'United Kingdom'){
    films$main_production_country_new1[i] <- 'United Kingdom'
  }
  else if (films$main_production_country[i] == 'France'){
    films$main_production_country_new1[i] <- 'France'
  } 
  else if (films$main_production_country[i] == 'Germany'){
    films$main_production_country_new1[i] <- 'Germany'
  } 
  else if (films$main_production_country[i]== 'Canada'){
    films$main_production_country_new1[i] <- 'Canada'
  } 
  else {
    films$main_production_country_new1[i] = 'Other'
  }} 
attach(films)
films$main_production_country_new1 = as.factor(films$main_production_country_new1)
attach(films)
levels(main_production_country_new1)

films$main_lang_new1 = films$main_lang
attach(films)
for (i in seq(1,2953)) {
  if (films$main_lang[i] == 'English') {
    films$main_lang_new1[i] <- 'English'
  }
  else if (films$main_lang[i] == 'Français'){
    films$main_lang_new1[i] <- 'Français'
  }
  else if (films$main_lang[i] == 'Deutsch'){
    films$main_lang_new1[i] <- 'Deutsch'
  }
  else {
    films$main_lang_new1[i] <- 'Other'
  }
}
warnings()
attach(films)
films$main_lang_new1 = as.factor(films$main_lang_new1)
attach(films)
levels(main_lang_new1)

#_______________________________________________________________________________
#               Model Construction - Version 1 
#  did not use this model in the end, please see Final Model Version 2
#_______________________________________________________________________________

# Step 1: running a general model included all categorical variables
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,4)+month_of_release+
            total_number_of_production_companies+
            poly(total_number_of_actors,2)+
            poly(total_number_of_producers,3)+
            total_number_languages+
            poly(total_number_of_production_countries,3)+
            poly(total_number_of_directors,2)+main_production_country_new1+main_lang_new1+
            genre_action+genre_adventure+genre_animation+genre_biography+genre_comedy+
            genre_crime+genre_documentary+genre_drama+genre_family+genre_fantasy+genre_filmnoir+
            genre_history+genre_horror+genre_musical+genre_music+genre_mystery+genre_romance+genre_scifi+
            genre_sport+genre_thriller+genre_war+genre_western+
            main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
            main_director_is_female)
summary(We_R)

# Step 2: removing categorical variables that has a p-value > 0.05
We_R1 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             total_number_of_production_companies+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             total_number_languages+
             poly(total_number_of_production_countries,3)+
             poly(total_number_of_directors,2)+main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_music+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female)
summary(We_R1)

# Step 3: removing numerical variables that has a p-value > 0.05
We_R2 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female)
summary(We_R2)

# Step 4: adding interaction terms for genre
# Since most of the movies has more than one genre, so consider adding interaction terms between genres
We_R3 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_adventure+genre_action*genre_animation+genre_action*genre_comedy+
             genre_action*genre_documentary+genre_action*genre_family+genre_action*genre_history+
             genre_action*genre_drama+genre_action*genre_horror+genre_action*genre_romance+
             genre_adventure*genre_animation+genre_adventure*genre_comedy+genre_adventure*genre_drama+
             genre_adventure*genre_horror+genre_adventure*genre_romance+
             genre_animation*genre_comedy+genre_animation*genre_documentary+genre_animation*genre_drama+
             genre_animation*genre_family+genre_animation*genre_history+genre_animation*genre_horror+
             genre_animation*genre_romance+
             genre_comedy*genre_documentary+genre_comedy*genre_drama+genre_comedy*genre_family+
             genre_comedy*genre_history+genre_comedy*genre_horror+genre_comedy*genre_romance+
             genre_documentary*genre_drama+genre_documentary*genre_family+genre_documentary*genre_history+
             genre_documentary*genre_horror+genre_documentary*genre_romance+
             genre_drama*genre_family+genre_drama*genre_history+genre_drama*genre_horror+genre_drama*genre_romance+
             genre_family*genre_history+genre_family*genre_horror+genre_family*genre_romance+
             genre_history*genre_horror+genre_history*genre_romance+genre_horror*genre_romance)

summary(We_R3)

# Step 5: removing interaction terms for genre that interaction coefficients = 0
We_R4 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_adventure+genre_action*genre_animation+genre_action*genre_comedy+
             genre_action*genre_documentary+genre_action*genre_family+genre_action*genre_history+
             genre_action*genre_drama+genre_action*genre_horror+genre_action*genre_romance+
             genre_adventure*genre_animation+genre_adventure*genre_comedy+genre_adventure*genre_drama+
             genre_adventure*genre_horror+genre_adventure*genre_romance+
             genre_animation*genre_comedy+genre_animation*genre_drama+
             genre_animation*genre_family+
             genre_comedy*genre_documentary+genre_comedy*genre_drama+genre_comedy*genre_family+
             genre_comedy*genre_horror+genre_comedy*genre_romance+
             genre_documentary*genre_drama+genre_documentary*genre_history+
             genre_drama*genre_family+genre_drama*genre_history+genre_drama*genre_horror+genre_drama*genre_romance+
             genre_family*genre_history+genre_family*genre_romance+
             genre_history*genre_romance+genre_horror*genre_romance)

summary(We_R4)

# Step 6: only keep interaction terms for genres that p-value <0.05
We_R5 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_drama+
             genre_comedy*genre_documentary+genre_comedy*genre_family+
             genre_comedy*genre_horror)

summary(We_R5)

# Step 7: adding interaction between year of release and genre, where genre has p-value <= 0.001 (***)
We_R6 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_drama+
             genre_comedy*genre_documentary+genre_comedy*genre_family+
             genre_comedy*genre_horror+
             year_of_release*genre_action+year_of_release*genre_adventure+year_of_release*genre_animation+
             year_of_release*genre_comedy+year_of_release*genre_documentary+year_of_release*genre_drama+
             year_of_release*genre_horror)

summary(We_R6)

# Step 8: removing interaction between year of release and genre, where interaction terms have p-value > 0.1
# adding interaction term between year of release and budget in millions
# adding interaction term between main_actor1_is_female and main_actor2_is_female
# adding interaction term between main_actor1_is_female and main_actor3_is_female
# adding interaction term between main_actor2_is_female and main_actor3_is_female
# adding main_actor1_is_female*budget_in_millions as interaction terms
We_R7 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_drama+genre_comedy*genre_documentary+
             genre_comedy*genre_family+
             genre_comedy*genre_horror+
             year_of_release*genre_comedy+year_of_release*genre_drama+
             year_of_release*genre_horror+year_of_release*budget_in_millions+
             main_actor1_is_female*main_actor2_is_female+main_actor1_is_female*main_actor3_is_female+
             main_actor2_is_female*main_actor3_is_female+main_actor1_is_female*budget_in_millions)

summary(We_R7)

# Step 9: removing unsignificant predictors
We_R8 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_drama+
             genre_comedy*genre_family+
             genre_comedy*genre_horror+
             year_of_release*genre_comedy+year_of_release*genre_drama+
             year_of_release*genre_horror+year_of_release*budget_in_millions+
             main_actor1_is_female*main_actor2_is_female+
             main_actor2_is_female*main_actor3_is_female+main_actor1_is_female*budget_in_millions)
summary(We_R8)

# k-fold Validation
library(boot)
fit4=glm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
         +poly(duration_in_hours,4)+month_of_release+
           poly(total_number_of_actors,2)+
           poly(total_number_of_producers,3)+
           main_production_country_new1+main_lang_new1+
           genre_action+genre_adventure+genre_animation+genre_comedy+
           genre_documentary+genre_drama+genre_family+
           genre_history+genre_horror+genre_romance+
           main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
           genre_action*genre_drama+
           genre_comedy*genre_family+
           genre_comedy*genre_horror+
           year_of_release*genre_comedy+year_of_release*genre_drama+
           year_of_release*genre_horror+year_of_release*budget_in_millions+
           main_actor1_is_female*main_actor2_is_female+
           main_actor2_is_female*main_actor3_is_female+
           main_actor1_is_female*budget_in_millions, data=films)
mse4 = cv.glm(films, fit4, K=100)$delta[1]
mse4 # = 0.55999

#_______________________________________________________________________________
#               Model Construction - Version 2 with Better Results
#                           This is the final model
#_______________________________________________________________________________
# Step 1: running a general model included all categorical variables
We_R = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
          +poly(duration_in_hours,4)+month_of_release+
            total_number_of_production_companies+
            poly(total_number_of_actors,2)+
            poly(total_number_of_producers,3)+
            total_number_languages+
            poly(total_number_of_production_countries,3)+
            poly(total_number_of_directors,2)+main_production_country_new1+main_lang_new1+
            genre_action+genre_adventure+genre_animation+genre_biography+genre_comedy+
            genre_crime+genre_documentary+genre_drama+genre_family+genre_fantasy+genre_filmnoir+
            genre_history+genre_horror+genre_musical+genre_music+genre_mystery+genre_romance+genre_scifi+
            genre_sport+genre_thriller+genre_war+genre_western+
            main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
            main_director_is_female)
summary(We_R)
# Step 2: removing categorical variables that has a p-value > 0.05
We_R1 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             total_number_of_production_companies+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             total_number_languages+
             poly(total_number_of_production_countries,3)+
             poly(total_number_of_directors,2)+main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_crime+genre_documentary+genre_drama+genre_family+genre_fantasy+
             genre_history+genre_horror+genre_music+genre_mystery+genre_romance+genre_scifi+
             genre_thriller+genre_war+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             main_director_is_female)
summary(We_R1)
# Step 3: removing numerical variables that has a p-value > 0.05
We_R2 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             #poly(total_number_of_production_countries,3)+
             #poly(total_number_of_directors,2)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_crime+genre_documentary+genre_drama+genre_family+genre_fantasy+
             genre_history+genre_horror+genre_music+genre_mystery+genre_romance+genre_scifi+
             genre_thriller+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             main_director_is_female)
summary(We_R2)

# Step 4: adding interaction terms for genre
# Since most of the movies has more than one genre, so consider adding interaction terms between genres
We_R3 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_adventure+genre_action*genre_animation+genre_action*genre_comedy+
             genre_action*genre_documentary+genre_action*genre_family+genre_action*genre_history+
             genre_action*genre_drama+genre_action*genre_horror+genre_action*genre_romance+genre_action*genre_crime+
             genre_action*genre_music+genre_action*genre_scifi+genre_action*genre_thriller+
             genre_adventure*genre_animation+genre_adventure*genre_comedy+genre_adventure*genre_drama+
             genre_adventure*genre_horror+genre_adventure*genre_romance+genre_adventure*genre_crime+
             genre_adventure*genre_music+genre_adventure*genre_scifi+genre_adventure*genre_thriller+
             genre_animation*genre_comedy+genre_animation*genre_documentary+genre_animation*genre_drama+
             genre_animation*genre_family+genre_animation*genre_history+genre_animation*genre_horror+
             genre_animation*genre_romance+genre_animation*genre_crime+
             genre_animation*genre_music+genre_animation*genre_scifi+genre_animation*genre_thriller+
             genre_comedy*genre_documentary+genre_comedy*genre_drama+genre_comedy*genre_family+
             genre_comedy*genre_history+genre_comedy*genre_horror+genre_comedy*genre_romance+genre_comedy*genre_crime+
             genre_comedy*genre_music+genre_comedy*genre_scifi+genre_comedy*genre_thriller+
             genre_crime*genre_documentary+genre_crime*genre_drama+genre_crime*genre_family+genre_crime*genre_fantasy+
             genre_crime*genre_history+genre_crime*genre_horror+genre_crime*genre_music+genre_crime*genre_mystery+
             genre_crime*genre_romance+genre_crime*genre_scifi+genre_crime*genre_thriller+
             genre_documentary*genre_drama+genre_documentary*genre_family+genre_documentary*genre_history+
             genre_documentary*genre_horror+genre_documentary*genre_romance+genre_documentary*genre_crime+
             genre_documentary*genre_music+genre_documentary*genre_scifi+genre_documentary*genre_thriller+
             genre_drama*genre_family+genre_drama*genre_history+genre_drama*genre_horror+genre_drama*genre_romance+
             genre_drama*genre_music+genre_drama*genre_scifi+genre_drama*genre_thriller+genre_drama*genre_crime+
             genre_family*genre_history+genre_family*genre_horror+genre_family*genre_romance+
             genre_family*genre_music+genre_family*genre_scifi+genre_family*genre_thriller+genre_family*genre_crime+
             genre_history*genre_horror+genre_history*genre_romance+
             genre_history*genre_music+genre_history*genre_scifi+genre_history*genre_thriller+genre_history*genre_crime+
             genre_horror*genre_romance+genre_horror*genre_music+genre_horror*genre_scifi+genre_horror*genre_thriller+genre_horror*genre_crime+
             genre_romance*genre_music+genre_romance*genre_scifi+genre_romance*genre_thriller+genre_romance*genre_crime+
             genre_music*genre_scifi+genre_music*genre_thriller+genre_music*genre_crime+
             genre_scifi*genre_thriller+genre_scifi*genre_crime+
             genre_thriller*genre_crime)

summary(We_R3)
# Step 5: removing interaction terms for genre that interaction coefficients = 0
We_R4 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_adventure+genre_action*genre_animation+genre_action*genre_comedy+
             genre_action*genre_documentary+genre_action*genre_family+genre_action*genre_history+
             genre_action*genre_drama+genre_action*genre_horror+genre_action*genre_romance+genre_action*genre_crime+
             genre_action*genre_scifi+genre_action*genre_thriller+
             genre_adventure*genre_animation+genre_adventure*genre_comedy+genre_adventure*genre_drama+
             genre_adventure*genre_horror+genre_adventure*genre_romance+genre_adventure*genre_crime+
             genre_adventure*genre_scifi+genre_adventure*genre_thriller+
             genre_animation*genre_comedy+genre_animation*genre_drama+
             genre_animation*genre_family+genre_animation*genre_crime+
             genre_animation*genre_scifi+genre_animation*genre_thriller+
             genre_comedy*genre_documentary+genre_comedy*genre_drama+genre_comedy*genre_family+
             genre_comedy*genre_horror+genre_comedy*genre_romance+genre_comedy*genre_crime+
             genre_comedy*genre_music+genre_comedy*genre_scifi+genre_comedy*genre_thriller+
             genre_crime*genre_documentary+genre_crime*genre_drama+genre_crime*genre_family+genre_crime*genre_fantasy+
             genre_crime*genre_history+genre_crime*genre_horror+genre_crime*genre_music+genre_crime*genre_mystery+
             genre_crime*genre_romance+genre_crime*genre_scifi+genre_crime*genre_thriller+
             genre_documentary*genre_drama+genre_documentary*genre_history+
             genre_drama*genre_family+genre_drama*genre_history+genre_drama*genre_horror+genre_drama*genre_romance+
             genre_drama*genre_music+genre_drama*genre_scifi+genre_drama*genre_thriller+genre_drama*genre_crime+
             genre_family*genre_history+genre_family*genre_romance+
             genre_family*genre_scifi+genre_family*genre_crime+
             genre_history*genre_romance+
             genre_history*genre_thriller+genre_history*genre_crime+
             genre_horror*genre_romance+genre_horror*genre_scifi+genre_horror*genre_thriller+genre_horror*genre_crime+
             genre_romance*genre_music+genre_romance*genre_scifi+genre_romance*genre_thriller+genre_romance*genre_crime+
             genre_music*genre_thriller+genre_music*genre_crime+
             genre_scifi*genre_thriller+genre_scifi*genre_crime+
             genre_thriller*genre_crime)
summary(We_R4)

# Step 6: only keep interaction terms for genres that p-value <0.05
We_R5 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_animation+
             genre_action*genre_drama+
             genre_adventure*genre_scifi+
             genre_animation*genre_comedy+genre_animation*genre_thriller+genre_comedy*genre_family+
             genre_comedy*genre_horror+
             genre_drama*genre_thriller+
             genre_crime*genre_fantasy+
             genre_crime*genre_horror+genre_crime*genre_mystery+
             genre_crime*genre_romance)
summary(We_R5)

# Step 7: adding interaction between year of release and genre
We_R6 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_animation+genre_action*genre_drama+
             genre_adventure*genre_scifi+
             genre_animation*genre_comedy+genre_animation*genre_thriller+
             genre_comedy*genre_family+genre_comedy*genre_horror+
             genre_drama*genre_thriller+
             genre_crime*genre_fantasy+genre_crime*genre_horror+genre_crime*genre_mystery+genre_crime*genre_romance+
             year_of_release*genre_action+year_of_release*genre_adventure+year_of_release*genre_animation+
             year_of_release*genre_comedy+year_of_release*genre_documentary+year_of_release*genre_drama+
             year_of_release*genre_family+year_of_release*genre_history+year_of_release*genre_horror+year_of_release*genre_romance)
summary(We_R6)

# Step 8: removing interaction between year of release and genre, where interaction terms have p-value > 0.1
# adding interaction term between year of release and budget in millions
# adding interaction term between main_actor1_is_female and main_actor2_is_female
# adding interaction term between main_actor1_is_female and main_actor3_is_female
# adding interaction term between main_actor2_is_female and main_actor3_is_female
# adding main_actor1_is_female*budget_in_millions as interaction terms
#total_number_of_production_countries*total_number_of_actors+
#total_number_of_directors*budget_in_millions
We_R7 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_animation+genre_action*genre_drama+
             genre_adventure*genre_scifi+
             genre_animation*genre_comedy+genre_animation*genre_thriller+
             genre_comedy*genre_family+genre_comedy*genre_horror+
             genre_crime*genre_fantasy+genre_crime*genre_horror+genre_crime*genre_mystery+
             genre_drama*genre_thriller+
             year_of_release*genre_action+
             year_of_release*genre_comedy+year_of_release*genre_drama+
             year_of_release*genre_history+year_of_release*genre_horror+year_of_release*genre_romance+
             year_of_release*budget_in_millions+
             main_actor1_is_female*main_actor2_is_female+main_actor1_is_female*main_actor3_is_female+
             main_actor2_is_female*main_actor3_is_female+main_actor1_is_female*budget_in_millions+
             total_number_of_production_countries*total_number_of_actors+
             total_number_of_directors*budget_in_millions)
summary(We_R7)

# Step 9: removing unsignificant predictors
We_R8 = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
           +poly(duration_in_hours,4)+month_of_release+
             poly(total_number_of_actors,2)+
             poly(total_number_of_producers,3)+
             main_production_country_new1+main_lang_new1+
             genre_action+genre_adventure+genre_animation+genre_comedy+
             genre_documentary+genre_drama+genre_family+
             genre_history+genre_horror+genre_romance+
             main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
             genre_action*genre_animation+genre_action*genre_drama+
             genre_adventure*genre_scifi+
             genre_animation*genre_comedy+genre_animation*genre_thriller+
             genre_comedy*genre_family+genre_comedy*genre_horror+
             genre_crime*genre_fantasy+genre_crime*genre_horror+genre_crime*genre_mystery+
             genre_drama*genre_thriller+
             year_of_release*genre_comedy+year_of_release*genre_drama+
             year_of_release*genre_history+year_of_release*genre_horror+year_of_release*genre_romance+
             year_of_release*budget_in_millions+
             main_actor1_is_female*main_actor2_is_female+
             main_actor2_is_female*main_actor3_is_female+main_actor1_is_female*budget_in_millions+
             total_number_of_production_countries*total_number_of_actors+
             total_number_of_directors*budget_in_millions)
summary(We_R8)

# k-fold validation
library(boot)
fit5=glm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
         +poly(duration_in_hours,4)+month_of_release+
           poly(total_number_of_actors,2)+
           poly(total_number_of_producers,3)+
           main_production_country_new1+main_lang_new1+
           genre_action+genre_adventure+genre_animation+genre_comedy+
           genre_documentary+genre_drama+genre_family+
           genre_history+genre_horror+genre_romance+
           main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
           genre_action*genre_animation+genre_action*genre_drama+
           genre_adventure*genre_scifi+
           genre_animation*genre_comedy+genre_animation*genre_thriller+
           genre_comedy*genre_family+genre_comedy*genre_horror+
           genre_crime*genre_fantasy+genre_crime*genre_horror+genre_crime*genre_mystery+
           genre_drama*genre_thriller+
           year_of_release*genre_comedy+year_of_release*genre_drama+
           year_of_release*genre_history+year_of_release*genre_horror+year_of_release*genre_romance+
           year_of_release*budget_in_millions+
           main_actor1_is_female*main_actor2_is_female+
           main_actor2_is_female*main_actor3_is_female+main_actor1_is_female*budget_in_millions+
           total_number_of_production_countries*total_number_of_actors+
           total_number_of_directors*budget_in_millions, data=films)
mse5 = cv.glm(films, fit5, K=200)$delta[1]
mse5 # = 0.5529

# Step 10: Outliers
outlierTest(We_R8)
# Remove outliers
films5=films[-c(633,895,2310,2045,2718,526),]
We_R8_noout = lm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
                 +poly(duration_in_hours,4)+month_of_release+
                   poly(total_number_of_actors,2)+
                   poly(total_number_of_producers,3)+
                   main_production_country_new1+main_lang_new1+
                   genre_action+genre_adventure+genre_animation+genre_comedy+
                   genre_documentary+genre_drama+genre_family+
                   genre_history+genre_horror+genre_romance+
                   main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
                   genre_action*genre_animation+genre_action*genre_drama+
                   genre_adventure*genre_scifi+
                   genre_animation*genre_comedy+genre_animation*genre_thriller+
                   genre_comedy*genre_family+genre_comedy*genre_horror+
                   genre_crime*genre_fantasy+genre_crime*genre_horror+genre_crime*genre_mystery+
                   genre_drama*genre_thriller+
                   year_of_release*genre_comedy+year_of_release*genre_drama+
                   year_of_release*genre_history+year_of_release*genre_horror+year_of_release*genre_romance+
                   year_of_release*budget_in_millions+
                   main_actor1_is_female*main_actor2_is_female+
                   main_actor2_is_female*main_actor3_is_female+main_actor1_is_female*budget_in_millions+
                   total_number_of_production_countries*total_number_of_actors+
                   total_number_of_directors*budget_in_millions, data=films5)
summary(We_R8_noout)

fit_noout=glm(imdb_score~poly(budget_in_millions,4)+poly(year_of_release,3)
              +poly(duration_in_hours,4)+month_of_release+
                poly(total_number_of_actors,2)+
                poly(total_number_of_producers,3)+
                main_production_country_new1+main_lang_new1+
                genre_action+genre_adventure+genre_animation+genre_comedy+
                genre_documentary+genre_drama+genre_family+
                genre_history+genre_horror+genre_romance+
                main_actor1_is_female+main_actor2_is_female+main_actor3_is_female+
                genre_action*genre_animation+genre_action*genre_drama+
                genre_adventure*genre_scifi+
                genre_animation*genre_comedy+genre_animation*genre_thriller+
                genre_comedy*genre_family+genre_comedy*genre_horror+
                genre_crime*genre_fantasy+genre_crime*genre_horror+genre_crime*genre_mystery+
                genre_drama*genre_thriller+
                year_of_release*genre_comedy+year_of_release*genre_drama+
                year_of_release*genre_history+year_of_release*genre_horror+year_of_release*genre_romance+
                year_of_release*budget_in_millions+
                main_actor1_is_female*main_actor2_is_female+
                main_actor2_is_female*main_actor3_is_female+main_actor1_is_female*budget_in_millions+
                total_number_of_production_countries*total_number_of_actors+
                total_number_of_directors*budget_in_millions, data=films5)
mse_noout5 = cv.glm(films5, fit_noout, K=200)$delta[1]
mse_noout5 # = 0.5258602
# K=300 -> MSE = 0.5265

#_______________________________________________________________________________
#                                  Plots
#_______________________________________________________________________________
library(ggplot2)
attach(films)
#main_lang_new1-------------------------------------------------------------
bin2 = c(1,2,3,4,5,6,7,8,9,10)
ggplot(films,aes(x=main_lang_new1, y=imdb_score)) +
  geom_boxplot(lwd=1.2) +geom_jitter(alpha=0.15, aes(color=cut(imdb_score,bin2),))+
  theme_gray()+
  labs(title="Language Grouping vs Imdb Score",x="Language group", y="Imdb Score",
       colour="Imdb Score") + scale_color_hue(l=10, c=150)+
  theme(plot.title = element_text(color = "black", size = 19, face = "bold", hjust= 0.7),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15, face="bold"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        legend.position="bottom")

#year_of_release---------------------------------------------------------------
ggplot(films, aes(x=year_of_release, fill=cut(imdb_score,bin2))) +
  geom_histogram(color="black", bins = 20)+
  geom_vline(data=films, aes(xintercept=mean(year_of_release)), color="blue",size=1.5,
             linetype="dotted")+
  labs(title="Frequency Distribution by Year",x="Release Year", y = "Frequency of Movies")+
  theme_minimal() +
  scale_fill_discrete(name = "Imdb Score Buckets",
                      labels=c("0 to 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5",
                               "5 to 6", "6 to 7", "7 to 8", "8 to 9", "9 to 10"))+
  theme(plot.title = element_text(color = "black", size = 19, face = "bold", hjust= 0.7),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15, face="bold"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        legend.position="bottom")

#total_number_of_actors---------------------------------------------------------
ggplot(films, aes(x=total_number_of_actors, y=imdb_score)) +
  geom_jitter(alpha=0.15, aes(color=cut(imdb_score,bin2),))+
  theme_gray()+
  labs(title="Number of Actors vs Imdb Score",x="Total Number of Actors", y="Imdb Score",
       colour="Imdb Score") + scale_color_hue(l=10, c=150)+
  theme(plot.title = element_text(color = "black", size = 19, face = "bold", hjust= 0.7),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15, face="bold"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        legend.position="bottom")

#duration_in_hours--------------------------------------------------------------
ggplot(films, aes(x=duration_in_hours, fill= cut(imdb_score,bin2))) +
  geom_histogram(color="black", bins=20)+
  geom_vline(data=films, aes(xintercept=mean(duration_in_hours)), color="blue",size=1.5,
             linetype="dotted")+
  labs(title="Frequency Distribution by Duration Time in Hours",x="Duration Time in Hours", y = "Frequency")+
  theme_minimal() +
  scale_fill_discrete(name = "Imdb Score",
                      labels=c("0 to 1", "1 to 2", "2 to 3", "3 to 4", "4 to 5",
                               "5 to 6", "6 to 7", "7 to 8", "8 to 9", "9 to 10"))+
  theme(plot.title = element_text(color = "black", size = 19, face = "bold", hjust= 0.5),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15, face="bold"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        legend.position="bottom")

##total_number_of_producers-----------------------------------------------------
ggplot(films, aes(x=total_number_of_producers, y=imdb_score)) +geom_boxplot(lwd=1)+
  geom_jitter(alpha=0.15, aes(color=cut(imdb_score,bin2)))+
  theme_gray()+
  labs(title="Total Number of Producers vs Imdb Score",x="Total Number of Producers", y="Imdb Score",
       colour="Imdb Score") + scale_color_hue(l=10, c=150)+
  theme( plot.title = element_text(color = "black", size = 19, face = "bold", hjust= 0.7),
         legend.title = element_text(color = "black", size = 16),
         legend.text = element_text(color = "black", size = 15, face="bold"),
         axis.text=element_text(size=13, face="bold"),
         axis.title=element_text(size=15,face="bold"),
         legend.position="bottom")

##budget_in_millions------------------------------------------------------------
ggplot(films, aes(x=budget_in_millions, y=imdb_score)) +
  geom_jitter(alpha=0.15, aes(color=cut(imdb_score,bin2),))+
  theme_gray()+
  labs(title="Budget vs Imdb Score (in millions)",x="Budget in Millions", y="Imdb Score",
       colour="Imdb Score") + scale_color_hue(l=10, c=150)+
  theme(plot.title = element_text(color = "black", size = 19, face = "bold", hjust= 0.7),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15, face="bold"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        legend.position="bottom")

#main_production_country_new1---------------------------------------------------
ggplot(films, aes(x=main_production_country_new1, y=imdb_score)) +
  geom_boxplot(lwd=1) +
  theme_gray()+
  labs(title="Main Country of Production vs Imdb Score",y="Imdb Score", x="Main Country of Production",
       colour="imdbRating") +
  theme(plot.title = element_text(color = "black", size = 19, face = "bold", hjust= 0.7),
        legend.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 15, face="bold"),
        axis.text=element_text(size=13, face="bold"),
        axis.title=element_text(size=15,face="bold"),
        legend.position="bottom")

#genre_total_count--------------------------------------------------------------
theme_update(plot.title = element_text(hjust = 0.5))
df <- data.frame(genre=genre_type,
                 genre_count=genre_type_count)

p <- ggplot(df, aes(x=genre, y=genre_count, fill=genre)) + geom_bar(stat="identity", color="black") + ggtitle("Frequency distribution of the genres")
p + labs(x="Genre Category", y = "Frequency")
+ theme_gray(base_size = 5)
+ theme(plot.title = element_text(hjust = 0.5))
+ theme(legend.position=c(.9,.75))


#genders of actors and director-------------------------------------------------
Role <- c("main_actor1", "main_actor2", "main_actor3", "main_director")
Role_male_count <- c(count(main_actor1_is_female)[1, ]$freq, count(main_actor2_is_female)[1,]$freq, count(main_actor3_is_female)[1,]$freq, count(main_director_is_female)[1,]$freq)
Role_female_count <- c(count(main_actor1_is_female)[2, ]$freq, count(main_actor2_is_female)[2,]$freq, count(main_actor3_is_female)[2,]$freq, count(main_director_is_female)[2,]$freq)

names(Role_male_count) <- Role
names(Role_female_count) <- Role
barplot(Role_male_count)
barplot(Role_female_count)

install.packages('plyr')
library(plyr)
y = table(movies$main_production_country)
t = as.data.frame(y)
names(t)[1] = 'main_production_country'
t
p <- ggplot(t, aes(x=main_production_country, y=Freq, fill=main_production_country)) + geom_bar(stat="identity", color="black") + ggtitle("Frequency distribution of the main production countries")
p + labs(x="Production country", y = "Frequency") +scale_fill_brewer(palette="Blues")
+ theme_gray(base_size = 5)
+ theme(plot.title = element_text(hjust = 0.5))
+ theme(legend.position=c(.9,.75))

# Final Model Summary ---------------------------------------------------------
install.packages("stargazer")
library(stargazer)
names(We_R8_noout$coefficients)[names(We_R8_noout$coefficients)=="poly(budget_in_millions,4)1"]="budget_in_millions"
names(We_R8_noout$coefficients)[names(We_R8_noout$coefficients)=="poly(budget_in_millions,4)2"]="budget_in_millions<sup>2</sup>"
names(We_R8_noout$coefficients)[names(We_R8_noout$coefficients)=="poly(budget_in_millions,4)3"]="budget_in_millions<sup>3</sup>"
names(We_R8_noout$coefficients)[names(We_R8_noout$coefficients)=="poly(budget_in_millions,4)4"]="budget_in_millions<sup>4</sup>"
names(We_R8_noout$coefficients)[names(We_R8_noout$coefficients)=="poly(year_of_release,3)1"]="year_of_release"
names(We_R8_noout$coefficients)[names(We_R8_noout$coefficients)=="poly(year_of_release,3)2"]="year_of_release<sup>2</sup>"
names(We_R8_noout$coefficients)[names(We_R8_noout$coefficients)=="poly(year_of_release,3)3"]="year_of_release<sup>3</sup>"

stargazer(We_R8_noout,type="html",single.row=TRUE,title="Final Regression Results",
          dep.var.labels = c("IMDB Rating"),digits=2)

#Changing of Adjusted R-Square from Step1 to Step 10---------------------------
a0=summary(We_R)$adj.r.square
a1=summary(We_R1)$adj.r.square
a2=summary(We_R2)$adj.r.square
a3=summary(We_R3)$adj.r.square
a4=summary(We_R4)$adj.r.square
a5=summary(We_R5)$adj.r.square
a6=summary(We_R6)$adj.r.square
a7=summary(We_R7)$adj.r.square
a8=summary(We_R8)$adj.r.square
a9=summary(We_R8_noout)$adj.r.square

steps <- c(1,2,3,4,5,6,7,8,9,10)
value <- c(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
df = data.frame("steps" <- steps,
                "value" <- value)

library(ggplot2)
ggplot(data=df,aes(x=steps,y=value,group=1))+geom_line()+geom_point()+scale_x_continuous("steps",breaks=steps)+
  labs(y='Adjusted R-Square',x="Modelling Steps")+ggtitle("Adjusted R-Square for Models from Step1 to Step10")


