# Load Data 
allData <- read.csv("data/ign.csv")
str(allData)

q.allData <- allData

# Data preparation 

q.allData$release_year <- as.factor(q.allData$release_year)
q.allData$release_month <- as.factor(q.allData$release_month)
q.allData$release_day <- as.factor(q.allData$release_day)
q.allData$editors_choice <- as.factor(q.allData$editors_choice)


# Question 1 : Analyze and list the platforms with the most "Editor's Choice" awards?
table(q.allData$platform,q.allData$editors_choice)

library(gmodels)
CrossTable(q.allData$platform,q.allData$editors_choice)

library(ggplot2)
ggplot(q.allData, aes(platform, ..count..)) + geom_bar(aes(fill = editors_choice), position = "dodge")

library(plyr)

q.allData$editors_choice <- as.integer(allData$editors_choice)
q.allData$editors_choice <- q.allData$editors_choice - 1

result1 <- aggregate(q.allData$editors_choice, by=list(platform=q.allData$platform), FUN=sum)
result1 <- result1[order(result1$x,decreasing=TRUE),]
result1

# Question 2 : Does number of games by a platform in a given year have any effect on these awards?
q.allData$editors_choice <- as.factor(allData$editors_choice)
ggplot(q.allData, aes(release_year, ..count..)) + geom_bar(aes(fill = platform), position = "dodge")


ggplot(q.allData, aes(release_year, ..count..)) + geom_bar(aes(fill = editors_choice), position = "dodge")+
  facet_wrap(~platform)

table(allData$editors_choice,allData$release_year,allData$platform)

# Answer : No, number of games by a platform in a given year not any effect on these awards.Its dependency has been show in the model prediction.
#          It depends mostly on score and score_phrase



# Question 3 : What is Macintosh's average award count?

result3 <- q.allData[q.allData$platform=="Macintosh",]
result3 <- aggregate(as.integer(result3$editors_choice), by=list(year=result3$release_year), FUN=sum)
result3


# Question 4 : What is the optimal month for releasing a game?

result4 <- aggregate(as.integer(q.allData$editors_choice), by=list(month=q.allData$release_month), FUN=sum)
result4 <- result4[order(result4$x,decreasing=TRUE),]
result4


# Question 5 : Analyze the percentage growth in the gaming industry over the years ?

result5 <- aggregate(as.integer(q.allData$editors_choice), by=list(year=q.allData$release_year), FUN=sum)
result5 <- result5[order(result5$x,decreasing=FALSE),]

growth <- function(x)((x*100)/lag(x)-1)-100

library(magrittr)
library(dplyr)
result5 %>% mutate_each(funs(growth), x)




