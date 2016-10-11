# Load Data 
allData <- read.csv("data/ign.csv")
str(allData)

allData$release_year <- as.factor(allData$release_year)
allData$release_month <- as.factor(allData$release_month)
allData$release_day <- as.factor(allData$release_day)

## Step 1 : Univarite Analysis

  # 1: Continuous Variable ( X, score)


  library(pastecs)
  stat.desc(allData$score)
  summary(allData$score)

  # 2: Categorical Varibale (score_phrase, title, url, genre, editors_choice, release_year, release_month, release_day) )

  cat.name <- c("score_phrase", "title", "url","genre", "editors_choice", "release_year", "release_month", "release_day")
  allData.cat <- allData[,cat.name] 
  apply(allData.cat,2,function(x){length(unique(x))})
  table(allData.cat$score_phrase,allData.cat$editors_choice)
  as.matrix((prop.table(table(allData.cat$score_phrase,allData.cat$editors_choice))))
  table(allData.cat$release_year,allData.cat$editors_choice)
  as.matrix((prop.table(table(allData.cat$release_year,allData.cat$editors_choice))))
  
## Step 2 : Multivariate Analysis
  
  # 1. Both Categorical
  
  cat.name
  library(gmodels)
  CrossTable(allData.cat$score_phrase,allData.cat$editors_choice)
  CrossTable(allData.cat$release_year,allData.cat$editors_choice)
  CrossTable(allData.cat$release_month,allData.cat$editors_choice)
  
  library(ggplot2)
  ggplot(allData.cat, aes(release_year, ..count..)) + geom_bar(aes(fill = editors_choice), position = "dodge")
  ggplot(allData.cat, aes(score_phrase, ..count..)) + geom_bar(aes(fill = editors_choice), position = "dodge")
  
  # 2. Both Continuous - There is no Continuous pair of feature variable
  
  # 3. Categorical-Continuous Combination
  
  ggplot(allData,aes(score,editors_choice)) + geom_boxplot()
  ggplot(allData, aes(score,editors_choice)) + geom_jitter()
  ggplot(allData, aes(score,platform)) + geom_jitter()
  
  
## Step 3: Missing Value Treatment
  
  summary(allData)
  sum(is.na(allData))
  
## Step 4: Outlier Treatment - There is no such outlier in the dataset
  
  boxplot(allData$score)
  
## Step 5: Feature Engineering
  
  summary(allData$score)
  allData$score <- allData$score * 10
  
## Step 6: Model Implementaion 
  
  #1. Prepare Data for modeling 
  
  xg.allData <- allData
  str(xg.allData)
  
  xg.allData$X <- NULL
  xg.allData$title <- NULL
  xg.allData$release_day <- NULL
  xg.allData$url <- NULL
  
  xg.allData$score_phrase <- as.numeric(xg.allData$score_phrase)
  xg.allData$platform <- as.numeric(xg.allData$platform)
  xg.allData$genre <- as.numeric(xg.allData$genre)
  xg.allData$editors_choice <- as.numeric(xg.allData$editors_choice)
  xg.allData$editors_choice <- xg.allData$editors_choice-1
  
  #2. Spliting Data into train(65%) and test dataset(35%)
  library(caTools)
  
  set.seed(1000)
  split = sample.split(xg.allData$editors_choice, SplitRatio = 0.65)
  train = subset(xg.allData, split==TRUE)
  test = subset(xg.allData, split==FALSE)
  
  feature.names <- names(train)
  feature.names <- feature.names[feature.names!= "editors_choice"]
  
  #3 Build Xgboost model
  library(xgboost)
  
  set.seed(1960)
  h<-sample(nrow(train),floor(0.3*nrow(train)))
  train_sample <- train[-h,]
  train_val <- train[h,]
  
  dval<-xgb.DMatrix(data=data.matrix(train_val[,feature.names]),label=train_val[,5],missing=NA)
  
  dtrain<-xgb.DMatrix(data=data.matrix(train_sample[,feature.names]),label=train_sample[,5],missing=NA)
  
  watchlist<-list(val=dval,train=dtrain)
  
  param <- list(  objective           = "binary:logistic",
                  booster             = "gbtree",
                  eta                 = 0.48,
                  max_depth           = 4, #7
                  subsample           = 0.9,
                  colsample_bytree    = 0.9
  )
  
  set.seed(1429)
  clf <- xgb.train(   params              = param,
                      data                = dtrain,
                      nrounds             = 2000,
                      verbose             = 0,
                      early.stop.round    = 100,
                      watchlist           = watchlist,
                      maximize            = TRUE
  )
  
  #4 . See the Feature Importance
  library(Ckmeans.1d.dp)
  importance_matrix <- xgb.importance(feature.names, model = clf)
  xgb.plot.importance(importance_matrix)
  
  #5 . Predict the test dataset
  pred <- predict(clf,data.matrix(test[,feature.names]))
  prediction <- as.numeric(pred > 0.5)
  
  #6. Accuracy 
  acc <- table(prediction,test$editors_choice)
  percentAccuracy <- (acc[1,1]+acc[2,2])/(nrow(test))
  percentAccuracy
  
  