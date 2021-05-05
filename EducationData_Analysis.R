library(corrplot)
library(mlbench)
library(ggpubr)
library(AppliedPredictiveModeling)
library(caret)
library(e1071) # misc library including skewness function
library(dplyr)
library(ISLR)
library(pls)
library(elasticnet)
library(tidyr)
library(lattice)
library(randomForest)
library(MASS)
library(pROC)
library(glmnet)
library (ggplot2)
library(DBI)
library(RANN)
library(mice)
library(naniar)
library(reshape2)

#Data Conditoning and Pre Processing----------------------------------------------------------------------------------------------------------------

#Need to perform data imputation on missin values 

    #Load Data
db <- 'DB_EDUCATION'
host_db <- 'dbeducation.cvqihokz5t3s.us-east-1.rds.amazonaws.com'
db_port <- '5432'
db_user <- '##'
db_password <- '##'
con <- dbConnect(RPostgres::Postgres(), dbname = db, host = host_db,
                 
                 port = db_port, user = db_user, password = db_password)

fosd <- dbReadTable(con, "fos_imputed")

institution <- dbReadTable(con, "institution")
institutionDF <- institution[ ,c(2:4,14:93)]


#Field of Study Imputation-------------------------------
input_data = fosd

fosdImpute = mice(input_data, m=5, method ="pmm", maxit=3)

summary(input_data$debt_all_stgp_any_mdn)
fosdImpute$imp$debt_all_stgp_any_mdn


finalFosdImpute = complete(fosdImpute,5)
View(finalFosdImpute)
  
setwd("/Users/saichindepalli/Desktop")
write.csv(finalFosdImpute, "finalFosdImpute.csv")



#Institution Data Imputation-------------------------------

input_data2 = institutionDF

my_imp2 = mice(input_data2, m=2, method ="cart", maxit=2)

summary(input_data2$sat_avg)
my_imp2$imp$sat_avg

insImpute = complete(my_imp2,1)
View(insImpute)

summary(insImpute$sat_avg)

setwd("/Users/saichindepalli/Desktop")
write.csv(insImpute, "finalInstitutionImpute.csv")

#Field of Data Analysis#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ff <- read.csv("/Users/saichindepalli/Desktop/Imputed_Data/finalFosdImpute.csv", header = TRUE)
FOSD <- as.data.frame(ff)

    #Correlation Analys

fosdCor <- cor(FOSD[ ,9:25], method = "pearson")
  View(cor(fosdCor) %>%
         as.data.frame() %>%
       mutate(var1 = rownames(.)) %>%
         gather(var2, value, -var1) %>%
          arrange(desc(value)) %>%
          group_by(value) %>%
        filter(row_number()==1))
    
  boxplot(FOSD, horizontal = TRUE)


fosdCorA <- fosdCor
  fosdCorA[lower.tri(fosdCorA)] <- NA
  melt_cor <- melt(fosdCorA, na.rm=TRUE)
  ggplot(data = melt_cor, aes(Var2, Var1, fill = value)) + 
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high= "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1)) + 
    coord_fixed()
 
#Linear Regression----------------------------------------------------------------------------------------------------------------------
  #PLS - All Predictors  - Field of Study
  FOSDNM <- FOSD[ ,9:25]
  trainingRows <- createDataPartition(y = FOSDNM$debt_all_stgp_any_mdn, p = .8,list= FALSE)
  
  trainData  <- FOSDNM[trainingRows, ]
  testData <- FOSDNM[-trainingRows, ]
  
  set.seed(622)
  plsModel <- train(debt_all_stgp_any_mdn ~.,data = trainData, method = "pls",
                    trControl = trainControl(method = "repeatedcv", repeats = 5))
  plsModel
  
  plot(varImp(plsModel))
  
  
  # tuneGrid = expand.grid(ncomp = 1:8),
  ?varImp
  
  plsPredPLS <- predict(plsModel, testData) 
  lmValuesPLS = data.frame(obs = testData$debt_all_stgp_any_mdn, pred = plsPredPLS)
  defaultSummary(lmValuesPLS) 
  
  
 #Data Partition - Linear Regression 1 
FOSDNM <- FOSD[ ,9:25]

trainingRows <- createDataPartition(y = FOSDNM$debt_all_stgp_any_mdn, p = .8,list= FALSE)

trainData  <- FOSDNM[trainingRows, ]
testData <- FOSDNM[-trainingRows, ]


     #Linear Model - 1
set.seed(622)
lmModel <- train(debt_all_stgp_any_mdn ~., data = trainData, method = "lm", 
                 trControl = trainControl(method = "LGOCV"))
lmModel


lmPredict <- predict(lmModel, testData) 
lmValues = data.frame(obs = testData$debt_all_stgp_any_mdn, pred = lmPredict)
#View(lmValues)
defaultSummary(lmValues)


plot(lmValues) 

ggplot(lmValues) + geom_point(col="blue") 
+ geom_text(hjust=-.5, vjust=0) + 
  labs(title="KNN Model Tuned with optimal K and resampled with 10 fold repeated CV ")


#Data Partition - Linear Regression 2
FOSDNM <- FOSD[ ,9:25]

trainingRows <- createDataPartition(y = FOSDNM$earn_count_nwne_hi_1yr, p = .8,list= FALSE)

trainData  <- FOSDNM[trainingRows, ]
testData <- FOSDNM[-trainingRows, ]


#Linear Model - 2
set.seed(622)
lmModel2 <- train(earn_count_nwne_hi_1yr ~., data = trainData, method = "lm", 
                 trControl = trainControl(method = "LGOCV"))
lmModel2


lmPredict2 <- predict(lmModel2, testData) 
lmValues2 = data.frame(obs = testData$earn_count_nwne_hi_1yr, pred = lmPredict2)
#View(lmValues2)
defaultSummary(lmValues2) 

  #Data Partition - Linear Regression 3 
FOSDNM <- FOSD[ ,9:25]
trainingRows <- createDataPartition(y = FOSDNM$earn_count_wne_hi_1yr, p = .8,list= FALSE)

trainData  <- FOSDNM[trainingRows, ]
testData <- FOSDNM[-trainingRows, ]

#Linear Model - 3
set.seed(622)
lmModel3 <- train(earn_count_wne_hi_1yr ~., data = trainData, method = "lm", 
                  trControl = trainControl(method = "LGOCV"))
lmModel3


lmPredict3 <- predict(lmModel3, testData) 
lmValues3 = data.frame(obs = testData$earn_count_wne_hi_1yr, pred = lmPredict3)
#View(lmValues3)
defaultSummary(lmValues3) 
  
  
  #Data Partition - Linear Regression 4 
FOSDNM <- FOSD[ ,9:25]
trainingRows <- createDataPartition(y = FOSDNM$earn_mdn_hi_1yr, p = .8,list= FALSE)

trainData  <- FOSDNM[trainingRows, ]
testData <- FOSDNM[-trainingRows, ]


#Linear Model - 4
set.seed(622)
lmModel4 <- train(earn_mdn_hi_1yr ~., data = trainData, method = "lm", 
                  trControl = trainControl(method = "LGOCV"))
lmModel4


lmPredict4 <- predict(lmModel4, testData) 
lmValues4 = data.frame(obs = testData$earn_mdn_hi_1yr, pred = lmPredict4)
#View(lmValues4)
defaultSummary(lmValues4) 


#Data Partition - Linear Regression 5
FOSDNM <- FOSD[ ,9:25]
trainingRows <- createDataPartition(y = FOSDNM$debt_all_stgp_any_mdn, p = .8,list= FALSE)

trainData  <- FOSDNM[trainingRows, ]
testData <- FOSDNM[-trainingRows, ]


#Linear Model - 5
set.seed(622)
lmModel5 <- train(debt_all_stgp_any_mdn ~ earn_count_wne_hi_1yr, data = trainData, method = "lm", 
                  trControl = trainControl(method = "LGOCV"))
lmModel5


lmPredict5 <- predict(lmModel5, testData) 
lmValues5 = data.frame(obs = testData$earn_mdn_hi_1yr, pred = lmPredict5)
#View(lmValues5)
defaultSummary(lmValues5) 

  




#Institution Data Analysis#-------------------------------------------------------------------------------------------------------------------------------------------------------------
dd <- read.csv("/Users/saichindepalli/Desktop/Imputed_Data/finalInstitutionImpute.csv", header = TRUE)
insData <- as.data.frame(dd)

#summary(insData$debt_mdn)

insDataN<- insData %>% mutate_if(is.factor, as.numeric)
#sapply(insDataN, class)

#Correlation Analysis
insDataF <- insData[ ,c(1:72, 80,84)]
insCor <- cor(insDataF, method = "pearson")
View(cor(insCor) %>%
       as.data.frame() %>%
       mutate(var1 = rownames(.)) %>%
       gather(var2, value, -var1) %>%
       arrange(desc(value)) %>%
       group_by(value) %>%
       filter(row_number()==1))

boxplot(FOSD, horizontal = TRUE)


insCorA <- insCor
insCorA[lower.tri(insCorA)] <- NA
melt_cor2 <- melt(insCorA, na.rm=TRUE)
ggplot(data = melt_cor2, aes(Var2, Var1, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high= "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) + 
  coord_fixed()

#Linear Regression Institution Data----------------------------------------------------------------------------------------------------------------------
#Data Partition - Linear Regression 1

insDataF <- insData[ ,c(1:72, 80,84)]
trainingRows <- createDataPartition(y = insDataF$debt_mdn, p = .8,list= FALSE)

trainData  <- insDataF[trainingRows, ]
testData <- insDataF[-trainingRows, ]

#PLS - All Predictors Institution Data---------------------------------
set.seed(622)
plsModelI1 <- train(debt_mdn ~., data = trainData, method = "pls",
                  trControl = trainControl(method = "repeatedcv", repeats = 5))
plsModelI1

plot(varImp(plsModelI1))  

plsPredPLS <- predict(plsModel, testData) 
lmValuesPLS = data.frame(obs = testData$earn_count_wne_hi_1yr, pred = plsPredPLS)
defaultSummary(lmValuesPLS) 


#Linear Model - 1
set.seed(622)
lmModelI1 <- train(debt_mdn ~., data = trainData, method = "lm", 
                 trControl = trainControl(method = "LGOCV"))
lmModelI1


lmPredictI1 <- predict(lmModelI1, testData) 
lmValuesI1 = data.frame(obs = testData$debt_mdn, pred = lmPredictI1)
#View(lmValues)
defaultSummary(lmValuesI1)


#Linear Model - 2
set.seed(622)
lmModelI2 <- train(debt_mdn ~ sat_avg, data = trainData, method = "lm", 
                   trControl = trainControl(method = "LGOCV"))
lmModelI2


lmPredictI2 <- predict(lmModelI2, testData) 
lmValuesI2 = data.frame(obs = testData$debt_mdn, pred = lmPredictI2)
#View(lmValues)
defaultSummary(lmValuesI2)

#Linear Model - 3
set.seed(622)
lmModelI3 <- train(debt_mdn ~ grad_debt_mdn10yr, data = trainData, method = "lm", 
                   trControl = trainControl(method = "LGOCV"))
lmModelI3


lmPredictI3 <- predict(lmModelI3, testData) 
lmValuesI3 = data.frame(obs = testData$debt_mdn, pred = lmPredictI3)
#View(lmValues)
defaultSummary(lmValuesI3)

#Linear Model - 4
set.seed(622)
lmModelI4 <- train(debt_mdn ~ tuitionfee_in, data = trainData, method = "lm", 
                   trControl = trainControl(method = "LGOCV"))
lmModelI4

lmPredictI4 <- predict(lmModelI4, testData) 
lmValuesI4 = data.frame(obs = testData$debt_mdn, pred = lmPredictI4)
#View(lmValues)
defaultSummary(lmValuesI4)

#Linear Model - 5
set.seed(622)
lmModelI5 <- train(debt_mdn ~ grad_debt_mdn10yr + mn_earn_wne_p10, data = trainData, method = "lm", 
                   trControl = trainControl(method = "LGOCV"))
lmModelI5

lmPredictI5 <- predict(lmModelI5, testData) 
lmValuesI5 = data.frame(obs = testData$debt_mdn, pred = lmPredictI5)
#View(lmValues)
defaultSummary(lmValuesI5)






 
