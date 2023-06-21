library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(C50)
library(readr)
data=read_csv("https://raw.github.sydney.edu.au/thol6150/data3888c2/master/covidDataScored.csv?token=AAABQQARVIGQPSYJIBAR2GLCTV6BO")
set.seed(3888)

# repeated vc
ind = sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))
trainset = data[ind == 1,]
testset = data[ind == 2,]
control = trainControl(method = "repeatedcv",number = 10,repeats = 3)
model = train(performanceScore~school_closing+workplace_closing+cancel_public_events+restrictions_on_gatherings+close_public_transport+stay_at_home_requirements+movementrestrictions+internationaltravel,data = trainset,na.action = na.omit,method = "rpart",preProcess = "scale" ,trControl = control)
importance = varImp(model,scale = FALSE)
maxC1 = max(data$school_closing,na.rm = TRUE)
maxC2 = max(data$workplace_closing,na.rm = TRUE)
maxC3 = max(data$cancel_public_events,na.rm = TRUE)
maxC4 = max(data$restrictions_on_gatherings,na.rm = TRUE)
maxC5 = max(data$close_public_transport,na.rm = TRUE)
maxC6 = max(data$stay_at_home_requirements,na.rm = TRUE)
maxC7 = max(data$movementrestrictions,na.rm = TRUE)
maxC8 = max(data$internationaltravel,na.rm = TRUE)
maxtestScore1 = 14*importance$importance[2,]/sum(importance$importance)*maxC8 + 14*importance$importance[4,]/sum(importance$importance) * maxC1 
+ 14*importance$importance[6,]/sum(importance$importance) * maxC2 + 14*importance$importance[1,]/sum(importance$importance) * maxC3 + 14*importance$importance[3,]/sum(importance$importance) * maxC4 +14*importance$importance[8,]/sum(importance$importance)* maxC5 
+ 14*importance$importance[7,] /sum(importance$importance)* maxC7 + 14*importance$importance[5,] /sum(importance$importance)* maxC6
Testscore1=(1 - 
              (14*importance$importance[4,]/sum(importance$importance) * data$school_closing + 14*importance$importance[6,] /sum(importance$importance)* data$workplace_closing
               + 14*importance$importance[1,]/sum(importance$importance) * data$cancel_public_events+ 14*importance$importance[3,]/sum(importance$importance)* data$restrictions_on_gatherings
               + 14*importance$importance[8,]/sum(importance$importance) * data$close_public_transport + 14*importance$importance[5,] /sum(importance$importance)* data$stay_at_home_requirements
               + 14*importance$importance[7,] /sum(importance$importance)* data$movementrestrictions + 14*importance$importance[2,]/sum(importance$importance)*data$internationaltravel)/maxtestScore1)*100





# logistic regression
logistic_function=glm(performanceScore~school_closing+workplace_closing+cancel_public_events+restrictions_on_gatherings+close_public_transport+stay_at_home_requirements+movementrestrictions+internationaltravel,data = trainset,na.action = na.omit)
summary(logistic_function)
maxtestScore2 = 14*logistic_function$coefficients[9]/sum(logistic_function$coefficients[2:9])*maxC8 
+ 14*logistic_function$coefficients[2]/sum(logistic_function$coefficients[2:9]) * maxC1 
+ 14*logistic_function$coefficients[3]/sum(logistic_function$coefficients[2:9])* maxC2 + 14*logistic_function$coefficients[4]/sum(logistic_function$coefficients[2:9])* maxC3 + 14*logistic_function$coefficients[5]/sum(logistic_function$coefficients[2:9])* maxC4 +14*logistic_function$coefficients[6]/sum(logistic_function$coefficients[2:9])* maxC5 
+ 14*logistic_function$coefficients[8]/sum(logistic_function$coefficients[2:9])* maxC7 + 14*logistic_function$coefficients[7]/sum(logistic_function$coefficients[2:9])* maxC6
Testscore2=(1 - 
              (14*logistic_function$coefficients[2]/sum(logistic_function$coefficients[2:9])* data$school_closing + 14*logistic_function$coefficients[3]/sum(logistic_function$coefficients[2:9])* data$workplace_closing
               + 14*logistic_function$coefficients[4]/sum(logistic_function$coefficients[2:9])* data$cancel_public_events+ 14*logistic_function$coefficients[5]/sum(logistic_function$coefficients[2:9])* data$restrictions_on_gatherings
               + 14*logistic_function$coefficients[6]/sum(logistic_function$coefficients[2:9])* data$close_public_transport + 14*logistic_function$coefficients[7]/sum(logistic_function$coefficients[2:9])* data$stay_at_home_requirements
               + 14*logistic_function$coefficients[8]/sum(logistic_function$coefficients[2:9])* data$movementrestrictions+ 14*logistic_function$coefficients[9]/sum(logistic_function$coefficients[2:9])*data$internationaltravel)/maxtestScore2)*100




# Random forest
library(randomForest)
rf<-randomForest(performanceScore~school_closing+workplace_closing
                 +cancel_public_events+restrictions_on_gatherings
                 +close_public_transport+stay_at_home_requirements
                 +movementrestrictions+internationaltravel, data=trainset, mtry=7, ntree=100, importance=T,na.action = na.omit )
rf_sum=sum(rf$importance[1:8])
maxtestScore3 = 14*rf$importance[8]/rf_sum*maxC8 
+ 14*rf$importance[1]/rf_sum * maxC1 
+ 14*rf$importance[2]/rf_sum* maxC2 + 14*rf$importance[3]/rf_sum* maxC3 
+ 14*rf$importance[4]/rf_sum* maxC4 +14*rf$importance[5]/rf_sum* maxC5 
+ 14*rf$importance[7]/rf_sum* maxC7 + 14*rf$importance[6]/rf_sum* maxC6

Testscore3=(1 - 
              (14*rf$importance[1]/rf_sum* data$school_closing + 14*rf$importance[2]/rf_sum* data$workplace_closing
               + 14*rf$importance[3]/rf_sum* data$cancel_public_events
               + 14*rf$importance[4]/rf_sum* data$restrictions_on_gatherings
               + 14*rf$importance[5]/rf_sum* data$close_public_transport + 14*rf$importance[6]/rf_sum* data$stay_at_home_requirements
               + 14*rf$importance[7]/rf_sum* data$movementrestrictions+ 14*rf$importance[8]/rf_sum*data$internationaltravel)/maxtestScore3)*100

# normalization using Linear transformation(because of the existence of negative numbers, log cannot be used for transformation)
t1=(Testscore1-min(Testscore1,na.rm = TRUE))/(max(Testscore1,na.rm = TRUE)-min(Testscore1,na.rm = TRUE))*100
t2=(Testscore2-min(Testscore2,na.rm = TRUE))/(max(Testscore2,na.rm = TRUE)-min(Testscore2,na.rm = TRUE))*100
t3=(Testscore3-min(Testscore3,na.rm = TRUE))/(max(Testscore3,na.rm = TRUE)-min(Testscore3,na.rm = TRUE))*100

boxplot(t1)
boxplot(t2)
boxplot(t3)

# correlation matrix
library(corrgram)
df=data.frame(data$school_closing,
              data$workplace_closing,
              data$cancel_public_events,
              data$restrictions_on_gatherings,
              data$close_public_transport,
              data$stay_at_home_requirements,
              data$movementrestrictions,
              data$internationaltravel,
              data$income_support,
              data$debtrelief,
              data$public_information_campaigns,
              data$testing_policy,
              data$contact_tracing,
              data$facial_coverings,
              data$vaccination_policy,
              data$protection_of_elderly_people)
corrgram(df,
         cex.labels = 0.8,         
         lower.panel="panel.fill",   
         upper.panel="panel.cor",   
         diag.panel="panel.density",
         cor.method="pearson") 



