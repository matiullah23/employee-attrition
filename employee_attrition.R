### Hypothesis Testing ###

## According to a Gallup Poll in 2018, 21% of millenials have left their job in the past year (poll was conducted in 2018 so this falls within our dataset time of 2017). The hypothesis that we will be testing is that the IBM dataset millenials are not in line with the nationwide average. H0: attrition for millenials is equal to .21, h1: attrition for millenials is not equal to .21. 

# import dependencies 
library(dplyr)

hypData <- read.csv('data/employees.csv',fileEncoding="UTF-8-BOM")

#subset data that will be used in hypothesis testing


# subset our data by millenials and not millenials
millenials <- hypData %>% filter(hypData$Age %in% (25:34))
notMillenials <- hypData %>% filter(!hypData$Age %in% (25:34))

# taking a look at the data, we see that millenials are significantly higher attrition rate, but still below the national average of 0.21
summary(millenials$Attrition)
x <- 112/(442+112) # attrition rate for millenials
x
summary(notMillenials$Attrition)
125/(791+125)

# z testing for confidence interval

n <- nrow(millenials)
phat <- .21 # population annual attrition rate for millenials
z <- 1.96 # z-score for 95% confidence interval
SE <- sqrt(phat*(1-phat)/n)

upperBound <- phat+z*SE
upperBound

lowerBound <- phat-z*SE
lowerBound


# test to see if x is between confidence interval
ifelse(x > lowerBound & x < upperBound, print("yes"), print("no"))

# since sample mean of x, 0.2, falls between the confidence interval, we fail to reject the null hypothesis


### Predicition Model ###

#import dependencies
library(tidyverse)
library(MASS)
library(tree)
library(randomForest)
library(shiny)
library(zeallot)
library(broom)
library(shinythemes)


## Data Pre-processing
# Read in the data
data <- read.csv('data/employees.csv',fileEncoding="UTF-8-BOM")

# check to see if there is missing data
colSums(is.na(data))

# drop columns that do not provide any predicting value
unique(data$EmployeeCount) # verify that there is only one unique value in this column
unique(data$Over18) # verify that there is only one unique value in this column
unique(data$StandardHours) # verify that there is only one unique value in this column
data<- within(data, rm(EmployeeCount, EmployeeNumber, Over18, StandardHours))

# conduct exploratory data analysis
dim(data)
head(data)
str(data)

# filter data based on whether the employee attritted
attritted <- filter(data, Attrition == "Yes")
employed <- filter(data, Attrition == "No")

# plot histograms 
hist(attritted$HourlyRate, breaks = 20)
hist(employed$HourlyRate, breaks= 20)



## Build Model

# split data into training and testing data

set.seed(123)
sample <- sample.int(n = nrow(data), size =.5*nrow(data), replace=F)
train <- data[sample,]
test <- data[-sample,]


glm.fits <- glm(Attrition~.,data=train,family=binomial())
summary(glm.fits)
coef(glm.fits)

glm.probs <- predict(glm.fits, type="response")
glm.probs[0:10]

contrasts(data$Attrition) #validate that the dummy variable for Attrition indicates that 0 = No and 1 = Yes.

#create a vector of class predictions 
glm.pred=rep("No", 735)
glm.pred[glm.probs>.5]="Yes"

#create a confusion matrix
cm <- table(glm.pred, train$Attrition)

#check training accuracy
(602+51)/735
mean(glm.pred==train$Attrition)

# calculate the training error rate
1-((602+51)/735)

## predict on the test data
glm.probs <- predict(glm.fits,test,type="response") 

#create a vector of class predictions 
glm.pred=rep("No", 735)
glm.pred[glm.probs>.5]="Yes"

#create a confusion matrix
cm <- table(glm.pred, test$Attrition)

mean(glm.pred == test$Attrition)


#create a function to obtain model metrics 
metrics <- function(confusionmatrix) {
  
  n = sum(confusionmatrix) # number of instances
  nc = nrow(confusionmatrix) #number of classes
  diag = diag(confusionmatrix) # number of correctly classified instances per class
  rowsums = apply(confusionmatrix,1,sum) # number of instances per class
  colsums = apply(confusionmatrix,2,sum) # number of predictions per class
  p = rowsums / n # distribution of instances of the actual classes
  q = colsums / n # distribution of instances of the predicted classes
  accuracy= sum(diag)/n # how accurate is the model
  precision= diag / colsums # how accurate is the model when prediction is "Yes" Attrition
  recall= diag/rowsums # what percentage of "Yes" attritions did model recognize
  f1= 2 * precision * recall / (precision+recall)
  
  list(
    accuracy,
    precision,
    recall,
    f1
  )
}

# calculate the test error rate and other metrics
c(glm_accuracy,glm_precision,glm_recall,glm_f1) %<-% metrics(cm)


## recreate the model using stepwise logistic regression
step.model <- glm.fits %>% stepAIC(trace=FALSE)
summary(step.model)
step.probs <- predict(step.model,test,type="response")
step.pred=rep("No", 735)
step.pred[step.probs>.5]="Yes"
step_cm <- table(step.pred, test$Attrition)

c(step_accuracy,step_precision,step_recall,step_f1) %<-% metrics(step_cm)



## recreate the model using reduced predictors
Attritions = data
Attritions$TravelFrequently = 0
Attritions$TravelFrequently[Attritions$BusinessTravel=="Travel_Frequently"]=1
Attritions$LifeSciences=0
Attritions$LifeSciences[Attritions$EducationField==	"Life Sciences"]=1
Attritions$Medical=0
Attritions$Medical[Attritions$EducationField==	"Medical"]=1
Attritions$Other=0
Attritions$Other[Attritions$EducationField==	"Other"]=1
Attritions$LaboratoryTechnician=0
Attritions$LaboratoryTechnician[Attritions$JobRole == "Laboratory Technician"]=1
Attritions$Single=0
Attritions$Single[Attritions$MaritalStatus==	"Single"]=1
Attritions$OvertimeYes=0
Attritions$OvertimeYes[Attritions$OverTime==	"Yes"]=1

set.seed(13)
sample <- sample.int(n = nrow(Attritions), size =.5*nrow(Attritions), replace=F)
Attritions_train <- Attritions[sample,]
Attritions_test <- Attritions[-sample,]

reduced = glm(Attrition ~ TravelFrequently + LifeSciences+ Medical+ Other+ LaboratoryTechnician+ Single+ OvertimeYes+ Age+ EnvironmentSatisfaction+ JobInvolvement+ JobSatisfaction+ NumCompaniesWorked+ PerformanceRating+ TrainingTimesLastYear+ YearsAtCompany+ YearsInCurrentRole+ YearsWithCurrManager, family=binomial, data=Attritions_train)
summary(reduced)

reduced.probs <- predict(reduced, Attritions_test,type="response")
reduced.pred=rep("No", 735)
reduced.pred[reduced.probs>.5]="Yes"
red_cm <- table(reduced.pred, Attritions_test$Attrition)

c(red_accuracy,red_precision,red_recall,red_f1) %<-% metrics(red_cm)




bestmodel <- step.model# the best model is the step model due to its superior precision. The other metrics are very close between the original model and step model. The backwards model was not seriously considered due to its lower accuracy and lower precision in predicting who attritted. 

## try decision trees to see if model is better

# fit a classification tree in order to predict attrition
tree.attrition = tree(Attrition~.,train)
summary(tree.attrition)

#estimate the test error
tree.pred=predict(tree.attrition, test, type="class")
tree_cm<- table(tree.pred,test$Attrition)

c(tree_accuracy,tree_precision,tree_recall,tree_f1) %<-% metrics(tree_cm)


## fit a pruned tree and obtain test metrics
set.seed(24)
cv.attrition <- cv.tree(tree.attrition,FUN=prune.misclass)
names(cv.attrition)
cv.attrition

#plot the error rate as a function of both size and k
par(mfrow=c(1,2))
plot(cv.attrition$size,cv.attrition$dev,type='b')
plot(cv.attrition$k,cv.attrition$dev,type='b')

#prune the tree to obtain the 14-node tree (14 is chosen since the plot shows it has the lowest dev which represents the test error)
prune.tree=prune.misclass(tree.attrition,best=14)
plot(prune.tree)
text(prune.tree,pretty=0)

#check performance of pruned tree on test set
tree.pred = predict(prune.tree, test, type="class")
prune_cm <- table(tree.pred, test$Attrition)

c(prune_accuracy,prune_precision,prune_recall,prune_f1) %<-% metrics(prune_cm)


## fit a bagged RF model
bag.attrition = randomForest(Attrition~.,data=train,mtry=31,importance=TRUE)
bag.pred = predict(bag.attrition,newdata=test)
bag_cm <- table(bag.pred,test$Attrition)

c(bag_accuracy,bag_precision,bag_recall,bag_f1) %<-% metrics(bag_cm)


#change the number of trees grown
rf.attrition = randomForest(Attrition~.,data=train,mtry=31,ntree=25)
rf.pred = predict(rf.attrition,newdata=test)
changed_cm <- table(rf.pred,test$Attrition)

c(changed_accuracy,changed_precision,changed_recall,changed_f1) %<-% metrics(changed_cm)




### final comparision of metrics of all models



# create a dataframe to compare the metrics easier
model <- c('Logistic Regression','Stepwise Logistic Regression','Reduced Logistic Regression','Decision Tree', 'Pruned Decision Tree', 'Bagged Random Forest', 'Changed Random Forest')
accuracy <- c(glm_accuracy, step_accuracy, red_accuracy, tree_accuracy, prune_accuracy, bag_accuracy, changed_accuracy)
precision <- c(glm_precision[2], step_precision[2], red_precision[2], tree_precision[2], prune_precision[2], bag_precision[2], changed_precision[2])
recall <- c(glm_recall[2], step_recall[2], red_recall[2], tree_recall[2], prune_recall[2], bag_recall[2], changed_recall[2])
f1 <- c(glm_f1[2], step_f1[2], red_f1[2], tree_f1[2], prune_f1[2], bag_f1[2], changed_f1[2])
metrics_df <- data.frame(model, accuracy, precision,recall,f1)

arrange(metrics_df,desc(accuracy)) #  Accuracy measures the overall percentage of correct predictions for both Yes and No when predicting attrition. The most accurate model is the stepwise logistic regression model.
arrange(metrics_df,desc(precision)) # Precision measures how often the model is correct when predicting 'Yes' for attrition. While these precision scores may seem low, it should be noted that high precision is difficult to obtain in unbalanced class sizes. In this case, the imbalance is very clear as 16% of the employees in the dataset attritted, while the other 84% remain employed. The model w/ the best precision score is the logistic regression model followed by the stepwise model.
arrange(metrics_df,desc(recall)) # Recall measures the proportion of the actual 'Yes' values of attrition the model predicted, i.e the Stepwise model accurately identified 71.*% of the actual "Yes" attrition values. The stepwise logisitic regression model is middle of the pack in terms of recall; although all the scores in the top 4 are very close. 
arrange(metrics_df,desc(f1)) # The f1 score is a mean of the recall and precision scores. f1 score is considered the best metric as it takes into consideration a portion of the other metrics. The stepwise model has the best f1 score.

#compare the aic of the models
glm.fits$aic
step.model$aic
reduced$aic

# while the reduced model has the best AIC, its precision score was too low so it has been ruled out as a possibliitiy. Comparing the remaining two, the Stepwise model has a better score than the basic logisitic regression model.

# none of the tree models were impressive compared to logistic regression models, the best model does not change


### SHINY APP ###
ui <- fluidPage(
  fluidRow(
    titlePanel(
      h1("Employee Attrition Predictor", align="center")
    )
  ),
  hr(),
  theme = shinytheme("superhero"),
  mainPanel(
    
    tabsetPanel(type="tabs",
                tabPanel("Single Prediction",
                         hr(),
                         textOutput("Pred"),
                         hr(),
                         fluidRow(
                           column(3,
                                  h4("Input/Select Predictors"),
                                  numericInput(inputId = 'Age', label = 'Age', value = 18, min = 18, max = 99, step = NA, width = NULL),
                                  selectInput(inputId = 'BusinessTravel', label = 'Business Travel', c("Travel_Rarely", "Travel_Frequently", "Non-Travel"), selected = "Non-Travel"),
                                  numericInput(inputId = 'DistanceFromHome', label = 'Distance From Home', value = 7, min = 0, max = 330, step = NA, width = NULL),
                                  numericInput(inputId = 'Education', label = 'Education Level', value = 0, min = 0, max = 5, step = NA, width = NULL),
                                  selectInput(inputId = 'EducationField', label = 'Education Field', c("Human Resources", "Life Sciences", "Marketing", "Medical", "Other", "Technical Degree"), selected = "Medical")
                                  
                           ),
                           column(3, br(), br(), br(),
                                  numericInput(inputId = 'EnvironmentSatisfaction', label = 'Environment Satsifaction', value = 3, min = 1, max = 4, step = NA, width = NULL),
                                  numericInput(inputId = 'JobInvolvement', label = 'Job Involvement', value = 3, min = 1, max = 4, step = NA, width = NULL),
                                  selectInput(inputId = "JobRole", label = "Job Role", c("Healthcare Representative", "Human Resources", "Labratory Technician", "Manager", "Manufacturing Director", "Research Director", "Research Scientist", "Sales Executive", "Sales Representative"), selected = "Manager"),
                                  numericInput(inputId = "JobSatisfaction", label = "Job Satisfaction", value = 3, min = 1, max =4, step = NA, width = NULL),
                                  selectInput(inputId = "MaritalStatus", label = "Marital Status", c("Divorced", "Married", "Single"), selected = "Married"),
                                  
                           ),
                           column(3, br(), br(), br(),
                                  numericInput(inputId = "NumCompaniesWorked", label = "Number of Companies Worked", value = 2, min = 0, max = 20, step = NA, width = NULL),
                                  selectInput(inputId = "OverTime", label = "Overtime", c("No", "Yes"), selected = "No"),
                                  numericInput(inputId = "PercentSalaryHike", label = "Salary Hike (%)", value = 14, min = 0, max = 100, step = NA, width = NULL),
                                  numericInput(inputId = "PerformanceRating", label = "Performance Rating", value = 3, min = 1, max = 5, step = NA, width = NULL),
                                  numericInput(inputId = "TrainingTimesLastYear", label = "Number of Trainings Attended Last Year", value = 3, min = 0, max = 50, step = NA, width = NULL)
                           ),
                           column(3, br(), br(), br(),
                                  numericInput(inputId = "WorkLifeBalance", label = "Work Life Balance Satisfaction", value = 3, min = 1, max = 4, step = NA, width = NULL),
                                  numericInput(inputId = "YearsAtCompany", label = "Number of Years at Company", value = 15, min = 0, max = 99, step = NA, width = NULL),
                                  numericInput(inputId = "YearsInCurrentRole", label = "Number of Years in Current Role", value = 3, min = 0, max = 50, step = NA, width = NULL),
                                  numericInput(inputId = "YearsSinceLastPromotion", label = "Number of Years Since Last Promotion", value = 10, min = 0, max = 30, step = NA, width = NULL),
                                  numericInput(inputId = "YearsWithCurrManager", label = "Number of Years With Current Manager", value = 3, min = 0, max = 20, step = NA, width = NULL)
                           )
                           
                         ),
                         hr(),
                         
                         fluidRow(h5("The prediction model above was built using the IBM HR Analytics Employee Attrition and Performance dataset. The dataset was created by IBM data scientists to help uncover the factors that lead to employee attrition and allow exploratory data analysis for an organization."))
                         
                         ),
                tabPanel("Predict from csv",
                         h5("Uploading a csv containing employee data. The data will render a table with a new column added at the end with the probability of attrition."),
                         hr(),
                         fileInput("csvFile", "Upload csv"),
                         tableOutput("Prediction"))
                  
                )
                
                
                )
    
    
    
      )    

server = function (input, output) {
  df <- reactive({
    data.frame(Age=input$Age,
               
               BusinessTravel=input$BusinessTravel,
               
               DistanceFromHome=input$DistanceFromHome,
               
               Education=input$Education,
               
               EducationField=input$EducationField,
               
               EnvironmentSatisfaction=input$EnvironmentSatisfaction,
               
               JobInvolvement=input$JobInvolvement,
               
               JobRole=input$JobRole,
               
               JobSatisfaction=input$JobSatisfaction,
               
               MaritalStatus=input$MaritalStatus,
               
               NumCompaniesWorked=input$NumCompaniesWorked,
               
               OverTime=input$OverTime, 
               
               PercentSalaryHike=input$PercentSalaryHike,
               
               PerformanceRating=input$PerformanceRating,
               
               TrainingTimesLastYear=input$TrainingTimesLastYear,
               
               WorkLifeBalance=input$WorkLifeBalance,
               
               YearsAtCompany=input$YearsAtCompany,
               
               YearsInCurrentRole=input$YearsInCurrentRole,
               
               YearsSinceLastPromotion=input$YearsSinceLastPromotion,
               
               YearsWithCurrManager=input$YearsWithCurrManager
               
               
               
    )
  })
  
  pred <- reactive({
    predict(bestmodel,df(), type="response")
  })
  
  output$Pred <- renderText({paste("The probability of attrition is",round(pred(), digits=2),".")}) 
  
  rawData <- eventReactive(input$csvFile, {
    read.csv(input$csvFile$datapath)
  })
  

  
  prediction <- reactive({
    predict(bestmodel,rawData(),type="response")
  })
  
  output$Prediction <- renderTable({cbind(rawData(), prediction())})
  
  
  
} 


shinyApp(ui=ui,server=server)


