### Predicition Model ###

require(shiny)
require(shinythemes)
require(dplyr)
require(MASS)
## Data Pre-processing

# Read in the data
data <- read.csv('data/employees.csv',fileEncoding="UTF-8-BOM")
data<- within(data, rm(EmployeeCount, EmployeeNumber, Over18, StandardHours))


# filter data based on whether the employee attritted
attritted <- filter(data, Attrition == "Yes")
employed <- filter(data, Attrition == "No")





## Build Model

# split data into training and testing data

set.seed(123)
sample <- sample.int(n = nrow(data), size =.5*nrow(data), replace=F)
train <- data[sample,]
test <- data[-sample,]


## create the model using stepwise logistic regression
glm.fits <- glm(Attrition~.,data=train,family=binomial())
bestmodel <- glm.fits %>% stepAIC(trace=FALSE)

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


