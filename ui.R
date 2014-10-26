library(shiny)

TitanicData <- as.data.frame(Titanic)
ClassVector <- unique(TitanicData$Class)

shinyUI(pageWithSidebar(
  headerPanel("Survival Prediction on Titanic!"),
  sidebarPanel(
    selectInput('class','Class',choices = c("NA",'1st','2nd','3rd','Crew')),
    selectInput('sex','Sex',choices = c("NA",'Male','Female')),
    selectInput('age','Age',choices = c("NA",'Adult','Child'))
    #submitButton('Submit')
    ),
  mainPanel(
  tabsetPanel(
  tabPanel("Input Data",h3('You entered Following Combination'),
  h4('Class'),
  verbatimTextOutput("class"),
  h4('Sex'),
  verbatimTextOutput("sex"),
  h4('Age'),
  verbatimTextOutput("age")),
  tabPanel("Computation Data",dataTableOutput("computetable"),h4('Yes Probability'),
  verbatimTextOutput("YesProbability"),
  h4('NoProbability'),
  verbatimTextOutput("NoProbability")
           ),tabPanel("Documentation",h2("About"),h5('The following section describes the data product in detail'),h4('1.Aim:'),h5('Given a set of details for a person who was on board on Titanic we try to predict whether would have survived or not.For solving this problem we devlop a simple Naive-Bayes model.'),h4('2.Dataset:'),h5('The Product uses Titanic dataset available with R to achieve its aim'),h4('3.Model:'),h5('A Naive-Bayes model works on bayesian assumption that simplifies conditional probability rules by allowing independence between different attributes. In this product a user selects a set of features they expect in a person on board on titanic. We then see the yes probability (No of persons that survived with given attribute)/(Total No. of persons with that attribute) while the probability that a person did not survive is given by (1 - yes_probability). We then find expected survival probability by multiplying yes_probability of all the selected  attributes. To avoid zero error we do laplacian correction that is add 1 to numerator and denominator of each individual probability which is zero. Finally user can observe what are the chances of survival and what are the chances that a person with selected characterstics was not able to survive',h4('4: How to Start?'),h5('User Needs to select attrbutes expected in a person. Input data Section displays required attributes. Computation Data Displays the results of the Calculation. Here Yes Probability displays Survival Chances while NoProbability displays chances that a person with given characterstics has not survived')))
      )
  )
))
