library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Admission Chances Prediction"),
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # select time range to display 
      numericInput("GRE","GRE Score",value = 300),
      
      numericInput("TOEFL","TOEFL Score",value = 100),
      
      numericInput("U_Rating","University Rating",value = 4,min = 0,
                   max = 5),
      
      numericInput("SOP","SOP",value = 3,min = 0,
                   max = 5),
      
      numericInput("LOR","LOR",value = 3,min = 0,
                   max = 5),
      
      numericInput("CGPA","CGPA",value = 8,min = 1,
                   max = 10),
      
      radioButtons("Research", "Research",
                   choices = c("Yes", "No"),
                   choiceNames =  c("Yes", "No"),
                   choiceValues = "Yes"),
      
      actionButton("update","GO")
    ),
    mainPanel(textOutput("prob"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  react <- eventReactive(input$update,
                         {
                           admission_predict <- function(GRE,TOEFL,University.rating,SOP,LOR,CGPA,Research){
                             data <- data.frame(GRE.Score = GRE, TOEFL.Score = TOEFL, University.Rating = University.rating,
                                                SOP=SOP,LOR=LOR,CGPA=CGPA,Research = as.factor(Research))
                             Prediction <- predict(admin_model, data)
                             print(paste("Your chance of Admission is", round(Prediction*100,2),"%"))
                           }
                           
                           admission_predict(GRE= input$GRE,TOEFL = input$TOEFL,University.rating = input$U_Rating,
                                             SOP = input$SOP, LOR = input$LOR, CGPA = input$CGPA, Research = ifelse(input$Research=="Yes",1,0))
                           
                           }
                         )
  library(caret)
  admission <- read.csv("Admission_Predict_Ver1.1.csv")
  admission <- admission[,-1]
  
  admission[,7] <- as.factor(admission[,7])
  
  colSums(is.na(admission))
  
  admin_model <- train(
    Chance.of.Admit ~.,
    data = admission,
    method = "nnet",
    trControl = trainControl(
      method = "repeatedcv",
      repeats = 5,
      number = 10,
      verboseIter = TRUE
    )
  )
  output$prob <- renderText({
    react()
  })
}
# Run the application
shinyApp(ui = ui, server = server)
