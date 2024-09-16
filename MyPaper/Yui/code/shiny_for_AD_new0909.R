library(shiny)
library(readr)
library(gridExtra)
#library(PupillometryR)
library(shinythemes)
library(shinyWidgets)

ui <- fluidPage(
  theme=shinytheme("slate"),
  titlePanel("The prediction"),
  sidebarLayout(
    sidebarPanel(
      #Imput number
      selectizeInput("gender", "Gender", choices = list("male"=1,"female"=0)),
      selectizeInput("APOE", "APOE", choices = list("Yes"=1,"No"=0)),
      textInput("CASI", label = "CASI_score", value = "84"),
    ),
    mainPanel(
      "The probability of never experiencing dementia until death", verbatimTextOutput("Prob"),
      "The estimated median time under the group of cognitive deterioration",verbatimTextOutput("MT")
    )
  )
)

server <- function(input, output, session) {
  results<-RF<-reactive({
    gender.1<-input$gender
    APOE.1<-input$APOE
    CASIScore<-as.numeric(input$CASI)
    #
    gender.1<-ifelse(gender.1==1,1,0)
    APOE.1<-ifelse(APOE.1==1,1,0)
    #Prob
    p.t<-exp(162.284047+(-2.259556*gender.1)+(12.732745*APOE.1)+(-1.721796*CASIScore))
    p.t.u<-1-(p.t/(1+p.t))
    #median time
    med.time<-(log(2)/exp(-12.101127759+(-0.001608063*gender.1)+(0.222739763*APOE.1)+(-0.012174811*CASIScore)))^(1/2.785996)
    list(Prob.uncog=p.t.u,median.time=med.time)
  })
  #output results
  output$Prob<-renderPrint({
    print(as.numeric(results()[1]))
  })
  output$MT<-renderPrint({
    print(as.numeric(results()[2]))
  })
}

shinyApp(ui = ui, server = server)
