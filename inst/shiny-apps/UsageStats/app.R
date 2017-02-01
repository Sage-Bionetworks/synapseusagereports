#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rmarkdown)
library(synapseClient)
source("../../../inst/lib.R")
library(plyr)
library(dplyr)

synapseLogin()

checkForProject <- function(projectId) {
  length(synQuery(sprintf('select id from project where projectId=="%s" LIMIT 1', projectId))) == 1
}

renderMyDocument <- function(reportType, projectId, nMonths, aclTeamOrder, useTeamGrouping, outputFile) {
    res <- rmarkdown::render(input=paste0("../../", reportType, ".Rmd"),
                      output_file=outputFile,
                      params = list(projectId=projectId, nMonths=nMonths, 
                                    aclTeamOrder=aclTeamOrder, 
                                    useTeamGrouping=useTeamGrouping))
}

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
  ),
  
   # Application title
   titlePanel("Synapse Project Usage"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput("projectId",
                     "Project ID"),
         checkboxInput('useTeamGrouping', 'Group by teams', value=FALSE),
         actionButton('lookup', "Lookup Project"),
         uiOutput('teamList'),
         hr(),
         selectInput('reportType', "Report Type", choices=c("report"), 
                     selected="report"),
         sliderInput("months", "Months", min=1, max=12, value=2, step=1),
         hr(),
         actionButton('report', "Make Report")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        includeMarkdown('info.md'),
        hr(),
        uiOutput("results")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  myVals <- reactiveValues()

  teamACL <- eventReactive(input$lookup, {
    validate(
      need(try(checkForProject(isolate(input$projectId))), 
           "That project doesn't exist. Please try again.")
    )
    
    acl <- synGetEntityACL(input$projectId)
    aclToMemberList(acl) %>% 
      filter(userName != "PUBLIC", !isIndividual)
  })
  
  output$teamList <- renderUI({
    withProgress(message = 'Looking up team...', value = NULL, style="old", {
      teamList <- teamACL()
      teamIds <- c(input$projectId, 
                   as.character(teamList$ownerId))
      names(teamIds) <- c("Project ACL Users", as.character(teamList$userName))
    })
    
    if (input$useTeamGrouping) {
      selectInput("teamOrder", "Team Order", choices=teamIds, 
                selected=NULL, width='100%', multiple = TRUE, selectize = TRUE)
    }
    else {
      list()
    }
  })
  
  res <- eventReactive(input$report, {
    withProgress(message=sprintf("Making %s Report", input$reportType), 
                 value = NULL, style="old", {
      
      validate(
        need(try(checkForProject(input$projectId)), 
             "That project doesn't exist. Please try again.")
      )
      
      if (input$useTeamGrouping) {
        validate(
          need(try(input$teamOrder != ""), 
               "Please select a Team ordering before generating a report.")
        )
      }
      
    myVals[['reportName']] <- 'myreport.html'
    print(input$teamOrder)
    renderMyDocument(reportType=input$reportType, 
                     projectId = input$projectId,
                     nMonths=input$months,
                     aclTeamOrder = input$teamOrder,
                     useTeamGrouping=input$useTeamGrouping,
                     #outputFile=paste0(as.numeric(as.POSIXct(Sys.Date())), "_", reportType, ".html"),
                     outputFile='myreport.html')
    })
  })
  
  
  output$download = downloadHandler(
    filename = 'myreport.html',
    content = function(file) {
      # out <- renderMyDocument(reportType=input$reportType,
      #                         projectId = input$projectId,
      #                         nMonths=input$months,
      #                         #outputFile=paste0(as.numeric(as.POSIXct(Sys.Date())), "_", reportType, ".html"),
      #                         outputFile='myreport.html')
      file.copy(sprintf('../../%s', myVals[['reportName']]), file) # move to file for downloading
    },
    contentType = 'application/html'
  )

   output$results <- renderUI({
     report <- res()
     list(h3("Results"), downloadButton('download'))
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

