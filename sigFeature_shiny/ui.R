#library(shiny)

# Define UI for application that draws a histogram

fluidPage(
  titlePanel("Compare Features"),
  
  fluidRow(
    column(4,
           wellPanel(
             fileInput('file1', 'Upload a Rdata file', accept = c('.Rda', '.Rdata')),
             textOutput('incidentCount'),
             checkboxInput("breaches", "Only Breaches", value = FALSE, width = NULL),
             textInput('df_name', "Name of the dataframe in the Rdata file", value="e.g. 'vcdb' or 'vz'"),
             actionButton('submit', 'Analyze'),
             uiOutput("featuresO"),
             uiOutput("sigEnumO"),
             uiOutput("enumValO"),
             uiOutput("sigFeaturesO")
           )
    ),
    #      mainPanel(
    column(4, 
           plotOutput("featureEnumO"),
           textOutput("withFeatureText")
    ),
    column(4, 
           plotOutput("notFeatureEnumO"),
           textOutput("notFeatureText")
    )
  )
)