## install packages
# install.packages(c("shiny", "shinythemes", "data.table", "ggplot2", "tm", "memoise", 
#                    "wordcloud", "colourpicker", "maps", "mapproj", "gdata"))

## load libraries
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(tm)
library(memoise)
library(wordcloud)
library(colourpicker)
library(maps)
library(mapproj)
library(gdata)

## load file content
source("AircraftRegistration_data.R")


##### Tab "Tables"
################################################################################

tableChoices <<- list("Engine Manufacturer"= "df_ENG_TopList_MFR",
                      "Aircraft Manufacturer"="df_ACFT_TopList_MFR") 

table_page <- tabPanel(
  title = "Tables",
  titlePanel("Tables"),
  sidebarLayout(
    sidebarPanel(selectInput("input_table",
                             label = "Choose a dataset",
                             choices = tableChoices,
                             selected = "df_master")
    ),
    mainPanel("Table", tableOutput("table"))
  )
)



##### Tab "Word Cloud"
################################################################################

## assign column and description for dropdown
wc_selection <<- list("City" = "CITY",
                      "Engine Manufacturer" = "MFR_engine", 
                      "Aircraft Manufacturer" = "MFR_ACFT")

## Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(wc_sel) {
  
  ## select column of interest and drop NAs
  content <- drop_na(df_aircraft, wc_sel)[[wc_sel]]
  
  ## remove leading and trailing blanks, replace spaces by dots 
  ## paste concatenates to list delimited by blank
  text <- paste(str_replace_all(trim(content), " ", "."), collapse=" ")
  
  ## vectorized text to corpus
  myCorpus = Corpus(VectorSource(text))
  
  ## build term matrix
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  m = as.matrix(myDTM)
  
  return(sort(rowSums(m), decreasing = TRUE))
})


# define tabs
wc_page <- tabPanel(
  title = "Word Clouds",
  titlePanel("Word Clouds"),
  fluidPage(
    sidebarLayout(
      ## Sidebar with a slider and selection inputs
      sidebarPanel(
        selectInput("selection", "Choose a feature:",
                    choices = wc_selection),
        ## actionButton("update", "Change"),
        hr(),
        sliderInput("freq",
                    "Minimum Frequency:",
                    min = 1,  max = 10, value = 1),
        sliderInput("max",
                    "Maximum Number of Words:",
                    min = 1,  max = 200,  value = 50)
      ),

      ## Show Word Cloud
      mainPanel(
        plotOutput("plot")
      )
    )
  )
)



##### Tab "About"
################################################################################

about_page <- tabPanel(
  title = "About",
  titlePanel("About this Project"),
  "This app was created by DataBauHeini and WhyKiki to highlight interesting facts \r 
  about the United States Aircraft Registration by the Federal Aircraft Association (FAA).",
  br(), br(),
  "The app was created with R Shiny.",
  br(), br(),
  "2021 September"
)



##### User Interface
################################################################################

ui <- navbarPage(
  title = "US Aircraft Registration by FAA",
  theme = shinytheme("cyborg"),
  wc_page,
  table_page, 
  about_page
)



##### Server
################################################################################

## Define server logic for random distribution app ----
server <- function(input, output) {
  
  ## Reactive expression to generate the requested distribution ----
  ## This is called whenever the inputs change. The output functions
  ## defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch("norm", #input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)

    dist(500) #(input$n)
    
  })
  
  ## Define a reactive expression for the document term matrix
  terms <- reactive({
    ## Change word cloud when selection is changed ...
    input$selection
    ## ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  
  ## WORD CLOUD part
  ## ----------------------------------------------------------
  ## Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  ## TABLE part
  ## ----------------------------------------------------------
  react_table <- reactive({ 
    if (input$input_table == "df_ENG_TopList_MFR") 
      return (df_ENG_TopList_MFR)
    else  
      return (df_ACFT_TopList_MFR)
  })
  output$table <- renderTable({react_table()})
  
}



##### Connection / Shiny App
################################################################################

shinyApp(ui=ui, server=server)
