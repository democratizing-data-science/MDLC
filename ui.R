###Example code to reproduce MDLC hosted here https://cutt.ly/uQpTVky
#we need all these libraries
library("tsne")
library(textstem)
library(dplyr)
library(stringi)
library(stringr)
library(LDAvis)
library(servr)
library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinyBS)
library(fs)
library(pdftools)
library(tidyverse)
library(parallel)
library(tm)
library(DT)
library("ldatuning")
library(topicmodels)

ui <- fluidPage(theme = shinytheme("readable"),
                pageWithSidebar(
  headerPanel(
    "Machine Learning Based Systematic Literature Reviews"
  ),
  sidebarPanel(

    tags$style(".btn-file {background-color:#CD1076; border-color: #2e6da4; } .btn:hover {
    color: #ffffff;
    background-color: #CD1076;
    border-color: #2e6da4;
}
							.progress-bar {background-color: #CD1076; }"),
    

    tags$b("Program Description"), 
    tags$p(HTML("<b>T</b>ext mining and Machine Driven Literature Classification
           <b>(MDLC)</b> analyses via Latent Dirichlet Allocation & Gibbs Sampling.<br>
           <b>T</b>he platform identifies the optimal number of topic allocations based on four metrics.<br>
           <b>F</b>inal outputs show the most representative articles per topic
           and an interactive platform to evaluate topic/word relevance.<br>
           <b>T</b>he topic-classifed database can be downloaded for further analyses")),
    

    
    tags$hr(),
    
    tags$b("Steps to execute the MDLC processes:"),
    div(style = "margin-top: -2.5px"),
    tags$ol(
      tags$li("Select files from local folder"),
div(style = "margin-top: -20px"),
      fileInput('directory', '', placeholder = 'You can also drag files here', multiple = TRUE, accept = c('.pdf'), buttonLabel=list(icon("folder"),"Browse")),
div(style = "margin-top: -20px"),
      tags$li("Execute Text Mining/Cleaning procedures"),
      actionButton("goButtontm", "Execute Text Mining", icon("file-word")),
      tags$li("Select numbers of most common words to trim (0 removes no words)"),
div(style = "margin-top: -20px"),      
numericInput("trim", "", 0),    
      actionButton("goButton", "Trim Common Words", icon("remove-format")),
      tags$li("Select numbers of: burn-in samples & Gibbs resamplings (recommended values pre-selected)"),
      numericInput("burnin", "Burn-in Samples", 500),
      numericInput("iterations", "MCMC/Gibbs resamplings", 5000),
      tags$li("Execute Metrics' to find optimal topic detection"),
      actionButton("goButtonT", "Execute Metrics", icon("project-diagram")),
      
      
      tags$li("Based on the metrics plot, select optimal number of topics"),
      sliderInput("obs", "Default value is 2, update below", min = 2, max = 10, value = 2),

div(style = "margin-top: -30px"),      
tags$li("Execute the MDLC engine (can readjust burn-in & resampling settings)"),  
      actionButton("go", "Execute MDLC", icon("rocket")),      
    ),

  ),
  mainPanel(

    
    tags$h4("Initial Text Mining Output"),
    verbatimTextOutput("nText"),
    tags$hr(),
    tags$h4("Trimming Top Words Output (if unsatisfied change number in step 3 and re-execute steps 2 & 3)"),
    verbatimTextOutput("nTextlda"),
    tags$hr(),
    tags$h4("Metrics Output (you can download the plot as *.PNG)"),
    verbatimTextOutput("nTopiclda"),

    downloadButton("save", "Download Metrics Plot"),
    plotOutput("plot"),

tags$hr(),
    tags$h4("Topic Modeling and Most Representative Articles per Topic"),
    

    tags$h5("After MDLC completion you can download the resulting MLTC datasets"),

    downloadButton('downloadData', 'Download Full MLTC data (all docs in the corpus)'),
    downloadButton('downloadDatas', 'Download Most Representative cases (shown below)'),
    verbatimTextOutput("TopicModeling"),
    sliderInput("topphu", "Select the number of most representative articles per topic", min = 1, max = 20, value = 5),
    dataTableOutput("table"),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #CD1076}")),
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #CD1076}")),
  )
))