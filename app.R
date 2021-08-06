library(shiny)
library(shinythemes)
library(shinyBS)
library("shinycssloaders")
library(DT)
library(LDAvis)
library(servr)
library("ldatuning")
library(topicmodels)
library("tsne")
library(pdftools)
library(textstem)
library(dplyr)
library(stringi)
library(stringr)
library(fs)
library(tidyverse)
library(parallel)
library(tm)
library(gistr)
library("syuzhet")
library(quanteda)
options(spinner.color="#CD1076", spinner.type = 6, shiny.maxRequestSize=30*1024^2)
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
      tags$li("Select numbers of: burn-in samples & Gibbs resamplings (recommended values pre-selected)"),#If the output of the folder selected, shown in the right panel, is correct, s
      numericInput("burnin", "Burn-in Samples", 500),
      numericInput("iterations", "MCMC/Gibbs resamplings", 5000),
      tags$li("Execute Metrics' to find optimal topic detection"),
      actionButton("goButtonT", "Execute Metrics", icon("project-diagram")),
      tags$li("Based on the metrics plot, select optimal number of topics"),
      sliderInput("obs", "Default value is 2, update below", min = 2, max = 15, value = 2),
div(style = "margin-top: -20px"),      
tags$li("Execute the MDLC engine (can readjust burn-in & resampling settings)"),  
      actionButton("go", "Execute MDLC", icon("rocket")),      
    ),
  ),
  mainPanel(
    tags$h4("1. File upload information"),
    withSpinner(textOutput("corpusphu"), hide.ui = FALSE),
     tags$hr(),
    tags$h4("2. Initial Text Mining Output"),
    withSpinner(verbatimTextOutput("nText"), hide.ui = FALSE),
    tags$hr(),
    tags$h4("3. Trimming Top Words Output (if unsatisfied change number in step 3 and re-execute steps 2 & 3)"),
    withSpinner(verbatimTextOutput("nTextlda"), hide.ui = FALSE),
    tags$hr(),
    tags$h4("4-5. Metrics Output (you can download the plot as *.PNG)"),
    withSpinner(verbatimTextOutput("nTopiclda"), hide.ui = FALSE),
    downloadButton("save", "Download Metrics Plot"),
    plotOutput("plot"),
tags$hr(),
    tags$h4("6-7. Topic Modeling, Visualization, and Most Representative Articles per Topic"),
    tags$h5("After MDLC completion you can download the resulting MLTC datasets"),
    downloadButton('downloadData', 'Download Full MLTC data (all docs in the corpus)'),
    downloadButton('downloadDatas', 'Download Most Representative cases (shown below)'),
    withSpinner(verbatimTextOutput("TopicModeling"), hide.ui = FALSE),
    sliderInput("topphu", "Select the number of most representative articles per topic", min = 1, max = 20, value = 5),
    dataTableOutput("table"),
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #CD1076}")),
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #CD1076}")),
  )
))

server <- shinyServer(function(input, output, session) {
    lst1 <- reactive({
    validate(need(input$directory != "", "To start select your files..."))
            print(paste("You uploaded",length(as.list(input$directory$datapath)), "PDFs", sep=" "))
        })
		        output$corpusphu <- renderPrint ({
		            lst1()
        })  
  values <- reactiveValues()
  ntext <- eventReactive(input$goButtontm, {
    phu<-as.data.frame(input$directory$name)
    colnames(phu)[1]<-"files"
    phu$id<-rownames(phu)
    values$phu<-phu
    documents <- lapply(input$directory$datapath, pdf_text)
    corp <- sapply(documents, function(row) str_replace_all(row, "[\r\n]" , " "))
    corp <- Corpus(VectorSource(corp))
	corp_quanteda <- corpus(corp) #PHUDCFILY new
	names(corp_quanteda)<-phu$files #PHUmDCFILY new
	corp2para <- corpus_reshape(corp_quanteda, to = "documents") #PHUDCFILY new
	x1<-convert(corp2para, format = "tm") #data.frame with doc_id and text columns that can be used to use normal vcorpus in tm PHUDCFILY #PHUDCFILY new
	values$x1<-x1 #PHUDCFILY new
    corp <- VCorpus(DataframeSource(x1),readerControl = list(language = "en")) #PHUDCFILY New
    ##Clean text
    for (j in seq(corp)) {
      corp[[j]] <- gsub("\\t", " ", corp[[j]])
                corp[[j]] <- gsub("<U+00AE>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+00B4>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0097>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0096>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0094>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0091>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0092>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+0093>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+2afd>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+4872>", " ", corp[[j]])
                corp[[j]] <- gsub("<U+00AD>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+fb01>", " ", corp[[j]])
                corp[[j]] <- gsub("<u+fb02>", " ", corp[[j]])
                corp[[j]] <- gsub("\\001", " ", corp[[j]])
                corp[[j]] <- gsub("\\023", " ", corp[[j]])
                corp[[j]] <- gsub("<u+03c7>", " ", corp[[j]])
                corp[[j]] <- gsub("<e9>", " ", corp[[j]])
                corp[[j]] <- gsub("’", " ", corp[[j]])
                corp[[j]] <- gsub("/", " ", corp[[j]])
                corp[[j]] <- gsub("#", " ", corp[[j]])
                corp[[j]] <- gsub("@", " ", corp[[j]])
                corp[[j]] <- gsub("\\|", " ", corp[[j]])
                corp[[j]] <- gsub("\u2028", " ", corp[[j]])
                corp[[j]] <- gsub("‘", " ", corp[[j]])
                corp[[j]] <- gsub("–", " ", corp[[j]])
                corp[[j]] <- gsub("”", " ", corp[[j]])
                corp[[j]] <- gsub("“", " ", corp[[j]])
                corp[[j]] <- gsub("'", " ", corp[[j]])
                corp[[j]] <- gsub("—", " ", corp[[j]])
                corp[[j]] <- gsub("ﬂ", " ", corp[[j]])
                corp[[j]] <- gsub("''", " ", corp[[j]])
                corp[[j]] <- gsub("''", " ", corp[[j]])
                corp[[j]] <- gsub("©", " ", corp[[j]])
                corp[[j]] <- gsub("-", " ", corp[[j]])
                corp[[j]] <- gsub("http", " ", corp[[j]])
                corp[[j]] <- gsub("www", " ", corp[[j]])
                corp[[j]] <- gsub("https", " ", corp[[j]])
                corp[[j]] <- gsub("‚", " ", corp[[j]])
                corp[[j]] <- gsub("~", " ", corp[[j]])
                corp[[j]] <- gsub("#", " ", corp[[j]])
                corp[[j]] <- gsub("Y", " ", corp[[j]])
                corp[[j]] <- gsub("Æ", " ", corp[[j]])
                corp[[j]] <- gsub("&", " ", corp[[j]])
                corp[[j]] <- gsub("%", " ", corp[[j]])
                corp[[j]] <- gsub("-", " ", corp[[j]])
                corp[[j]] <- gsub("¶¶", " ", corp[[j]])
      corp[[j]] <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", " ", corp[[j]]) # Remove 1-3 letter words
    }
aphu<-as.data.frame(unlist(corp))
            x1<-values$x1
            x1$text<-aphu[,1]
            corp <- VCorpus(DataframeSource(x1),readerControl = list(language = "en")) #PHUDCFILY new
    corp <-tm_map(corp,content_transformer(tolower))
    #remove punctuation
    corp <- tm_map(corp, removePunctuation)
    #Strip digits
    corp <- tm_map(corp, removeNumbers)
    #remove stopwords
    corp <- tm_map(corp, removeWords, c(stopwords("english"),c("will", "literature", "research", "review", "study","journal")))
    #remove whitespace
    corp <- tm_map(corp, stripWhitespace)
    corp <- tm_map(corp, lemmatize_strings)
for (j in seq(corp)) {   
    corp[[j]] <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", " ", corp[[j]]) # Remove 1-3 letter words
  }
	aphu<-as.data.frame(unlist(corp))
	toDelete <- seq(0, nrow(aphu), 2)
	aphu <-  as.data.frame(aphu[-toDelete, ])
	x1$text<-aphu[,1]
	corp <- VCorpus(DataframeSource(x1),readerControl = list(language = "en"))
    #Convert to document matrix
    dtm <- DocumentTermMatrix(corp)
    #remove sparse words
    dtm <- removeSparseTerms(dtm,0.95)
    ui = unique(dtm$i)
    dtm = dtm[ui,]
    print(dtm)
    top_10_words<-data.frame(words=names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10)),freq=head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10), row.names=NULL)
    values$dtm<-dtm
dtm2list <- apply(values$dtm, 1, function(x) {
                paste(rep(names(x), x), collapse=" ")
            })
            tphu<-as.data.frame(dtm2list)
            tphu$dropped<-0
            values$x1phu <- values$x1
            values$x1phu$dropped <- tphu$dropped[match(values$x1phu$doc_id, rownames(tphu))]
            
            print(paste("Article dropped due to sparsity issues or content is not text, Text ID: ",values$x1phu$doc_id[is.na(values$x1phu$dropped)], sep= ""))
            
            print(top_10_words)
            
            corp <- VCorpus(VectorSource(dtm2list))
            names(corp) <- values$x1phu$doc_id[!is.na(values$x1phu$dropped)]
            values$corp<-corp
  })

ntextlda <- eventReactive(input$goButton, {   
    ## Convert tdm to a list of text
    dtm2list <- apply(values$dtm, 1, function(x) {
      paste(rep(names(x), x), collapse=" ")
    })  
    ## convert to a Corpus
	corp <- values$corp#VCorpus(VectorSource(dtm2list))
    corp <- tm_map(corp, removeWords, names(head((sort(rowSums(as.matrix(t(values$dtm))),decreasing=TRUE)),input$trim)))
    dtm <- DocumentTermMatrix(corp)
    trp_10_words<-data.frame(words=names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10)),freq=head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10), row.names=NULL)
    print(dtm)
    print(trp_10_words)
    values$dtm<-dtm
    values$corp<-corp
  })    
    ntopiclda <- eventReactive(input$goButtonT, {  
    cluster <- makeCluster(detectCores(logical = TRUE) - 1)
    result <- FindTopicsNumber(
      values$dtm,
      topics = seq(from = 2, to = 15, by = 1),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 47, burnin = input$burnin, iter = input$iterations),
      mc.cores = cluster,
      verbose = TRUE
    )
    values$results<-result
    output$plot <- renderPlot({
      input$newplot
      FindTopicsNumber_plot(result)
    })
})
		output$save <- downloadHandler(
		file = "Metrics_plot.png" , # variable with filename
		content = function(file) {
		    png(file = file, width = 2000, height = 1000)
		    FindTopicsNumber_plot(values$results)
		    dev.off()
})
  output$nText <- renderPrint ({
    ntext()
  })
output$nTextlda <- renderPrint ({  
  ntextlda()
  })
  output$nTopiclda <- renderPrint ({  
    ntopiclda()
  })
  topicmodeling <- eventReactive(input$go, {
  ap_lda <- LDA(values$dtm, k = input$obs, method = "Gibbs", control = list(seed = 47, burnin = input$burnin, iter = input$iterations))
    topicProbabilities <- as.data.frame(ap_lda@gamma)
    topicProbabilities <- round(topicProbabilities,3)
    topicProbabilities$topic<-colnames(topicProbabilities)[max.col(topicProbabilities,ties.method="first")]
    topicProbabilities$text<- ap_lda@documents
    topicProbabilities$title <- values$x1$doc_id[match(topicProbabilities$text, values$x1$doc_id)]
    topicProbabilities$text#<-NULL
    topicProbabilities$relative_probability <- apply(topicProbabilities[,1:input$obs], 1, max)
    g<-as.data.frame(aggregate(relative_probability~topic, topicProbabilities, max))
    gavg<-as.data.frame(aggregate(relative_probability~topic, topicProbabilities, mean))#PHUDCFILY
    topicProbabilities$max_group <- g$relative_probability[match(topicProbabilities$topic, g$topic)]
    topicProbabilities$max_group_avg <- gavg$relative_probability[match(topicProbabilities$topic, gavg$topic)]
    topicProbabilities$post_estimate<-topicProbabilities$relative_probability#/topicProbabilities$max_group
    topicProbabilities$relative_probability<-topicProbabilities$relative_probability/topicProbabilities$max_group
    topicProbabilities$relative_group_fit<-topicProbabilities$max_group_avg/(max(topicProbabilities$max_group_avg))
    topicProbabilities$relative_text_contribution <- round(topicProbabilities$relative_probability, 3)
    topicProbabilities$relative_group_fit <- round(topicProbabilities$relative_group_fit, 3)
    topicProbabilities<-topicProbabilities[,c("topic","title","relative_text_contribution", "relative_group_fit")]
    values$findings<-topicProbabilities
  
    fitted<-ap_lda
    corpus<-values$corp
    doc_term<-values$dtm
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
      temp <- paste(corpus[[i]]$content, collapse = ' ')
      doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    temp_frequency <- as.matrix(doc_term)#inspect(doc_term)
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    rm(temp_frequency)
            # Convert to json
            svd_tsne <- function(x) tsne(svd(x)$u)
            json_lda <- createJSON(phi = phi, theta = theta,
            vocab = vocab,
            doc.length = doc_length,
            term.frequency = freq_matrix$Freq,
            mds.method = svd_tsne)
            Sys.setenv(GITHUB_PAT = "add_your_key_here")
            serVis2 <- function (json, out.dir = tempfile(),  
                as.gist = FALSE, ...)
            {
                dir.create(out.dir)
                src.dir <- system.file("htmljs", package = "LDAvis")
                to.copy <- Sys.glob(file.path(src.dir, "*"))
                file.copy(to.copy, out.dir, overwrite = TRUE, recursive = TRUE)
                cat(json, file = file.path(out.dir, "lda.json"))
				d<<-(out.dir)
m<<-httd(d, daemon = TRUE)$url
			}
            serVis2(json_lda)#, as.gist=T)
		    utils::browseURL(m) #PHUDCFILY	
    output$table <- renderDataTable({
      ex1 <-  as.data.frame(topicProbabilities %>% group_by(topic) %>% top_n(input$topphu, 
	  relative_text_contribution))
      values$ex1<-ex1
      datatable(ex1, options = list(
        pageLength = input$topphu*input$obs))
    })
    observeEvent(input$topphu, {
      updateSliderInput(session,inputId = "table" ,value = input$topphu,
                        min = 1, max = 20, step = 1)
    })
  })
  output$TopicModeling <- renderPrint ({
    topicmodeling()
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Full_Machine_Learned_Classes", "csv", sep = ".")
    },
    content = function(file) {
      write.table(values$findings, file, sep = ",",
                  row.names = FALSE)
    }
  )
  output$downloadDatas <- downloadHandler(
    filename = function() {
      paste("Most_Representative_Docs", "csv", sep = ".")
    },
    content = function(file) {
      write.table(values$ex1, file, sep = ",",
                  row.names = FALSE)
    }
  )
})
shinyApp(ui = ui, server = server)