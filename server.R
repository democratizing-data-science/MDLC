server <- shinyServer(function(input, output, session) {
options(shiny.maxRequestSize=30*1024^2)

  ## print to console to see how the value of the shinyFiles 
  ## button changes after clicking and selection

  lst1 <- reactive({
    validate(need(input$directory != "", "select files..."))
    
    if (is.null(input$directory)) {
      return(NULL)
    } else {
      
      path_list <- as.list(input$directory$datapath)
      values$name_list <- as.list(input$directory$name)
     values$numfiles <- length(path_list)
      documents <- lapply(input$directory$datapath, pdf_text)
    }
  })  

  #  a<-parseDirPath(volumes, input$directory)
  values <- reactiveValues()
  ntext <- eventReactive(input$goButtontm, {

    phu<-as.data.frame(input$directory$name)
    colnames(phu)[1]<-"files"
    phu$id<-rownames(phu)
    values$phu<-phu
    documents <- lapply(input$directory$datapath, pdf_text)
    corp <- sapply(documents, function(row) str_replace_all(row, "[\r\n]" , " "))

    corp <- Corpus(VectorSource(corp))
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
      #corp[[j]] <- gsub("*", " ", corp[[j]])
      corp[[j]] <- gsub("/", " ", corp[[j]])
      corp[[j]] <- gsub("#", " ", corp[[j]])
      corp[[j]] <- gsub("@", " ", corp[[j]])
      corp[[j]] <- gsub("\\|", " ", corp[[j]])
      corp[[j]] <- gsub("\u2028", " ", corp[[j]]) 
      corp[[j]] <- gsub("‰Ûª", " ", corp[[j]]) 
      corp[[j]] <- gsub("‰ûï", " ", corp[[j]]) 
      corp[[j]] <- gsub("‰Û", " ", corp[[j]]) 	
      corp[[j]] <- gsub("•", " ", corp[[j]]) 	
      corp[[j]] <- gsub("’", " ", corp[[j]]) 	
      corp[[j]] <- gsub("“", " ", corp[[j]])
      corp[[j]] <- gsub("”", " ", corp[[j]])
      corp[[j]] <- gsub("ß", " ", corp[[j]])
      corp[[j]] <- gsub("‘‘", " ", corp[[j]])
      corp[[j]] <- gsub("’’", " ", corp[[j]])
      corp[[j]] <- gsub("©", " ", corp[[j]])
      corp[[j]] <- gsub("–", " ", corp[[j]])
      corp[[j]] <- gsub("http", " ", corp[[j]])
      corp[[j]] <- gsub("www", " ", corp[[j]])
      corp[[j]] <- gsub("https", " ", corp[[j]])
      corp[[j]] <- gsub("â", " ", corp[[j]])
      corp[[j]] <- gsub("~", " ", corp[[j]])
      corp[[j]] <- gsub("#", " ", corp[[j]])
      corp[[j]] <- gsub("Ÿ", " ", corp[[j]])
      corp[[j]] <- gsub("®", " ", corp[[j]])
      corp[[j]] <- gsub("€", " ", corp[[j]])
      corp[[j]] <- gsub("™", " ", corp[[j]])
      corp[[j]] <- gsub("„", " ", corp[[j]])
      corp[[j]] <- gsub("&", " ", corp[[j]])
      corp[[j]] <- gsub("%", " ", corp[[j]])
      corp[[j]] <- gsub("—", " ", corp[[j]])
      corp[[j]] <- gsub("†", " ", corp[[j]])
      corp[[j]] <- gsub("‘", " ", corp[[j]])
      corp[[j]] <- gsub("…", " ", corp[[j]])
      corp[[j]] <- gsub("¦¦", " ", corp[[j]])
#      corp[[j]] <- gsub("literature", " ", corp[[j]])
 #     corp[[j]] <- gsub("research", " ", corp[[j]])
  #    corp[[j]] <- gsub("review", " ", corp[[j]])
   #   corp[[j]] <- gsub("study", " ", corp[[j]])
    #  corp[[j]] <- gsub("journal", " ", corp[[j]])
      corp[[j]] <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", " ", corp[[j]]) # Remove 1-3 letter words
    }

 #   corp <- Corpus(VectorSource(corp))
    corp <-tm_map(corp,content_transformer(tolower))
    #remove punctuation
    corp <- tm_map(corp, removePunctuation)
    #Strip digits
    corp <- tm_map(corp, removeNumbers)
    #remove stopwords
    corp <- tm_map(corp, removeWords, c(stopwords("english"),c("will", "literature", "research", "review", "study","journal")))
    #remove whitespace
    corp <- tm_map(corp, stripWhitespace)
    #Remove URL
    urlPat<-function(x) gsub("(ftp|http)(s?)://.*\\b", "", x)
    corp<-tm_map(corp, urlPat)
    #Remove Email
    emlPat<-function(x) gsub("\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b", "", x)
    corp<- tm_map(corp, emlPat)
    
    corp <- tm_map(corp, lemmatize_strings)
#    corp <- tm_map(corp, PlainTextDocument)
    
    #Stem document
#    corp <- tm_map(corp,stemDocument)

    for (j in seq(corp)) {   
    corp[[j]] <- gsub(" *\\b[[:alpha:]]{1,3}\\b *", " ", corp[[j]]) # Remove 1-3 letter words
  }
    
    #Convert to document matrix
    dtm <- DocumentTermMatrix(corp)
    
    #remove sparse words
    dtm <- removeSparseTerms(dtm,0.5)
    
    ui = unique(dtm$i)
    dtm = dtm[ui,]
    
    print(dtm)
    
    top_10_words<-data.frame(words=names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10)),freq=head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10), row.names=NULL)
    print(top_10_words)
#    phu<-names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),20))
    values$dtm<-dtm

    values$corp<-corp
  })

  ntextlda <- eventReactive(input$goButton, {   
    
    ## Convert tdm to a list of text
    dtm2list <- apply(values$dtm, 1, function(x) {
      paste(rep(names(x), x), collapse=" ")
    })  
    ## convert to a Corpus
    corp <- VCorpus(VectorSource(dtm2list))
    
#    corp <- tm_map(corp, removeWords, values$phu[input$caption])
    
    corp <- tm_map(corp, removeWords, names(head((sort(rowSums(as.matrix(t(values$dtm))),decreasing=TRUE)),input$trim)))
    dtm <- DocumentTermMatrix(corp)
    trp_10_words<-data.frame(words=names(head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10)),freq=head((sort(rowSums(as.matrix(t(dtm))),decreasing=TRUE)),10), row.names=NULL)
    print(dtm)
    print(trp_10_words)
    
    values$dtm<-dtm
    values$corp<-corp    
  })    
    
  ntopiclda <- eventReactive(input$goButtonT, {  
#    cluster <- makeCluster(detectCores(logical = TRUE) - 1)
    
    result <- FindTopicsNumber(
      values$dtm,
      topics = seq(from = 2, to = 15, by = 1),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = "Gibbs",
      control = list(seed = 47, burnin = input$burnin, iter = input$iterations),
      mc.cores = NA, #cluster,
      verbose = TRUE
    )
    values$results<-result
    output$plot <- renderPlot({
      input$newplot
      # Add a little noise to the cars data
      FindTopicsNumber_plot(result)
    })
    #str(documents)
})
  
  p2 <- reactive(FindTopicsNumber_plot(values$results))
  output$save <- downloadHandler(
    file = "save.png" , # variable with filename
    content = function(file) {
      #ggsave(p(), filename = file)
      png(file = file)
      p2()
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
    #head(topicProbabilities)
    #dim(topicProbabilities)
    topicProbabilities$topic<-colnames(topicProbabilities)[max.col(topicProbabilities,ties.method="first")]
    topicProbabilities$text<- ap_lda@documents
    topicProbabilities$title <- values$phu$files[match(topicProbabilities$text, values$phu$id)]
    topicProbabilities$text#<-NULL
    
    topicProbabilities$relative_probability <- apply(topicProbabilities[,1:input$obs], 1, max)
    #Select the five best examples PHUDCFILY
    
    topicProbabilities$relative_probability<-topicProbabilities$relative_probability/max(topicProbabilities$relative_probability)
    topicProbabilities<-topicProbabilities[,c("topic","title","relative_probability")]
    
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
    serVis(json_lda)


    
#    values$ex1<-ex1
    
    output$table <- renderDataTable({
      ex1 <-  as.data.frame(topicProbabilities %>% group_by(topic) %>% top_n(input$topphu, relative_probability))
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
  
  # downloadHandler() takes two arguments, both functions.
  # The content function is passed a filename as an argument, and
  #   it should write out data to that filename.
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Full_Machine_Learned_Classes", "csv", sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
    #  sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(values$findings, file, sep = ",",
                  row.names = FALSE)
    }
  )
  
  output$downloadDatas <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Most_Representative_Docs", "csv", sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      #sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(values$ex1, file, sep = ",",
                  row.names = FALSE)
    }
  )
  
})

#shinyApp(ui = ui, server = server)