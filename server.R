# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(quanteda)
library(dplyr)
library(RColorBrewer)

#devtools::install_github("kbenoit/quanteda")
#library(quanteda)

getCorpus <- function(path){
  if(is.null(path)){
    return(NULL)
  }
  print(path$datapath)
  tmpPath <- tempdir
  fs <- path$datapath
  textsvec = c()
  for (i in 1:length(fs)){
    textsvec[i] <- paste(readLines(fs[i]), collapse = " ")
  }
  names(textsvec) <- basename(path$name)
  qc <- corpus(textsvec)
  return(qc)
}


getCorpusFromZip <- function(path, fixedZip = FALSE){
  if(is.null(path)){
    return(NULL)
  }
  if(fixedZip){
    # need an altered version of textfiles.R to make this work 
    # (around line 298)
    qc <- (textfile(path))
  }
  else{
    td <- tempdir()
    fs <- unzip(path$datapath, exdir = td)
    textsvec = c()
    for (i in 1:length(fs)){
      textsvec[i] <- paste(readLines(fs[i]), collapse = " ")
    }
    names(textsvec) <- basename(fs)
    qc <- corpus(textsvec)
  }
  return(qc)
}


shinyServer(function(input, output) {
  myCorpus <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)

    if(input$uploadTypeRadio == 1){
      mc <- getCorpusFromZip(inFile)
    }
    else{
      mc <- getCorpus(inFile)
    }
    return(mc)
  })
  output$fileTable <- renderDataTable({
    curCorpus <- myCorpus()
    summary(curCorpus)
  }, options = list(searching = FALSE))
  output$dfmTable <-  renderDataTable({
    curCorpus <- myCorpus()
    weightType = 'relFreq'
    curDf <- dfm(curCorpus) %>% trim(as.numeric(input$minFreq), as.numeric(input$minDoc))
    if(input$weightingRadio != "count"){
      curDf <- weight(curDf, input$weightingRadio)
    }
    showN <- as.numeric(input$showN)
    curDf <- as.data.frame(curDf)
    toRender <- cbind(filenames = row.names(curDf[,1:showN]), curDf[,1:showN])
  }, options = list(searching = FALSE))
  
  output$kwicTable <- renderDataTable({
    curCorpus <- myCorpus()
    kwic(curCorpus, input$keyword, window = input$contextSize,
         valuetype = input$kwicValueType,
         case_insensitive = !(input$caseSensitive))
  }, options = list(searching = FALSE))

  output$cloudTable <- renderPlot({
    curCorpus <- myCorpus()
    curDf <- dfm(curCorpus, ignoredFeatures = c("will", "i", "it", ":", "'", ",", "?", ".", "\"", "(", ")", "png", "subtype", "time", "files", "heh", "yeah", "u06jmcyav", "u065393a4", "name", "type", "ts", "user", "text", "message", "https", "u064zh0cd", "false", "like", "jpg", "u0788p59d", stopwords("english")))
    topfeatures(curDf, 100)
    customcolors <- c("#32c850","#1f2421","#216869","#9cc5a1","#dce1de")
    plot(curDf, max.words = 150, colors = customcolors, scale = c(8, .5))
  })

})
