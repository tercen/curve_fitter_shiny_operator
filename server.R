########################################
#
# nplr App deployed on Synapse
#
########################################

## REQUIRED
library(nplr)
library(XLConnect)
library(shinyjs)
library(rtercen)
source("helpers.R")

############################

getTercenData = function(session){
  # retreive url query parameters provided by tercen
  query = parseQueryString(session$clientData$url_search)
  
  token = query[["token"]]
  workflowId = query[["workflowId"]]
  stepId = query[["stepId"]]
  
  # create a Tercen client object using the token
  client = rtercen::TercenClient$new(authToken=token)
#   client = rtercen::TercenClient$new(username='alex',password='xxxx')
  # https://tercen.com/core/index.html#ds/6a85a913c87ea0c3f35766d71c15864e/b910f840-aff0-11e5-90e9-a9e3f96ceaa1
#   workflowId = '6a85a913c87ea0c3f35766d71c15864e'
#   stepId='a2ca7f10-aff0-11e5-e88f-c9278cc0fb87'
  
  # get the cube query defined by your workflow
  query = client$getCubeQuery(workflowId, stepId)
  
  xAxiscolum = query$xaxisColumn
  if (is.null(xAxiscolum)) stop('A x axis is required')
  # TODO : check the type : must be numeric
  
  # execute the query and get the data
  cube = query$execute()
  
  nMatrixCol=cube$sourceTable$getNMatrixCols()
    
  ids = cube$sourceTable$getColumn("ids")$getValues()$getData()
  rows = floor(((ids -1) / nMatrixCol)) + 1
  row.df = cube$rowsTable$as.data.frame()
  row.df = lapply(row.df, as.character)
  row.df = data.frame(lapply(row.df, as.character), stringsAsFactors=FALSE)
  conditions = sapply(rows, function(ri){
    return(toString(row.df[ri,]))
  })
  
#   conditions = cube$sourceTable$getColumn("ids")$getValues()$getData()
  x = cube$sourceTable$getColumn(xAxiscolum$name)$getValues()$getData()
  y = cube$sourceTable$getColumn("values")$getValues()$getData()
  
  dat = data.frame(cell=conditions,conc=x,resp=y,stringsAsFactors=FALSE)
  
  dat <- dat[dat[,2]!=0,]

print(dat)
  
  return(split(dat, dat[,1]))
}

shinyServer(function(input, output, session) {
  
  Input <- reactiveValues(data = matrix(),
                          cells = character()
  )
  observe({
    tercenData = reactive({getTercenData(session)}) 
    Input$data <- tercenData()
    Input$cells <- names(tercenData())
    
    #       Input$data <- .getData(input$file1$datapath, input$header)
    #       Input$cells <- names(Input$data)
  })
  
  test <- reactive({
    if(is.null(Input$data))
      return(NULL)
    models <- lapply(Input$data, function(tmp){
      x <- tmp[,2]
      y <- tmp[,3]
      if(!is.numeric(x) || !is.numeric(y))
        return(NULL)
      if(input$props){
        y <- convertToProp(y, T0 = NULL, Ctrl = NULL)
      }
      npars <- ifelse(input$npar=="all", "all", as.numeric(input$npar))
      nplr(x, y, npars=npars, useLog=input$toLog, silent = TRUE)
    })
    models
  })
  
  
  exampleInput <- reactive({
    x <- c("cellLines", rep(c("cell1", "cell2"), each = 3), "...")
    y <- c("[Conc.]", format(rep(c(1.0, 2.5, 5.0), 2), digits = 2), "...")
    z <- c(1.021, 0.765, 0.432, 1.087, 0.654, 0.342)
    z <- c("Responses", round(z, 3), "...")
    note1 <- "Note: 'Responses' can be raw values, e.g. opt. densities."
    note2 <- "If so, be sure you check the 'Compute proportions' box."
    plot.new()
    legend("top", title = "Expected data format", legend = c(x, y, z), ncol = 3, bty = "n")
    legend("bottom", legend = c(note1, note2), bty = "n", cex = 1)
  })
  
  output$summary <- renderTable({
    models <- test()
    if(is.null(models))
      return(NULL)
    buildSummary(models)
  })
  
  output$message <- renderUI({
    #         if(is.null(input$file1$datapath)){
    #             return(
    #                 tags$p("Please, provide a file with cell lines in col1, [conc.] in col2 and responses in col3, as in this ",
    #                     tags$a(href="https://raw.github.com/fredcommo/nplr/master/inst/extdata/multicell.tsv", download="multicell.tsv", target="_blank", "Example File")
    # #                    tags$a(href="https://github.com/fredcommo/nplr/blob/master/inst/extdata/multicell.tsv", target="_blank", "Example file")
    #                     )
    #                 )
    #         } else
    if(is.null(Input$data)){
      return("This file format is not supported.")
    } else {
      nonNum <- sapply(Input$data, function(tmp){ !is.numeric(tmp[,2]) || !is.numeric(tmp[,3]) })
      if(any(nonNum)){
        msg <- sprintf("%s may contain non-numeric values", Input$cells[which(nonNum)])
        return(msg)
      }
    }
    return(NULL)
  })
  
  checkFile <- reactive({
    #             if(is.null(input$file1$datapath))
    if(length(Input$data) == 0)
      return(0)
    return(1)
  })
  
  welcomeImage <- reactive({
    return(
      list(
        src="images/welcome.png",
        filetype="image/png",
        alt="welcome-image"
      )
    )
  })
  
  
  output$checkFile <- renderText({ checkFile() })
  output$welcomeImage <- renderImage({ welcomeImage() }, deleteFile = FALSE)
  
  output$modelNames <- renderUI({
    models <- test()
    if(length(models)<1)
      return(NULL)
    else{
      items <- names(models)
      withTags(
        div(class="row",
            div(class="col-xs-12 radioText", "Set colors", style="margin-left: 15px; "),
            div(class="col-xs-12",
                lapply(.renderDiv(items), function(x) eval(parse(text=x)) )
            )
        )
      )
    }
  })
  
  # Put all the input colors in a vector
  getColors <- reactive({
    models <- test()
    items <- names(models)
    cols <- lapply(seq_len(length(items)), function(i) {
      input[[paste("col", i, sep="_")]]
    })
    unlist(cols)
  })
  
  output$plot <- renderPlot({
    if(is.null(input$file1$datapath))
      exampleInput()
    if(is.null(test()))
      return(NULL)
    
    models <- test()
    
    .multiCurve(models,
                showPoints = input$points,
                showMeans = input$Means,
                showSDerr = input$SDerr,
                pSize = input$pSize,
                lWidth = input$lWidth,
                legendSize = input$legendSize,
                showAsLog = input$showAsLog,
                Legend = input$showLegend,
                Cols = getColors(),
                xlab=input$xlabel, ylab=input$ylabel,
                las = 1
    )
    
  }, res=180, width=1033, height=875)
  
  output$downloadData <- downloadHandler(
    filename <- function(){sprintf("%s.xls", input$fname)},
    content <- function(file) {
      models <- test()
      if(is.null(models))
        return(NULL)
      out <- buildSummary(models)
      out <- cbind.data.frame(Params = rownames(out), out)
      write.table(out, file, sep="\t", row.names=FALSE)
    }
  )
  
  output$downloadPLot <- downloadHandler(
    filename <- function(){sprintf("%s.pdf", input$fname)},
    content <- function(file) {
      pdf(file, width=6, height=5)
      models <- test()
      if(is.null(models))
        return(NULL)
      
      .multiCurve(models,
                  showPoints = input$points,
                  showMeans = input$Means,
                  showSDerr = input$SDerr,
                  pSize = input$pSize,
                  lWidth = input$lWidth,
                  legendSize = input$legendSize,
                  showAsLog = input$showAsLog,
                  Legend = input$showLegend,
                  Cols = getColors(),
                  xlab=input$xlabel, ylab=input$ylabel,
                  las = 1
      )
      
      dev.off()
    },
    contentType = 'application/pdf'
  )
  
  
  session$onSessionEnded(function() { stopApp() })
  
})
