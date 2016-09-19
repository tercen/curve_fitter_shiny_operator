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
#     client = rtercen::TercenClient$new(username=getOption('tercen.username'),password=getOption('tercen.password'))
  # https://tercen.com/core/#ds/0496e9537627fbb9538acdfb96209c9b/ae250870-7340-11e6-871d-477da5546461
#     workflowId = '0496e9537627fbb9538acdfb96209c9b'
#     stepId='ae250870-7340-11e6-871d-477da5546461'
  
  # get the cube query defined by your workflow
  query = client$getCubeQuery(workflowId, stepId)
  
  xAxiscolum = query$xaxisColumn
  if (is.null(xAxiscolum)) stop('A x axis is required')
  # TODO : check the type : must be numeric
  
  # execute the query and get the data
  cube = query$execute()
  
  nMatrixCol=cube$sourceTable$getNMatrixCols()
  
  ids = cube$sourceTable$getColumn(".ids")$getValues()$getData()
  rows = floor(((ids -1) / nMatrixCol)) + 1
  row.df = cube$rowsTable$as.data.frame()
  row.df = lapply(row.df, as.character)
  row.df = data.frame(lapply(row.df, as.character), stringsAsFactors=FALSE)
  conditions = sapply(rows, function(ri){
    return(toString(row.df[ri,]))
  })
  
  x = cube$sourceTable$getColumn(rtercen::removeTablePrefix(xAxiscolum$name))$getValues()$getData()
  y = cube$sourceTable$getColumn(".values")$getValues()$getData()
  
  dat = data.frame(cell=conditions,conc=x,resp=y)
  
  dat <- dat[dat[,2] > 0,]
  
  dat = split(dat, dat[,1])
  
  return(dat)
}

shinyServer(function(input, output, session) {
  
  Input <- reactiveValues(data = NULL,
                          cells = NULL
  )
  
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
  
  output$summary <- renderTable({
    models <- test()
    if(is.null(models))
      return(NULL)
    buildSummary(models)
  })
  
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
  
  tercenData = reactive({
    getTercenData(session)
  })
  
  output$plot <- renderPlot({
    
    Input$data <- tercenData()
    Input$cells <- names(tercenData())
    
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
    
    hide('loader')
    
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
  
  #   session$onSessionEnded(function() { stopApp() })
  
})
