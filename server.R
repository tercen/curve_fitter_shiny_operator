########################################
#
# nplr App deployed on Synapse
#
########################################

## REQUIRED
library(nplr)
#library(XLConnect)
library(shinyjs)
library(tercen)
library(dplyr)
library(tidyr)

library(drda)

source("helpers.R")

############################


# http://127.0.0.1:5402/admin/w/b69f8f196272abf4573f6c6948005e09/ds/65fe42d6-50f6-407c-b6c8-b21eec58fa5b
# options('tercen.workflowId' = 'b69f8f196272abf4573f6c6948005e09')
# options('tercen.stepId' = '65fe42d6-50f6-407c-b6c8-b21eec58fa5b')


getData <- function(session){
  #browser()
  ctx <- getCtx(session)
  cellName <- list( unlist(ctx$rnames) )[[1]]
  df0 <- ctx$select(c('.x', '.y', '.ri') )
  df1 <- ctx$rselect(c( cellName ) ) %>%
    mutate( '.ri' = seq(0, nrow(.)-1)) 
  
  df <- df0 %>%
    left_join(df1, by='.ri'  )
  
  dat = data.frame(cell=df[cellName],conc=df['.x'],resp=df['.y'])
  
  #dat <- dat[dat[,2] > 0,]
  
  dat = split(dat, dat[,1])
  
  return(dat)
}

getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
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
      
      #browser()
      if(input$lib == 'nplr'){
        if( input$weighting == TRUE ){
          mdl <- nplr(x, y, npars=npars, useLog=input$toLog, silent = TRUE,
               method='res', LPweight=2)
        }else{
          mdl <- nplr(x, y, npars=npars, useLog=input$toLog, silent = TRUE)
          
        }
      }else if(input$lib == 'drda'){
        
        isLog <- !input$toLog
        tmp$lx <- tmp$.x
        if( !isLog ){
          tmp$lx <- log10(tmp$lx)
        }
        
        w <- tmp$lx *0 + 1
        if(input$weightingd == TRUE){
          w <- 1 / ( (tmp$.y)**2  )
        }
        
        cellName <- tmp$cell[[1]]
        return(as.drda.obj(drda(.y ~ lx, data = tmp, 
                                mean_function = input$npard,
                                weights = w,
                                is_log=TRUE), tmp, isLog = TRUE, cell=cellName))
        
        
      }
      return(mdl)
    })
    
    return(models)
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
    getData(session)
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
                las = 1,
                used_lib = input$lib
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
