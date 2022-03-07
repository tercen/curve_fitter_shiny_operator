
############################
# HELPERS
############################
as.drda.obj <- function(m_, tmp, isLog = FALSE, mfunc='NA', cell='CELL'){
  coeff <- unlist( m_$coefficients, use.names = FALSE)
  
  x <- m_$model$x
  y <- m_$model$y
  
  interpVals <- approx(x, y,  n = length(x) * 50, t='mean')
  
  xCurve <- (interpVals$x)
  
  

  if(m_$mean_function == 'logistic2'){
    yCurve <- 1 / (1 +  exp(-coeff['eta'] * (xCurve - coeff['phi']) ))
  }
  
  if(m_$mean_function == 'logistic4'){
    yCurve <- coeff['alpha'] + (coeff['beta'] -coeff['alpha']  ) / 
      (1 +  exp(-coeff['eta'] * (xCurve - coeff['phi']) ))
  }
  
  if(m_$mean_function == 'logistic5'){
    yCurve <- coeff['alpha'] + (coeff['beta'] -coeff['alpha']  ) / 
      (1 + coeff['nu'] * exp(-coeff['eta'] * (xCurve - coeff['phi']) ))^(1 / coeff['nu'])  
  }
  
  if(m_$mean_function == 'gompertz'){
    yCurve <- coeff['alpha'] + (coeff['beta'] -coeff['alpha']  ) *
       exp(-exp(-coeff['eta'] * (xCurve - coeff['phi']) ) )
  }
  
  
  # Calculate IC50 & EC50

  
  idx <- which.min(diff(yCurve))+1
  inflPoint <- list('x'= xCurve[idx], 'y'= yCurve[idx])
  
  
  infl <- c(FALSE, diff(diff(yCurve)>0)!=0)
  
  # IC50 - an estimate of the dose required to produce 50% of the maximal result
  ic50 <- list( 'x'=xCurve[ which.max( yCurve / max(yCurve) <= 0.5 ) ],
                'y'=yCurve[ which.max( yCurve / max(yCurve) <= 0.5 ) ] 
  )
  
  
  #ic50 <- lapply(models, function(model){
  #  estim <- getEstimates(model, .5)
  #  interv <- sprintf("[%s | %s]",
  #                    format(estim$x.025, digits=3, scientific = TRUE),
  #                    format(estim$x.975, digits=3, scientific = TRUE)
  #  )
  #  cbind(resp = format(estim$y, digits = 2),
  #        IC = format(estim$x, digits = 3, scientific = TRUE),
  #        "[95%]" = interv)
  #})
  #browser()
  drdaList <- list( 
    y = m_$model$y, 
    x = m_$model$x,
    yFit = m_$fitted.values, 
    w = m_$weights,
    loglikelihood = m_$loglik,
    coeff = m_$coefficients,
    aic = AIC(m_),
    bic = BIC(m_),
    npar=mfunc,
    n = m_$n,
    converged = m_$converged,
    #auc = nauc(m_),
    xCurve=xCurve,
    yCurve=yCurve,
    inflPoint=inflPoint,
    ic50=ic50,
    cellName=cell
  )
  
  class(drdaList) <- "drdaObject"
  return(drdaList)
}


.getData <- function(filepath, h){
  if (is.null(filepath))
    return(NULL)

  else{
    dat <- try(read.delim(filepath, header=h, sep="\t"))
    if(inherits(dat, "try-error") || ncol(dat)<3)
      dat <- try(read.delim(filepath, header=h, sep=","))
    if(inherits(dat, "try-error") || ncol(dat)<3)
      dat <- try(read.delim(filepath, header=h, sep=";"))
    if(inherits(dat, "try-error") || ncol(dat)<3)
        dat <- try(read.delim(filepath, header=h, sep=" "))
    if(inherits(dat, "try-error") || ncol(dat)<3){
          cat("format not supported.\n")
          return(NULL)
        }

    # Check whether ',' is used as decimal sep
    if(any(grepl(",", dat[,2])))
    	dat[,2] <- as.numeric(as.character(gsub(",", ".", dat[,2])))
    if(any(grepl(",", dat[,3])))
    	dat[,3] <- as.numeric(as.character(gsub(",", ".", dat[,3])))

    dat <- dat[dat[,2]!=0,]
    return(split(dat, dat[,1]))
  }
}

############################
# HELPERS PLOT
############################
# .plot <- function(object, showAsLog,...){
#     x <- getX(object)
#     y <- getY(object)
# 
#     plot(x, y, type = "n", bty = "n", axes = FALSE, cex.axis = .85,
#         ylim = range(min(c(y, 0), na.rm = TRUE), max(c(y, 1), na.rm = TRUE)+.1), ...)
#     .addXaxis(x, showAsLog)
#     .addYaxis(y)
#     
#     if(min(y, na.rm = TRUE) < 0)
#         abline(h = 0, lty = 2)
#     if(max(y, na.rm = TRUE) > 1)
#         abline(h = 1, lty = 2)
# }
.addPolygon <- function(object){
    newx <- getXcurve(object)
    newy <- getYcurve(object)
    bounds <- nplr:::.confInt(getStdErr(object), getY(object), getFitValues(object), newy)
    xx <- c(newx, rev(newx))
    yy <- c(bounds$lo, rev(bounds$hi))
    polygon(xx, yy, border = NA, col = rgb(.8,.8,.8,.4))
}
# .addEstim <- function(object, showEstim, unit, B, conf.level){
#     stdErr <- getStdErr(object)
#     estim <- nplr:::.estimateRange(showEstim, stdErr, getPar(object)$params, B, object@useLog, conf.level)
#     newx <- getXcurve(object)
#     newy <- getYcurve(object)
#     legend1 <- sprintf("IC%d : %s%s", showEstim*100, format(estim[2], scientific=TRUE, digits=2), unit)
#     legend2 <- sprintf("[%s, %s]", format(estim[1], scientific=TRUE, digits=2), format(estim[3], scientific=TRUE, digits=2))
#     legend(ifelse(newy[length(newy)]<newy[1], 'bottomleft', 'topleft'),
#            legend = c(legend1, legend2), cex = 1, text.col = 'steelblue4', bty = 'n')
    
# }
# .addGOF <- function(object){
#     gof <- format(getGoodness(object), digits=3, scientific = TRUE)
#     newx <- getXcurve(object)
#     newy <- getYcurve(object)
#     legend(ifelse(newy[length(newy)]<newy[1], 'topright', 'bottomright'),
#            legend = paste('Goodness of fit:', gof), bty = 'n', cex = 1)
# }
.addPoints <- function(object, pcol, pSize, lib='nplr', ...){
  if(lib=='nplr'){
    x <- getX(object)
    y <- getY(object)
    
  }else{
    x <- .getXDrda( object, use.names=FALSE )
    y <- .getYDrda( object, use.names=FALSE )
  }
    
    points(x, y, col = pcol, pch = 19, cex = 3*pSize)
#    points(x, y, pch = 1)
}
.addCurve <- function(object, lcol, lWidth, lib='nplr',...){
    if(lib == 'nplr'){
      x <- getXcurve(object)
      y <- getYcurve(object)
      
    }else{
      x <- .getXCurveDrda( object, use.names=FALSE )
      y <- .getYCurveDrda( object, use.names=FALSE )
    }
    lines(y ~ x, col=lcol, lwd=6*lWidth, ...)
}
.SE <- function(x, y){
    .len <- function(x){ sum(!is.na(x)) }
    n <- by(y, x, .len)
    er <- by(y, x, sd, na.rm = TRUE)
    sEr <- as.vector(er/sqrt(n))
    sEr    
}
.addMeans <- function(object, pSize, lib='nplr',...){
    if(lib=='nplr'){
      x <- getX(object)
      y <- getY(object)
      
    }else{
      x <- .getXDrda( object, use.names=FALSE )
      y <- .getYDrda( object, use.names=FALSE )
    }
    
    my <- as.vector(by(y, x, mean, na.rm = TRUE))
    points(unique(x), my, pch = 19, cex = 3*pSize, ...)
}
.addErr <- function(object, pSize, lib='nplr',...){
  if(lib=='nplr'){
    x <- getX(object)
    y <- getY(object)
    
  }else{
    x <- .getXDrda( object, use.names=FALSE )
    y <- .getYDrda( object, use.names=FALSE )
  }
    my <- as.vector(by(y, x, mean, na.rm = TRUE))
    sEr <- .SE(x, y)
    e <- diff(range(x, na.rm = TRUE))/60
    segments(x0 = unique(x), y0 = my - sEr, y1 = my + sEr, lwd = 5*pSize, ...)
    segments(x0 = unique(x) - e, x1 = unique(x) + e, y0 = my - sEr, lwd = 5*pSize, ...)
    segments(x0 = unique(x) - e, x1 = unique(x) + e, y0 = my + sEr, lwd = 5*pSize, ...)
}
.addXaxis <- function(x, showAsLog){
    x <- unique(x)
    x <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), len = length(x))
    if(!showAsLog){
        l <- 10^seq(-20, 20)
        axis(1, at = log10(l), labels = format(l, digits = 1, scientific = TRUE), cex.axis = .85)
    } else{
        axis(1, at = x, labels = format(x, digits = 2, scientific = FALSE), cex.axis = .85)
    }
}
.addYaxis <- function(y){
    y <- seq(-1, 2, by = .25)
    axis(2, at = y, labels = format(y, digits = 2), cex.axis = .85, las = 1)
}

.getXDrda <- function(mdl, use.names = TRUE){
  return( unlist(setNames(as.list(mdl$x), unlist(lapply( 1:length(mdl$x), function(i) paste0(mdl$cellName, i) ))), use.names = use.names) )
}

.getYDrda <- function(mdl, use.names = TRUE){
  return( unlist(setNames(as.list(mdl$y), unlist(lapply( 1:length(mdl$y), function(i) paste0(mdl$cellName, i) ))), use.names = use.names) )
  
}

.getXCurveDrda <- function(mdl, use.names = TRUE){
  return( unlist(setNames(as.list(mdl$xCurve), unlist(lapply( 1:length(mdl$xCurve), function(i) paste0(mdl$cellName, i) ))), use.names = use.names) )
}

.getYCurveDrda <- function(mdl, use.names = TRUE){
  return( unlist(setNames(as.list(mdl$yCurve), unlist(lapply( 1:length(mdl$yCurve), function(i) paste0(mdl$cellName, i) ))), use.names = use.names) )
  
}

.multiCurve <- function(models, showPoints = FALSE, showMeans = FALSE, showSDerr = TRUE, pSize = 1, lWidth=1, legendSize=1, showAsLog = TRUE, Legend = TRUE, Cols = NULL, used_lib = 'nplr',...){


    showAsLog <- ifelse(showAsLog == "TRUE", TRUE, FALSE)
    K <- length(models)
    if(used_lib == 'drda')
    {

      allX <- do.call(c, lapply(models, function(tmp) .getXDrda(tmp) ))
      allY <- do.call(c, lapply(models, function(tmp) .getYDrda(tmp) ))
    }else{
      allX <- do.call(c, lapply(models, function(tmp) getX(tmp) ))
      allY <- do.call(c, lapply(models, function(tmp) getY(tmp) ))
    }
    

    plot(range(allX, na.rm = TRUE), range(min(allY, na.rm = TRUE), max(allY, na.rm = TRUE)+.3),
         type = "n", bty = "n", axes = FALSE, cex.axis = .95,
         ylim = range(min(min(allY, na.rm = TRUE), 0), max(max(allY, na.rm = TRUE), 1.25)+.2), ...)

    .addXaxis(allX, showAsLog)
    .addYaxis(allY)

    if(is.null(Cols))
        Cols <- rep("black", K)

    for(k in seq_len(K)){
        tmp <- models[[k]]
        Col <- Cols[k]
        if(showPoints) .addPoints(tmp, pSize = pSize, pcol = Col, lib=used_lib, ...)
        if(showMeans) .addMeans(tmp, pSize = pSize, col = Col, lib=used_lib, ...)
        if(showSDerr) .addErr(tmp, pSize, col = Col, lib=used_lib, ...)
        .addCurve(tmp, Col, lWidth, lib=used_lib)
    }

    if(min(allY, na.rm = TRUE) < 0)
        abline(h = 0, lty = 2)
    if(max(allY, na.rm = TRUE) > 1)
        abline(h = 1, lty = 2)
    
    if(Legend){
        nm <- length(names(models))
        nc <- sum(nchar(names(models)))
#        Cex <- 1 - min(nc/90, .7)
        nc <- nc + 10*nm
        Cex <- 1.5*legendSize
        if(nc > 100){
            K <- ceiling(K/2)            
        }
        legend("top", legend = names(models), ncol = K,
            col = Cols, bty = "n", cex = Cex, lwd = 2, pch = 19)
    }
}
############################
# HELPERS color picker
############################
.renderDiv <- function(items){
    out <- c()
#     for(item in items){
#         txt <- sprintf("div(class = 'col-sm-12', checkboxInput('', '%s', FALSE))", item)
#         out <- c(out, txt)
#     }
    N <- length(items)
    for(ii in seq_len(N)){
        item <- items[ii]
        col_i <- sprintf('col_%s', ii)
        txt <- sprintf(
            "div(class = 'col-md-12', style='display: inline',
                div(class = 'col-xs-7 cellName', '%s'),
                div(class = 'col-xs-5 colPicker', colourInput('%s', '', '#888'))
            )",
            item, col_i)
        out <- c(out, txt)
    }
    out
}

############################
# HELPERS SUMMARY TABLE
############################
buildDrdaSummary <- function(models){
  #drdaList <- list( 
  #  y = m_$model$.y, 
  #  x = m_$model$lx,
  #  yFit = m_$fitted.values, 
  #  w = m_$weights,
  #  loglikelihood = m_$loglik,
  #  coeff = m_$coefficients,
  # aic = AIC(m_),
  #  bic = BIC(m_),
  #  n = m_$n,
  #  converged = m_$converged,
  #  auc = nauc(m_, c(-100,100), c(0,1)),
  #  xCurve=xCurve,
  #  yCurve=yCurve,
  #  inflPoint=inflPoint,
  #  cellName=cell
  #)
  
  cellLine <- names(models)
  drdav <- as.character(packageVersion("drda"))
  drdaDate <- as.character(packageDescription("drda")["Built"])
  drdaDate <- strsplit( drdaDate, ';' )[[1]][[3]]
  rv <- as.character(version["version.string"])

  out <- cbind(
    cellLine,
    "Mean Function"=lapply(models, function(tmp) tmp$npar ) ,
    "LogLikelihood"=lapply(models, function(tmp) format(tmp$loglikelihood, digits=3 ) ), 
    "AIC"=lapply(models, function(tmp) format(tmp$aic, digits=3 ) ),
    "BIC"=lapply(models, function(tmp) format(tmp$bic, digits=3 ) ) ,
    "AUC"=lapply(models, function(tmp) format(tmp$auc, digits=3 ) ),
    "Inflection X"=lapply(models, function(tmp) format(tmp$inflPoint$x, digits=3 ) ),
    "Inflection Y"=lapply(models, function(tmp) format(tmp$inflPoint$y, digits=3 ) ),
    "date (Y-m-d)" = format(Sys.Date(), "%Y-%m-%d") ,
    "drda version" = sprintf("%s (%s)",drdav, drdaDate),
    "R version" = gsub("R version ", "", rv)
    
  )
  
  rownames(out) <- sprintf("model-%s", seq_len(length(rownames(out))))
  out <- as.data.frame(t(out))

  out  
}


buildNplrSummary <- function(models){
  pars <- lapply(models, function(model){
    p <- getPar(model)
    p <- unlist(p)
    p[-1] <- format(p[-1], digits = 3, scientific = TRUE)
    p
  })
  gof <- lapply(models, function(model) format(getGoodness(model), digits = 3, scientific = TRUE) )
  auc <- lapply(models, function(model) signif(getAUC(model), 3) )
  inflpt <- lapply(models, function(model){
    infl <- as.numeric(getInflexion(model))
    cbind.data.frame(
      xInfl = format(infl[1], digits = 3),
      yInfl = format(infl[2], digits = 3)
    )
  })
  ic50 <- lapply(models, function(model){
    estim <- getEstimates(model, .5)
    interv <- sprintf("[%s | %s]",
                      format(estim$x.025, digits=3, scientific = TRUE),
                      format(estim$x.975, digits=3, scientific = TRUE)
    )
    cbind(resp = format(estim$y, digits = 2),
          IC = format(estim$x, digits = 3, scientific = TRUE),
          "[95%]" = interv)
  })
  nplrv <- as.character(packageVersion("nplr"))
  nplrDate <- as.character(packageDescription("nplr")["Date"])
  rv <- as.character(version["version.string"])
  
  
  parsDf <- t(data.frame(pars)) 
  rownames(parsDf) <- NULL
  
  gofDf <- t(data.frame(gof)) 
  rownames(gofDf) <- NULL
  
  aucDf <- do.call(rbind, auc)
  rownames(aucDf) <- NULL
  
  inflptDf <- do.call(rbind, inflpt)
  rownames(inflptDf) <- NULL
  
  ic50Df <- do.call(rbind, ic50)
  rownames(ic50Df) <- NULL
  
  out <- data.frame( cellLine = names(models) ) %>%
    cbind( parsDf  ) %>%
    cbind( gofDf  )  %>%
    cbind( aucDf )   %>%
    cbind( inflptDf )   %>%
    cbind( ic50Df )   %>%
    cbind( data.frame( "date (Y-m-d)" = format(Sys.Date(), "%Y-%m-%d") ) ) %>%
    cbind( data.frame( "nplr version" = sprintf("%s (%s)",nplrv, nplrDate) ) ) %>%
    cbind( data.frame( "R version" = gsub("R version ", "", rv) ) ) 
  
  rownames(out) <- sprintf("model-%s", seq_len(length(rownames(out))))
  out <- as.data.frame(t(out))
  out
}


buildSummary <- function(models, lib = 'nplr'){
  if( lib == 'nplr' ){
    buildNplrSummary(models)
  }else if( lib == 'drda' ){
    buildDrdaSummary(models)    
  }
  

}