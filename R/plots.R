


makeElipseCoords <- function(x0 = 0, y0 = 0, b = 1, a = 1, alpha = 0, pct.range = c(0,1), len = 50){
  rad.range <- 2 * pi * pct.range
  theta <- seq(rad.range[1], rad.range[2], length=(len))
  x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
  y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
  cbind(x,y)
}


plotClock <- function(hour, minute, title, x0 = 0, y0 = 0, r = 1){  #ampm = "notUsed"
  
  circleXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.1*r, a = 1.1*r, alpha = 0, pct.range = c(0,1), len = 50)
  quarHourTickMarksXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.05*r, a = 1.05*r, alpha = (pi/2), pct.range = c((12*4-1)/(12*4),0), len = 12*4)
  hourLabelsXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .9*r, a = .9*r, alpha = (pi/2), pct.range = c(11/12,0), len = 12)

  polygon(circleXY)
  text(hourLabelsXY[,1],hourLabelsXY[,2],seq(1,12), cex=.5)
  text(quarHourTickMarksXY[,1],quarHourTickMarksXY[,2],".")

  minuteV <- minute/12
  minuteVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = r, a = r, alpha = 0, pct.range =  (.25 - c(minuteV,minuteV)), len = 1)
  segments(x0,y0,minuteVXY[1],minuteVXY[2])

  hourV <- hour/12
  hourVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .7*r, a =.7*r, alpha = 0, pct.range = (.25 - c(hourV,hourV)), len = 1)
  segments(x0,y0,hourVXY[1],hourVXY[2])  

}


### a better pie function with origin positions ###
pies <- function(x, show.labels = FALSE, show.slice.labels = FALSE, color.table = NULL, 
		radii = 1, x0=NULL, y0=NULL, xlim=c(-1,1), ylim=c(-1,1),
		edges = 200,  clockwise = FALSE, 
                init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                border = NULL, lty = NULL, main = NULL) 
{
  
  if(class(x)!='list')
    stop("x must be a list")
  
  if(length(x) != length(x0) | length(x0) != length(y0))
    stop(paste("x0 and y0 lengths (",length(x0),',',length(y0),") must match length of x (",length(x),")", sep=''))
  
  if(length(radii) < length(x))
    radii <- rep(radii, length.out=length(x))
  
  ## calculate the char size to pie radius conversions
  cx <- .25 * par('cxy')[1]
  cy <- .19 * par('cxy')[2]
  # old -> * (par('csi')/par('pin')[2]) * diff(ylim) * .2 # inches to coords scaling
  
  radii <- radii  
  y2x.asp <- diff(xlim)/diff(ylim)

  pie.labels <- names(x)

  if (is.null(color.table)) {
    unique.labels <- unique(unlist(lapply(x,names)))
    color.table <- rainbow(length(unique.labels))
    names(color.table) <- unique.labels
  }
  
  ## loop through the list of pie tables
  for(j in seq(along=x)){
    X <- x[[j]]
    data.labels <- names(X)

    if(j != 1)
      par(new=TRUE)
    if (!is.numeric(X) || any(is.na(X) | X < 0)) 
        stop("'x' values must be positive.")

    ## generate a slice fraction vector
    X <- c(0, cumsum(X)/sum(X))
    names(X) <- data.labels  #re-label it
    
    dx <- diff(X)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    if(all(xlim == c(-1, 1)) && all(ylim == c(-1,1))){
     if (pin[1] > pin[2]) 
         xlim <- (pin[1]/pin[2]) * xlim
     else ylim <- (pin[2]/pin[1]) * ylim
    }

    plot.window(xlim, ylim, "") #, asp = 1)
    col <- color.table[names(X)]

    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise) 
      -2 * pi
    else 2 * pi
    ## function to turn theta into xy coordinates
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radii[j] *cx * cos(t2p), y =  radii[j] * cy * sin(t2p)) #y
    }

    ## loop through each slice of the pie
    for (i in 1:nx) {
      lab <- as.character(names(X)[i])
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(X[i], X[i + 1], length.out = n))
      polygon(c(x0[j]+ P$x, x0[j]), c(y0[j] +P$y, y0[j]), density = density[i], angle = angle[i], 
              border = border[i], col = col[lab], lty = lty[i])
      P <- t2xy(mean(X[i + 0:1]))

      if (!is.na(lab) && nzchar(lab)) {
        if(show.slice.labels){
          lines(x0[j] +c(1, 1.05) * P$x, y0[j] +c(1, 1.05) * P$y)
          text(x0[j] +1.1 * P$x, y0[j] + 1.1 * P$y, lab, xpd = TRUE, 
               adj = ifelse(P$x < 0, 1, 0))
        }
      }
    }
    title(main = main)
    if(show.labels)
      text(x0[j],y0[j] + radii[j]+.2, pie.labels[j])
    
    invisible(NULL)
    
  }
}




## a better violin plot that takes a list as input ##
violins <- function (x, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
    horizontal = FALSE, col = "magenta", border = "black", lty = 1, 
    lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
    at, add = FALSE, wex = 1, drawRect = TRUE, main = "vioplot") 
{
   #library(vioplot)

    if(is.list(x)){                #dschruth added
    	datas <- x                 #dschruth added
    	if(length(names) == 0)     #dschruth added
    		names <- names(x)  #dschruth added
    }else{                         #dschruth added
    	datas <- list(x)
    }                              #dschruth added
    n <- length(datas)
    if (missing(at)) 
        at <- 1:n
    upper <- vector(mode = "numeric", length = n)
    lower <- vector(mode = "numeric", length = n)
    q1 <- vector(mode = "numeric", length = n)
    q3 <- vector(mode = "numeric", length = n)
    med <- vector(mode = "numeric", length = n)
    base <- vector(mode = "list", length = n)
    height <- vector(mode = "list", length = n)
    baserange <- c(Inf, -Inf)
    args <- list(display = "none")
    if (!(is.null(h))) 
        args <- c(args, h = h)
    for (i in 1:n) {
        data <- datas[[i]]
        data.min <- min(data)
        data.max <- max(data)
        q1[i] <- quantile(data, 0.25)
        q3[i] <- quantile(data, 0.75)
        med[i] <- median(data)
        iqd <- q3[i] - q1[i]
        upper[i] <- min(q3[i] + range * iqd, data.max)
        lower[i] <- max(q1[i] - range * iqd, data.min)
        est.xlim <- c(min(lower[i], data.min), max(upper[i], 
            data.max))
        smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
            args))
        hscale <- 0.4/max(smout$estimate) * wex
        base[[i]] <- smout$eval.points
        height[[i]] <- smout$estimate * hscale
        t <- range(base[[i]])
        baserange[1] <- min(baserange[1], t[1])
        baserange[2] <- max(baserange[2], t[2])
    }
    if (!add) {
        xlim <- if (n == 1) 
            at + c(-0.5, 0.5)
        else range(at) + min(diff(at))/2 * c(-1, 1)
        if (is.null(ylim)) {
            ylim <- baserange
        }
    }
    if (is.null(names)) {
        label <- 1:n
    }
    else {
        label <- names
    }
    boxwidth <- 0.05 * wex
    if (!add) 
        plot.new()
    if (!horizontal) {
        if (!add) {
            plot.window(xlim = xlim, ylim = ylim)
            axis(2)
            axis(1, at = at, label = label)
            title(main)
        }
        box()
        for (i in 1:n) {
            polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
                c(base[[i]], rev(base[[i]])), col = col[i], border = border,    #dschruth changed col to col[i]
                lty = lty, lwd = lwd)
            if (drawRect) {
                lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
                  lty = lty)
                rect(at[i] - boxwidth/2, q1[i], at[i] + boxwidth/2, 
                  q3[i], col = rectCol)
                points(at[i], med[i], pch = pchMed, col = colMed)
            }
        }
    }
    else {
        if (!add) {
            plot.window(xlim = ylim, ylim = xlim)
            axis(1)
            axis(2, at = at, label = label)
        }
        box()
        for (i in 1:n) {
            polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], 
                rev(at[i] + height[[i]])), col = col[i], border = border,  #dschruth changed col to col[i]
                lty = lty, lwd = lwd)
            if (drawRect) {
                lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
                  lty = lty)
                rect(q1[i], at[i] - boxwidth/2, q3[i], at[i] + 
                  boxwidth/2, col = rectCol)
                points(med[i], at[i], pch = pchMed, col = colMed)
            }
        }
    }
    invisible(list(upper = upper, lower = lower, median = med, 
        q1 = q1, q3 = q3))
}




