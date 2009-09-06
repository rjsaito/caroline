


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



plotRA <- function(x, subgroups=NULL, weights=NULL, prob=0.5, sig.lines=TRUE, asymp.lines=TRUE, epsilon=NULL, plot=TRUE ){

  ##################
  ## CREATE TABLE ##
  ##################
  ## CHECK 'subgroups' and 'weights' ARGUMENTS ##
  if(!is.null(subgroups)){
    if(class(subgroups) == 'list'){
      subgroup.list <- subgroups
    }else{
      stop("ERROR: subgroup must either be passed as a list or attached as a factor to x")
    }
  }
  if(!is.null(weights)){
    if(class(weights) != 'numeric'){
      stop("ERROR: these weights must be a passed as a numeric vector.")
    }
  }

  ## CHECK main 'x' ARGUMENT (either a dataframe or a matrix) ##
  if(class(x) == 'data.frame'){
    
    if(all(  names(x) %in% c('treatment', 'group','subgroup','weight'))){
      if(ncol(x) >= 2){

        x$treatment <- as.factor(x$treatment)
        x$group     <- as.factor(x$group)
        x$subgroup  <- as.factor(x$subgroup)
        
        if(nlevels(x$treatment) != 2){
          stop("ERROR: your treatment column needs to have exactly 2 levels")
        }
        ## CROSS TABULATE data.frame into treatments vs groups ## 
        count.tab <- as.table(by(x, list(x$group, x$treatment), nrow ))  
        
        ## handle the optional subgroup and weight columns of x 
        if(!is.null(x$subgroup)){
          if(!is.null(subgroups)){            
            stop("ERROR: you've specifed subgroups in both dataframe x and the subgroup param")
          }else{
            subgroup.list <- by(x, x$group, function(x) x$subgroup)
          }
        }
        if(!is.null(x$weight)){
          if(!is.null(weights)){            
            stop("ERROR: you've specifed weights in both dataframe x and the weights param")
          }else{
            weights <- as.vector(by(x$weight, x$group, mean))
          }
        }
                
      }else{
        print('ERROR: Need 2 columns (treatment & group) for dataframes. Convert to a (treatment) vector?')        
        return(FALSE)
      }
    }else{
      count.tab <- x  #just pass it on and check it out later
    }
    
  }else if(class(x) == 'table'){
    count.tab <- x    
  }else{
    stop("x must be either a dataframe or a matrix")
  }



  ## CHECK integerity of the table ## 
  if(class(count.tab[,1]) != 'integer' | class(count.tab[,1]) != 'integer')
    stop("ERROR: your matrix must be integer counts")
  if(!all(count.tab > 0, na.rm=TRUE))
    stop("ERROR: your matrix must be entirely postive numbers")
  if(ncol(count.tab) != 2)
    stop("ERROR: your matrix must have two columns (one for each treatment)")  
        

  ## CONVERT table it to a dataframe ##
  count.df <- tab2df(count.tab)
  treat.names <- names(count.df)
  

  ## CHECK dimentions of optional 'subgroup' and 'weight'  (combine with upper checks)
  if(!is.null(subgroups)){
    if(length(subgroups) != nrow(count.df)){
      stop("ERROR: Your subgroups list length doesn't match your count table row count.")      
    }
  }
  if(!is.null(weights)){
    if(length(weights) != nrow(count.df)){
      stop("ERROR: Your weights vector length doesn't match your count table row count.")
    }else if(!(range(weights)[1] >= 0 & range(weights)[2] <= 10^10)){
      stop("ERROR: your weights vector is not > 0")
    }else if(abs(1-mean(weights))>.1){
      stop("ERROR: your weights vector doesn't average to 1")
    }else{
      count.df$weight <- weights
      ## be sure to back up the original counts before we decimalize them
      count.df[,paste(treat.names[1],'ct',sep='.')] <- count.df[,treat.names[1]]
      count.df[,paste(treat.names[2],'ct',sep='.')] <- count.df[,treat.names[2]]
      
    }
  }else{
    count.df$weight <- 1
  }

  count.df$n <- apply(count.df[,treat.names], 1, sum)
  count.df$max  <- apply(count.df[,treat.names], 1, max)
  count.df[,treat.names] <- count.df[,treat.names] * count.df$weight

  avg.log <- function(a,b){
    return((log(a,2)+log(b,2))/2)
  }
  
  
  count.df$ratio <- log(count.df[,treat.names[1]]/count.df[,treat.names[2]], 2)
  ##count.df[!is.inf(count.df$ratio)] 
  count.df$average <- avg.log(count.df[,treat.names[1]], count.df[,treat.names[2]]) 


  if(!plot){  
    return(count.df)
  }else{

      ## remove the non bi-treatment represented
    if(!is.null(epsilon)){
      count.df[,treat.names] <- count.df[,treat.names] + epsilon
    }else{
      non.bi.treatment.represented <- as.logical(!apply(sapply(count.df, is.na),1,sum))
      count.df <- count.df[non.bi.treatment.represented,]
      subgroup.list <- subgroup.list[non.bi.treatment.represented]
    }

    ##########
    ## PLOT ##
    ##########

    if(!is.null(weights)){
      weight.label <- 'Weighted '
    }else{
      weight.label <- ''
    }
    if(!is.null(subgroup.list)){
      PCHs <- '.'  #better compliment to pie graph points
    }else{
      PCHs <- 1
    }
    
    xl <- range(count.df$average)+c(-1,1)
    yl <- range(count.df$ratio)+c(-1,1)
    
    plot(count.df$average, count.df$ratio,
         ylab=paste(weight.label,"Count Ratio: log2(",treat.names[1],"/",treat.names[2],")", sep=''),
         xlab=paste(weight.label,"Count Average:\n(log2(",treat.names[1],")+log2(",treat.names[2],"))/2", sep=''),
         main=paste("RA plot"), pch=PCHs, xlim=xl, ylim=yl)


       
    ## add ASSYMPTOTES
    if(asymp.lines){
      a <- .9
      b <- 1:100
      lines(avg.log(a,b), log(a/b,2))
      lines(avg.log(a,b), -log(a/b,2))
    }
      
    ## add CONFIDENCE INTERVAL lines
    
    if(sig.lines){
      ns <- round(seq(range(count.df$n)[1],range(count.df$n)[2]+10))
      sigs <- c(0.05, 0.01, 0.001, 0.5)
      sig.cols <- c(1,1,1,1)
      sig.ltys <- c(1,2,3,1)
      sig.lwds <- c(1,1,1,2) 
      for(i in seq(along=sigs)){
        
        a <- qbinom(p=sigs[i], size=ns, prob=prob)
        b <- qbinom(p=1-sigs[i], size=ns, prob=prob)

        if(!is.null(epsilon)){
          a <- a + epsilon
          b <- b + epsilon
        }else{
          non.zero <- a!=0 & b!=0
          a <- a[non.zero]
          b <- b[non.zero]
        }

        xs <- avg.log(a,b)[-1]
        ya <- log(a/b,2)[-1]
        yb <- log(b/a,2)[-1]
        # why are these saw shaped?
        
        ssa <- smooth.spline(xs,ya, nknots=4) # hack fix? 
        ssb <- smooth.spline(xs,yb, nknots=4) # hack fix? 
        lines(ssa, col=sig.cols[i], lwd=sig.lwds[i], lty=sig.ltys[i])
        lines(ssb, col=sig.cols[i], lwd=sig.lwds[i], lty=sig.ltys[i])
      }
      legend(max(xl)-1.5, min(yl)+1.5, legend=paste(100*(1-sigs),"% CI",sep=''), lty=sig.ltys, lwd=sig.lwds, col=sig.cols)
    }

    if(exists('subgroup.list')){
      
      ## add PIE CHART points
      
      ## figure out colors
      who.color.names <- unique(unlist(subgroup.list))
      who.colors <- rainbow(length(who.color.names))
      names(who.colors) <- who.color.names
      
      ## and sizes
      sig.sizes <- abs(.5 - pbinom(q=count.df$max, size=count.df$n, prob=prob))
      par(new=TRUE)
      pies(x=lapply(subgroup.list, table), x0=count.df$average, y0=count.df$ratio, radii=sig.sizes/10,
           xlim=xl ,ylim=yl,  color.table=who.colors)
      legend(max(xl)-1.5, max(yl), pch=16,col=who.colors,legend=names(who.colors))
    }
  }
}





### a better pie function with origin positions ###
pies <- function(x, show.labels = FALSE, show.slice.labels = FALSE, color.table = NULL, 
		radii = 0.2, x0=NULL, y0=NULL, xlim=c(-1,1), ylim=c(-1,1),
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
        list(x = radii[j] * cos(t2p), y =  radii[j] * sin(t2p))
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
