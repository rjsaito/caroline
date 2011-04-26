makeElipseCoords <- function(x0 = 0, y0 = 0, b = 1, a = 1, alpha = 0, pct.range = c(0,1), len = 50){
  rad.range <- 2 * pi * pct.range
  theta <- seq(rad.range[1], rad.range[2], length=(len))
  x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
  y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
  return(tab2df(cbind(x,y)))
}


plotClock <- function(hour, minute, x0 = 0, y0 = 0, r = 1){  
  
  circleXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.1*r, a = 1.1*r, alpha = 0, pct.range = c(0,1), len = 50)
  quarHourTickMarksXY <- makeElipseCoords(x0 = x0, y0 = y0, b = 1.05*r, a = 1.05*r, alpha = (pi/2), pct.range = c((12*4-1)/(12*4),0), len = 12*4)
  hourLabelsXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .9*r, a = .9*r, alpha = (pi/2), pct.range = c(11/12,0), len = 12)

  polygon(circleXY)
  text(hourLabelsXY[,1],hourLabelsXY[,2],seq(1,12), cex=.5)
  text(quarHourTickMarksXY[,1],quarHourTickMarksXY[,2],".")

  minuteV <- minute/60
  minuteVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = r, a = r, alpha = 0, 
  				pct.range =  (.25 - minuteV), len = 1)
  segments(x0,y0,minuteVXY$x[1],minuteVXY$y[1])

  hourV <- hour/12
  hourVXY <- makeElipseCoords(x0 = x0, y0 = y0, b = .7*r, a =.7*r, alpha = 0, 
  				pct.range = (.25 - c(hourV,hourV)), len = 1)
  segments(x0,y0,hourVXY$x,hourVXY$y)  

}

## function to grab par's usr param for use in subsequence plots
usr2lims <- function(adj=.04){
  
  par.usr <- par('usr')
  xlims <- par.usr[c(1,2)]
  ylims <- par.usr[c(3,4)]

  adj <- adj - adj^2 *2 # this simplifies the math below
  
  xlims <- xlims + c(adj,-adj) *  diff(xlims)
  ylims <- ylims + c(adj,-adj) *  diff(ylims)

  return(list(x=xlims, y=ylims))
}




### a better pie function with origin positions ###
pies <- function(x, show.labels = FALSE, show.slice.labels = FALSE, color.table = NULL, 
		radii = rep(2,length(x)), x0=NULL, y0=NULL, xlim=c(-1,1), ylim=c(-1,1),
		edges = 200,  clockwise = FALSE, 
                init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                border = NULL, lty = NULL, main = NULL, 
                other.color='gray', na.color='white', ...) 
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
        stop("'x' values must be non-missing positive.")


    if(length(X) == 0){
      warning(paste(names(x)[[j]], 'has zero length vector'))

    }else{
      
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

      ## change to gray all of the X names without colors in the color table
      nolgnd <- names(X)[!names(X) %in% names(color.table) ]
      color.table <- c(color.table, nv(rep(other.color,length(nolgnd)),nolgnd))
      col <- color.table[names(X)]
      col[names(col)=='NA'] <- na.color 

      if(length(border)> 1){
        if(length(border) != length(x))
          stop('length of border doesnt equal length of x')
        this.brdr <- border[j]
      }else{
        this.brdr <- border
      }
      
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
        nx <- as.character(names(X)[i])
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(X[i], X[i + 1], length.out = n))
        polygon(c(x0[j]+ P$x, x0[j]), c(y0[j] +P$y, y0[j]), density = density[i], angle = angle[i], 
                border = this.brdr, col = col[lab], lty = lty[i], ...)
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
}



#annulus()  #donut or ring plot


vennMatrix <- function(l){
  ## much of the code in this function was inspired by parts of Yongmin Sun's doVennDiagram function
  if(is.null(names(l)))
    stop("The list 'l' must have named elements")
  l.all <- unique(do.call(c,lapply(l, as.character)))
  l.mat <- matrix(0, nrow = length(l.all), ncol = length(l))
  colnames(l.mat) <- names(l)
  rownames(l.mat) <- l.all

  for(i in 1:length(l.all)) 
    for(nm in names(l))
      l.mat[i,nm] <- l.all[i] %in% l[[nm]]
  return(l.mat)
}



text.plot <- function(..., x=1, y=1){
  plot(x, y, pch='', bty='n',xaxt='n',yaxt='n', xlab='', ylab='')
  text(x, y, ...)
}




mvlabs <- function(df, n=nrow(df), x='x', y='y', l='lab', cols=colors()[grep("dark",colors())], ...){

  for(i in (1:n)+1){

    ## identify point to move
    idx <- identify(x=df[,x], y=df[,y], labels=df[,l],n=1)
    print(df[idx,])
    ## locate new location to move to
    locs <- locator(n=1)

    ## move the point  (refresh the plot?)
    df[idx, x] <- locs$x[1]
    df[idx, y] <- locs$y[1]
    #points(x=df[idx, x], y=df[idx, y], col=colors()[i])
    text(x=df[idx, x], y=df[idx, y], labels=as.character(df[idx,l]), col=cols[i], ...)

  }
  return(df)
}



labsegs <- function(x0, y0, x1, y1, buf=.3, ...){

  a <- x1 - x0
  b <- y1 - y0
  c0 <- sqrt(a^2 + b^2)
  theta <- atan(b/a)
  theta[a<0] <- theta[a<0] + pi
  c1 <- c0 - buf
  
  a1 <- c1*cos(theta)
  b1 <- c1*sin(theta)

  x1 <- x0 + a1
  y1 <- y0 + b1

  segments(x0,y0,x1,y1,...)
}
