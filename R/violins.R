

.vio.stats <- function (x,by)       # adapted from http://legacy.ncsu.edu/ST370/distance/rlab/
                                        # with robust range from package sfsmisc Zurich
                                        # 'by' grouping variable presumes that x is univariate
{
                                        #    if (!missing(by)) {
                                        #        x <- cat.to.list(c(x), by)
  if (!missing(by)) { # handle the 'by' variable
    if (!is.numeric(x)) x<-as.numeric(simplify2array(x))
    if (!is.numeric(by)) by<-as.numeric(simplify2array(by))
    x <- .cat.to.list(x[order(by)],sort(by)) 
  }
  if (!is.list(x) & !is.matrix(x))
    x <- matrix(x, ncol = 1)
  if (is.list(x)) {
    nc <- length(x)
    out <- matrix(NA, ncol = nc, nrow = length(.distDescription()))
    dimnames(out) <- list(.distDescription(), names(x))
    for (j in (1:nc)) {
      if (!is.factor(x[[j]])) {
        if (is.numeric(x[[j]])) {
          out[, j] <- .distDescription(x[[j]])  #list
        }
      }
    }
    return(noquote(out))
  }
  if (is.matrix(x)) {
    nc <- ncol(x)
    out <- matrix(NA, ncol = nc, nrow = length(.distDescription()))
    dimnames(out) <- list(description(), dimnames(x)[[2]])
    for (k in (1:nc)) {
      if (!is.factor(x[[k]])) {
        out[, k] <- .distDescription(x[, k])  #matrix
      }
    }
    return(noquote(out))
  }
}

.distDescription <- function (x)
{
  lab <- c("N", "mean", "SD", "robust min", "min", "Q1", "median",
           "Q3", "max", "robust max",
           "skew", "kurtosys","Huber mu","missing values","unique values","Shapiro p")
  if (missing(x)) {
    return(lab)
  }
  temp <- rep(0, length(lab))
  xt <- x[!is.na(x)]
  n <- length(xt)
  if (!is.numeric(xt) || all(is.na(x))) {
    return(c(n, rep(NA, 12), length(x) - length(xt), NA, NA))
  }
  else {
    if (n < 4) {
      xt <- sort(xt)
      if (n < 2) {
        return(sprintf("%10.4f",c(n, xt[1], NA, rep(c(NA,xt[1]), 3), rep(NA,4), length(x) -
                                  length(xt),length(unique(xt)),NA)))
      }
      if (n == 2) {
        return(sprintf("%10.4f",c(n, mean(xt), sqrt(var(xt)), .rrange(xt)[1], c(xt[1], xt[1],
                                                                                mean(xt), xt[2], xt[2]), .rrange(xt)[2], rep(NA,3), length(x) - length(xt),length(unique(xt)),NA)))
      }
      if (n == 3) {
        return(sprintf("%10.4f",c(n, mean(xt), sqrt(var(xt)), .rrange(xt)[1], c(xt[1], xt[1],
                                                                                xt[2], xt[3], xt[3]), .rrange(xt)[2], rep(NA,3), length(x) - length(xt),length(unique(xt)),NA)))
      }
    }
    else {
      if (length(unique(x[!is.na(x)]))>1) {
        if(length(x)<5001) {    shapp<-shapiro.test(x)  }}
      else  { shapp<-NULL              #trap shapiro's with no variability
              shapp$p<-NA
            }
      if(length(x)>5000) { shapp<-NULL
                           shapp$p<-NA    }

      return(sprintf("%10.4f",c(length(xt), mean(xt), sqrt(var(xt)), .rrange(xt)[1], min(xt),
                                quantile(xt, c(0.25, 0.5, 0.75)), max(xt), .rrange(xt)[2], .skewed(xt), .kurtosys(xt),.huber.mu(xt),
                                length(x) - length(xt),length(unique(xt)),shapp$p)))     #
    }
  }
}



# fields, Tools for spatial data
# Copyright 2004-2007, Institute for Mathematics Applied Geosciences
# University Corporation for Atmospheric Research
# Licensed under the GPL -- www.gpl.org/licenses/gpl.html
.cat.to.list <- function(x, a) {
    a <- as.character(a)
    label <- unique(a)
    out <- as.list(1:length(label))
    names(out) <- label
    for (k in 1:length(label)) {
        out[[k]] <- x[label[k] == a]
        if (length(out[[k]]) == 0) 
            out[[k]] <- NA
    }
    out
}

.elimna <- function (m) 										
  {
    m <- as.matrix(m)
    ikeep <- c(1:nrow(m))
    for (i in 1:nrow(m)) if (sum(is.na(m[i, ]) >= 1)) 
      ikeep[i] <- 0
    elimna <- m[ikeep[ikeep >= 1], ]
    elimna
  }

.huber.mu <- function (x, c = 1.28, iter = 20, conv = 1e-07) 	# from asbio::huber.mu
  {
    mu.hat <- .huber.NR(x, c, iter)
    mu.est = max(mu.hat)
    mu.est
  }

.huber.NR <- function (x, c = 1.28, iter = 20) 
  {
    mu.k <- matrix(nrow = iter, ncol = 1)
    mu.k[1] <- median(x)
    for (i in 1:iter) {
      {
        A1 <- (x - mu.k[i])/mad(x)
        A <- sum(sapply(A1, function(x) {
          max(-c, min(c, x))
        }))
        B1 <- (A1 >= c | A1 <= -c)
        B <- length(B1[B1 == FALSE])
        mu.k[i + 1] <- mu.k[i] + ((mad(x) * A)/B)
      }
    }
    mu.k
  }

.kurtosys <-function (x)
{ # from Tom Fletcher
  n <- length (x[!(is.na(x))])
  sd <- sqrt(var(x,na.rm=T))
  m <- mean(x,na.rm=T)
  (((n*(n+1))/((n-1)*(n-2)*(n-3)))*(sum(((x-m)/sd)^4, na.rm=T)))-((3*(n-1)^2)/((n-2)*(n-3)))
}

.mad <- function (x, center = median(x), constant = 1.4826, na.rm = FALSE, 
                  low = FALSE, high = FALSE) 
{
  if (na.rm) 
    x <- x[!is.na(x)]
  n <- length(x)
  constant * if ((low || high) && n%%2 == 0) {
    if (low && high) 
      stop("'low' and 'high' cannot be both TRUE")
    n2 <- n%/%2 + as.integer(high)
    sort(abs(x - center), partial = n2)[n2]
  }
  else median(abs(x - center))
}

.rrange <-function (x, range = 1, coef = 1.5, na.rm = TRUE)
{                                    ### robust range from package sfsmisc Zurich
  if (!missing(range)) {
    if (!missing(coef))
      stop("Must use either 'range' or 'coef'")
    coef <- 1.5 * range
  }
  if (!na.rm && any(is.na(x)))
    return(0 + c(NA, NA))
  boxplot.stats(x, coef = coef, do.conf = FALSE, do.out = FALSE)$stats[c(1,5)]
} 


# Tom Fletcher, University of Missouri - St. Louis
# http://www.umsl.edu/~fletchert/quant/DataScreen.txt
.skewed <-function (x)
{
  n <- length (x[!(is.na(x))])
  sd <- sqrt(var(x,na.rm=T))
  m <- mean(x,na.rm=T)
  (n/((n-1)*(n-2)))*sum(((x-m)/sd)^3, na.rm=T)
}


# from package asbio
# Maintainer: Ken Aho <kenaho1@gmail.com>
.ci.median<-function(x,conf=.95){
  n<-nrow(as.matrix(x))
  if(qbinom((1-conf)/2,n,0.5)==0)
    stop("CI not calculable")
  L<- qbinom((1-conf)/2,n,0.5)
  U<-n-L+1
  if(L>=U)
    stop("CI not calculable")
  order.x<-sort(x)
  res<-list()
  res$head<-paste(paste(as.character(conf*100),"%",sep=""),
                  c("Confidence Interval for Population Median"))
  res$ci<-c(median=median(x),lower=order.x[L],upper=order.x[n-L+1])
  res$ends<-c("Estimate",
              paste(as.character(c((1-conf)/2,1-((1-conf)/2))*100),"%",sep=""))
  res$coverage<-1-(2*pbinom(q=L-1,n,0.5))
  class(res)<-"ci"
  res
}



violins <- function (x, by, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                     horizontal = FALSE, col = "transparent", border = "black", lty = 1, 
                     lwd = 1, rectCol = "grey50", colMed = "grey80", pchMed = 19, 
                     at, add = FALSE, wex = 1, drawRect = TRUE, main = "", connect=TRUE,
                     connect.col='lightgreen', connect.lty=1, connect.lwd=1,
                     xlab="",ylab="", stats=FALSE, ci.median_mu=TRUE) 
  
  ## an improved violin plot by 
  ## Dave McArthur <dmca@ucla.edu>
  ## that takes a list or dataframe as input
  ## lightly adapted 2/19/10 from package 'caroline'  
  ## David M. Schruth <dschruth at u.washington.edu>  2/12/10
  ## who migrated the function out of the vioplot library
  ##
  ##   stats, violins
  ##      mods 4/25/11
  ##       D L McArthur, David Geffin School of Medicine at UCLA   dmca <at> ucla.edu
  ##
  ## a further improved violin plot adapted from package caroline 
  ##  includes 95% ci.median and huber.mu M-estimator, 
  ##     both from package asbio (“Applied Statistics for Biologists”)
  ##  and handles variables that contain no data or are factors
  ##
  ## Provide either a list or dataframe as input, with names or not (list need not be rectilinear)
  ##   'by' grouping variable presumes that x is univariate and length(grouping var)=length(x)
  ##   'at' can be specific list of positions or single incrementer
  ##   'las = 2'...Set tick label orientation (1=horizontal, 2=vertical)
  ##   'ci.median_mu' portrays median confidence intervals and Huber mu


{
  #options(warn=-1)
  require(sm)	## call required library if not already loaded
  xx<-x  
  if(is.data.frame(x)) x<-as.list.data.frame(x)  ## convert dataframe to list if needed
  if (!missing(by)) {                           ## handle 'by' variable
    if(is.numeric(by)) x<-.cat2list(x[order(by)],sort(by))  ## 'by' is numeric so sort it
    if(!is.numeric(by)) x <- .cat2list(x, by)               ## 'by' is categorical
  }                           ## 'by' presumes univariate x input
  if(is.list(x)){                
    datas <- x                 
    if(length(names) == 0)     
      names <- names(x)      
  }else{                         
    datas <- list(x)
  }                              
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  medupper <- vector(mode = "numeric", length = n)
  medlower <- vector(mode = "numeric", length = n)
  hubermu <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.null(h))) 
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- (datas[[i]])
    if (is.factor(datas[[i]])) {
      data.min<-min(x[,sapply(x,is.numeric)],na.rm=TRUE)           
      data<-data.min
      names[i]=paste("factor :\n",names[i])
    } else {
      if (min(data,na.rm=TRUE)==Inf) {
        data.min<-min(x[,sapply(x,is.numeric)],na.rm=TRUE)           
        data<-data.min
        names[i]=paste("no data :\n",names[i])	
      }
    }        
    data.min <- min(data,na.rm=TRUE)   			
    data.max <- max(data,na.rm=TRUE)			
    q1[i] <- quantile(data, 0.25,na.rm=TRUE)	
    q3[i] <- quantile(data, 0.75,na.rm=TRUE)	
    med[i] <- median(data,na.rm=TRUE)
    medlower[i] <- .ci.median(data)$ci[2]
    medupper[i] <- .ci.median(data)$ci[3]
    hubermu[i] <- .huber.mu(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), args))
    datae<-na.omit(datas[[i]])
    if (length(datae)>0) {
      hscale <- 0.4/max(smout$estimate) * wex
      base[[i]] <- smout$eval.points
      height[[i]] <- smout$estimate * hscale
      t <- range(base[[i]])
      baserange[1] <- min(baserange[1], t[1])
      baserange[2] <- max(baserange[2], t[2])
    }
  } 
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5)
    else range(at) + min(diff(at))/2 * c(-1, 1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) 
    label <- 1:n
  
  else {
    label <- names(xx)
  }
  boxwidth <- 0.05 * wex
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, labels = label)
      title(main,xlab=xlab,ylab=ylab)
    }
    box()
    for (i in 1:n) {
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
              c(base[[i]], rev(base[[i]])), col = col[i], border = border,    
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
              lty = lty)
        rect(at[i] - boxwidth/2, q1[i], at[i] + boxwidth/2, 
             q3[i], col = rectCol)
        points(at[i], med[i], pch = pchMed, col = colMed)
      }
      if (ci.median_mu) {
        points(at[i], medupper[i],pch=6, col=colMed)
        points(at[i], medlower[i],pch=2, col=colMed)
        points(at[i], hubermu[i], pch=12, col=colMed)
      }
      if (connect) {													
        s <- seq(length(datas))											
        s <- s[-length(s)]												
        segments(at[s], med[s], at[s+1], med[s+1], col= connect.col, lty=connect.lty, lwd=connect.lwd)
      }


    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, labels = label)
    }
    box()
    
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], rev(at[i] + height[[i]])),
      , col = col[i], border = border,  lty = lty, lwd = lwd)
      if (drawRect) {
        lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, lty = lty)
        rect(q1[i], at[i] - boxwidth/2, q3[i], at[i] + boxwidth/2, col = rectCol)
        points(med[i], at[i], pch = pchMed, col = colMed)
      }
      if(connect)														
        warning('connect not allowed for horizontal plots')
    }
  }
  invisible(list(upper = upper, lower = lower, median = med,  q1 = q1, q3 = q3))
  if (stats) .vio.stats(xx,by) 
}


