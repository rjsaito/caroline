
 .vio.stats <- function (x,by)       # see http://legacy.ncsu.edu/ST370/distance/rlab/
{
    if (!missing(by)) {
        x <- .cat2list(c(x), by)
    }
    if (!is.list(x) & !is.matrix(x))
        x <- matrix(x, ncol = 1)
    if (is.list(x)) {
        nc <- length(names(x))
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
        dimnames(out) <- list(.distDescription(), dimnames(x)[[2]])
        for (k in (1:nc)) {
           if (!is.factor(x[[k]])) {
            out[, k] <- .distDescription(x[, k])  #matrix
           }
        }
        return(noquote(out))
    }
}

.distDescription <-function (x)
{
  lab <- c("N", "mean", "SD", "robust min", "min", "Q1", "median",
      "Q3", "max", "robust max",
      "skew", "kurtosys","missing values","unique values","Shapiro p")
  if (missing(x)) {
      return(lab)
  }
  temp <- rep(0, length(lab))
  xt <- x[!is.na(x)]
  n <- length(xt)
  if (!is.numeric(xt) || all(is.na(x))) {
      return(c(n, NA, NA, rep(NA, 7), length(x) - length(xt),NA))
  }
  else {
  if (n < 4) {
      xt <- sort(xt)
      if (n == 1) {
	  return(sprintf("%10.4f",c(n, xt[1], NA, rep(c(NA,xt[1]), 3), rep(NA,3), length(x) -
	      length(xt),length(unique(xt)),NA)))
      }
      if (n == 2) {
	  return(sprintf("%10.4f",c(n, mean(xt), sqrt(var(xt)), .rrange(xt)[1], c(xt[1], xt[1],
	      mean(xt), xt[2], xt[2]), .rrange(xt)[2], NA, NA, length(x) - length(xt),length(unique(xt)),NA)))
      }
      if (n == 3) {
	  return(sprintf("%10.4f",c(n, mean(xt), sqrt(var(xt)), .rrange(xt)[1], c(xt[1], xt[1],
	      xt[2], xt[3], xt[3]), .rrange(xt)[2], NA, NA, length(x) - length(xt),length(unique(xt)),NA)))
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
	  quantile(xt, c(0.25, 0.5, 0.75)), max(xt), .rrange(xt)[2], .skewed(xt), .kurtosys(xt),
	  length(x) - length(xt),length(unique(xt)),shapp$p)))     #
  }
 }
}

.skewed <-function (x)
{
  # from Tom Fletcher, University of Missouri - St. Louis  : http://www.umsl.edu/~fletchert/quant/DataScreen.txt
  n <- length (x[!(is.na(x))])
  sd <- sqrt(var(x,na.rm=T))
  m <- mean(x,na.rm=T)
  (n/((n-1)*(n-2)))*sum(((x-m)/sd)^3, na.rm=T)
}

.kurtosys <-function (x)
{
  # from Tom Fletcher
  n <- length (x[!(is.na(x))])
  sd <- sqrt(var(x,na.rm=T))
  m <- mean(x,na.rm=T)
  (((n*(n+1))/((n-1)*(n-2)*(n-3)))*(sum(((x-m)/sd)^4, na.rm=T)))-((3*(n-1)^2)/((n-2)*(n-3)))
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

## cat.to.list stolen verbatim from the 'fields' package from University Center for Atmospheric Research,
## required in the new violins function
.cat2list <- function (x, a) 
{
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


## an improved violin plot by 
## Dave McArthur <dmca@ucla.edu>
## that takes a list or dataframe as input
## lightly adapted 2/19/10 from package 'caroline'  
## David M. Schruth <dschruth at u.washington.edu>  2/12/10
## who migrated the function out of the vioplot library

violins <- function (x, by, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
    horizontal = FALSE, col = "transparent", border = "black", lty = 1, 
    lwd = 1, rectCol = "grey50", colMed = "grey80", pchMed = 19, 
    at, add = FALSE, wex = 1, drawRect = TRUE, main = "", connect=TRUE,
    xlab="",ylab="", stats=FALSE) 
{
   #library(vioplot)
   require(sm)										  ## call required library if not already loaded
   if(is.data.frame(x)) x<-as.list.data.frame(x)  ## convert dataframe to list if needed
    if (!missing(by)) {                           ## handle 'by' variable
    if(is.numeric(by)) x<-.cat2list(x[order(by)],sort(by))  ## 'by' is numeric so sort it
    if(!is.numeric(by)) x <- .cat2list(x, by)               ## 'by' is categorical
                      }                           ## 'by' presumes univariate x input
    if(is.list(x)){                #dschruth added
    	datas <- x                 #dschruth added
    	if(length(names) == 0)     #dschruth added
    		names <- names(x)      #dschruth added
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
        data <- (datas[[i]])
        data.min <- min(data,na.rm=TRUE)   			##
        data.max <- max(data,na.rm=TRUE)			##
        q1[i] <- quantile(data, 0.25,na.rm=TRUE)	##
        q3[i] <- quantile(data, 0.75,na.rm=TRUE)	##
        med[i] <- median(data,na.rm=TRUE)
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
            title(main,xlab=xlab,ylab=ylab)
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
            if(connect) {														## connect medians
    			s <- seq(length(datas))											##
    			s <- s[-length(s)]												##	
    			segments(at[s], med[s], at[s+1], med[s+1], col= 'lightblue')	##
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
            if (connect) {														## connect medians
                s <- seq(length(datas))											##
			    s <- s[-length(s)]												##
    			segments(at[s], med[s], at[s+1], med[s+1], col= 'lightgreen')	##
    			         }
        }
    }
    invisible(list(upper = upper, lower = lower, median = med, 
        q1 = q1, q3 = q3))
    if (stats) .vio.stats(x,by) 
}
