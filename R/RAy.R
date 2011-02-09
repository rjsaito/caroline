.avgLog <- function(a,b){
  return((log(a,2)+log(b,2))/2)
}


raPlot <- function(a, b, uniques=5, normalize=FALSE,  
                   mm=0, alpha = 0.01, jitter=FALSE, jit.wgts=NULL,
                   rex=1, flat=TRUE, tail=.5, arms=.5, spine=1, border=NULL, ...){
  
  ## find the library-unique genes
  a0 <- a==0
  b0 <- b==0
  
  ## add an epsilon factor to include uniques in the plot
  if(as.logical(uniques)){
    if(uniques > 5 | uniques <= 0)
      stop("'uniques' width  must either be FALSE or or between 1 and 5")
    u.epsilon <- function(n) runif(n,min=(.5 - .1 * uniques/5), max=.5)
    a[a0] <- a[a0] + u.epsilon(sum(a0))
    b[b0] <- b[b0] + u.epsilon(sum(b0))
  }

  ## spread out the overploted points (randomly for now, by weights eventually)
  if(as.numeric(jitter)){ 
    if(jitter > .5 | jitter < 0)
      warning('jitter amount (in this context) is best set between 0 and .5') 
    if(!is.null(jit.wgts) & class(jit.wgts) == 'data.frame'){
      a[!a0] <- wjitter(a[!a0], jit.wgts[!a0,1], amount=jitter)
      b[!b0] <- wjitter(b[!b0], jit.wgts[!b0,2], amount=jitter)
    }else{
      ## this fleshes out the visibility of the low count points
      a[!a0] <- jitter(a[!a0], amount=jitter) 
      b[!b0] <- jitter(b[!b0], amount=jitter)
    }
  }
  
  ## remove uniques if specified
  if(!as.logical(uniques)){
    a <- a[!(a0 | b0)]
    b <- b[!(a0 | b0)]
  }

  ## normalize by library sums
  a.norm <- a/sum(a) #a.sum
  b.norm <- b/sum(b) #b.sum

  ## calculate (magnitude) fold change ratio and amplitude
  R.norm <- log(a.norm/b.norm, 2)
  A.norm <- .avgLog(a.norm,b.norm)    
  R <- log(a/b,2)
  A <- .avgLog(a,b)

  ## handle the case where things are normalized
  if(normalize){
    ## point cex sizes 
    sizes <- (A + 1) * abs(R) # +1 is for the uniques

    ## normalization shift factor
    A.shift <- A.norm[1] - A[1]
    R.shift <- R.norm[1] - R[1] 
    
    R <- R.norm
    A <- A.norm
    
  }else{      0
    sizes <- (A + 1) * abs(R.norm)  # +1 is for the uniques
    A.shift <- R.shift <- 0  
  }
  
  sizes <- sizes/max(sizes) * 3 + .3
  if(flat)
    sizes <- 1  

  ## create the actual plot
  plot(A, R, cex= sizes * rex, ...)

  if(!is.null(border))
    if(length(border) == length(A) | length(border)==1)
      points(A, R, cex= sizes * rex, col=border)

  ## add in the RAy skeleton
  if(as.logical(arms))
    raAddArms(lwd=arms, lty=2, col='gray', A.shift=A.shift, R.shift=R.shift)  
  
  if(as.logical(tail) & !is.null(alpha))
    raAddSigLines(n=length(a), mm=mm, lwd=tail, A.shift=A.shift, col='gray')
  
  if(as.logical(spine))
    segments(min(A)+2, mm, max(A), lwd=spine, col='gray')

  invisible(list(R=R,A=A, sizes=sizes))
}



## RAy SKELETON HELPER FUNCTIONS

raAddArms <- function(epsilon=.55, start=1, end=6, A.shift=0, R.shift=0, ...){
    a <- epsilon
    b <- 2^(start:end)
    lines(.avgLog(a,b)+A.shift, log(a/b,2)+R.shift, ...)
    lines(.avgLog(a,b)+A.shift,-log(a/b,2)+R.shift, ...)
}




raAddSigLines <- function(n, end=20, alpha=1e-3, mm=0, A.shift=0, plot=FALSE, ...){

  alpha.crctd <- alpha/(n/2) # simple bonferroni multiple testing correction

  ns <- 2^seq(0,20,by=.2)  
    
  for(k in c(1,0)){ 
    sig <- abs(k - alpha.crctd)
    a <- qnorm(p= sig, mean = 0.5 * ns, sd = sqrt( ns * 0.5))
    b <- ns - a
    
    non.zero <- a >= 0 & b >= 0
    a <- a[non.zero]
    b <- b[non.zero]
    
    x <- .avgLog(a, b) + A.shift
    y <- log(a/b, 2) + mm

    if(plot)
      lines(x, y, ...)
    invisible(data.frame(x,y))
  }
}


raAddAxLabs <- function(conditions=nv(c('a','b'),c('ref','obs')), normalize=T, add=TRUE, line=2){

  n <- as.numeric(normalize) + 1
  l.p <- c('','(')[n]
  r.p <- c('',')')[n]
  ltr1 <- nv(substr(conditions,1,1), names(conditions))
  denom <- cbind(rep('',2),paste('/N',ltr1,')',sep=''))[,n]; names(denom) <- names(ltr1)
  
  olab <- paste(l.p,conditions['obs'],denom['obs'], sep='')
  rlab <- paste(l.p,conditions['ref'],denom['ref'], sep='')
  r <- paste( c('','normalized')[n],'fold change = log2(',olab, '/',rlab ,')')
  a <- paste( c('','normalized')[n],' amplitude = ',l.p,'(log2(',olab, ') + log2(',rlab ,')',r.p,')/2', sep='')

  if(add){
    mtext(r, side=2, line=line)
    mtext(a, side=1, line=line)
  }

  invisible(list(r=r,a=a))
}

#expression(paste(log[2], might want to add to above



wjitter <- function(x, w, amount=.43){
  wj <- function(w, amount)
    (match(1:length(w), order(w)) / length(w)) *  2 * amount - amount
  df <- data.frame(r=1:length(x), x=x,w=w)
  out <- do.call(rbind.data.frame, (by(df, round(x), function(z) cbind.data.frame(z, j=z$x + wj(z$w, amount=amount)))))
  out[order(out$r),'j']
}
