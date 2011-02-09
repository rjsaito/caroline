 

.aggregateDFrow <- function(x, clmns, funcs=rep('sum',length(clmns))){
  if(length(clmns)!= length(funcs))
    stop('clmns length does not equal funcs length')
  dfl <- list()
  for(i in seq(along=clmns))
    dfl[[clmns[i]]] <- do.call(funcs[i], list(x[,clmns[i]]))
  df <- as.data.frame(dfl);
  return(df)  
}


groupBy <- function(df, fact, clmns=names(df), funcs=rep('sum',length(clmns))){
  if(any(sapply(as.character(fact), nchar)> 255))
    stop("levels of fact must have no more than 256 characters")
  do.call(rbind.data.frame, by(df, fact, function(x) .aggregateDFrow(x, clmns, funcs)))
}

regroup <- function(df, old, new, clmns=names(df), funcs=rep('sum',length(clmns)), combine=TRUE){
  
  if(length(old) != length(new))
    stop('old and new must be the same length')

  if(combine)
    groupings <- row.names(df)
  else
    groupings <- rep(NA, nrow(df))
  matches <- match(old, row.names(df))
  groupings[matches[!is.na(matches)]] <- new[!is.na(matches)]

  regroup <- groupBy(df, as.factor(groupings), clmns, funcs)
  return(regroup)
}

  
addFactLevs <- function(x, new.levs=NA){

  if(class(x) != 'data.frame')
    stop('x must be a data.frame')

  isfacts <- sapply(x, is.factor)
  for(clmn in names(x)[isfacts])
    x[,clmn] <- factor(x[,clmn], levels=c(levels(x[,clmn]), new.levs))
  x
}



sstable <- function(x, idx.clmns, ct.clmns=NULL, na.label='NA'){#,exclude=exclude, ...){

  if(class(x) != 'data.frame')
    stop('x must be a data.frame')

  if(any(sapply(x[,idx.clmns], class)!= 'factor')){
    warning('some or all of your index columns are not factors. coercing them...')
    for(idx in idx.clmns)
      x[,idx] <- as.factor(x[,idx])
  }
    
  
  # should I make this run more like the other 'table' functions?
  if(is.null(ct.clmns)){
    ## plain old table method
    tab <- table(x[,idx.clmns], exclude='ifany')
    rownames(tab)[is.na(rownames(tab))] <- na.label
    
  }else{
    ## this whole section need reworking into a much simpler agg2tab function perhaps    
    ncc <- length(ct.clmns)
    nic <- length(idx.clmns)
  
    if(ncc > 2 | nic > 2)
      stop(" can't have more than 2 index or count columns")
    if(ncc > 1 & nic > 1)
      stop('cannot have both more than one index and count columns')
       
    y <- tab2df(x[,idx.clmns])
    y <- addFactLevs(y, 'NA')
    y[is.na(y)] <- na.label
      
    if(nic == 2 & ncc == 1){
      tmp <- as.table( by(x[,ct.clmns], as.list(y), sum))
    }else{
      tmp <- as.table(sapply(ct.clmns, function(cc)
                             by(x[,cc], as.list(y), sum)
                             )
                      )
    }

    tab <- tab2df(tmp)
    tab [is.na(tab)] <- 0

    if(dim(tmp)[1]==2 & NROW(tab)==1 & ncc==2){
      colnames(tab) <- ct.clmns
      rownames(tab) <- unique(y)
    }
    
  }

  if(NCOL(tab) > 1){
    tab <- tab2df(tab)    
    tab$sum <- apply(tab, 1, sum)

    tab <- subset(tab, sum > 0)
    
    return(tab[rev(order(tab$sum)),])
  }else{
    return(tab[rev(order(tab[,1])),])
  }
}


leghead <- function(x, n=7, tabulate=FALSE, colors=TRUE, na.name='NA',na.col='white', other.col='gray'){

  if(as.logical(tabulate))
    x <- sstable(x, tabulate)
  
  if(n > 3 & n < nrow(x)){
    
    o.rn <- rownames(x)    
    is.n <- sapply(x, is.numeric)
    n <- n + sum(!is.n)  # so that we can utilize (an) extra color(s)
    others.summed <- apply(x[(n+1):nrow(x), is.n],2,sum)    
    x <- rbind(head(x,n), c(others.summed, 'NA'[!is.n]))
    rownames(x) <- c(o.rn[1:n],'other')
    
  }

  if('color' %in% colnames(x)){
    warn.col.clmn <- 'dataframe already has a color column:'
    isna <- rownames(x)== 'NA'
    if(sum(isna) > 0){
      warn.col.clmn <- paste(warn.col.clmn , 'changing NA levels to', na.col, '.')
      x$color[isna] <- na.col
    }else{
      warn.col.clmn <- paste(warn.col.clmn , 'leaving it as is.')
    }
    warning(warn.col.clmn)
    colors <- FALSE
  }
  
  if(all(colors != FALSE)){
    
    if(length(colors)>1){

      matches <- match(names(colors),rownames(x))
      if(all(is.na(matches)))
        stop('color vector names do not match the top n table row names')
      x$color <- colors[matches]
      
    }else{
      isna <- rownames(x)=='NA'
      x$color[!isna] <- rainbow(nrow(x) - sum(isna))
      x$color[ isna] <- na.col
    }
  }
  if('color' %in% colnames(x))
    x$color[rownames(x)=='other'] <- other.col  

  if(na.name != 'NA')
    rownames(x)[match('NA', rownames(x))] <- na.name

  x
}




.vle2df <- function(vl,i){
  ## vector list element to dataframe (preserves list element names)

  if(class(vl[[i]])=='data.frame'){
    df <- vl[[i]]
    names(df) <- paste(names(df),'.',i,sep='')    
  }else{
    df <- as.data.frame(vl[[i]])
    #names(df) <- i
    colnames(df) <- i
  }
  df$rownames <- rownames(df)  #necessary because the built in b='row.names' merge is really slow (if not completely broken)
  df
}


nerge <- function(l, ...){
  ## named data.frame or vector merge

  if(!all(sapply(l, function(k) class(k) == 'data.frame' | is.vector(k))))
     stop('list elements must be either of class data.frame or of type vector')
  if(length(l) < 2)
    stop('list l must have at least 2 elements')
  if(is.null(names(l)))
    stop('list l elements must be named')
  if(!all(sapply(l, function(j) !is.null(rownames(j)) | !is.null(names(j)))))
    stop('all list elements must have names')
    
  df <- .vle2df(l,names(l)[1])
  for(e in 2:length(l)){
    df <- merge(df, .vle2df(l, names(l)[e]), by='rownames', all=T)
    rownames(df) <- df$rownames
  }
  df$rownames <- NULL
  df
}


