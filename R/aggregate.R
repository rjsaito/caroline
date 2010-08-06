

.aggregateDFrow <- function(x, cols, funcs=rep('sum',length(cols))){
  if(length(cols)!= length(funcs))
    stop('cols length does not equal funcs length')
  dfl <- list()
  for(i in seq(along=cols))
    dfl[[cols[i]]] <- do.call(funcs[i], list(x[,cols[i]]))
  df <- as.data.frame(dfl);
  return(df)  
}


groupBy <- function(df, fact, cols=names(df), funcs=rep('sum',length(cols))){
  do.call(rbind.data.frame, by(df, fact, function(x) .aggregateDFrow(x, cols, funcs)))
}

regroupDF <- function(df, old, new, cols=names(df), funcs=rep('sum',length(cols)), combine=TRUE){
  
  if(length(old) != length(new))
    stop('old and new must be the same length')

  if(combine)
    groupings <- row.names(df)
  else
    groupings <- rep(NA, nrow(df))
  matches <- match(old, row.names(df))
  groupings[matches[!is.na(matches)]] <- new[!is.na(matches)]

  regroup <- groupBy(df, as.factor(groupings), cols, funcs)
  return(regroup)
}



sumSortedTable <- function(x, clmns){

  tab <- table(x[,clmns], exclude='ifany')
  rownames(tab)[is.na(rownames(tab))] <- 'NA'  #migrate to caroline?
  tab <- tab2df(tab)
  tab$sum <- apply(tab, 1, sum)
  tab[rev(order(tab$sum)),]
}



ledghead <- function(x, n=6, colors=TRUE, tabulate=FALSE){

  if(as.logical(tabulate))
    x <- sumSortedTable(x, tabulate)
  
  if(n > 3 & n < nrow(x)){
    
    o.rn <- rownames(x)    
    is.n <- sapply(x, is.numeric)
    others.summed <- apply(x[n:nrow(x), is.n],2,sum)    
    x <- rbind(head(x,n), c(others.summed, 'NA'[!is.n]))
    rownames(x) <- c(o.rn[1:n],'other')
    
  }

  if('color' %in% colnames(x)){
    warning('dataframe already has a color column. leaving it as is.')
    colors <- FALSE
  }
  
  if(all(colors != FALSE)){

    if(length(colors)>1){

      matches <- match(names(colors),rownames(x))
      if(all(is.na(matches)))
        stop('color vector names do not match the top n table row names')
      x$colors <- colors[matches]
      
      }else{
        
        x$color <- rainbow(nrow(x))
        x$color[rownames(x)=='NA'] <- 'gray'
      }
  }

  x
}
