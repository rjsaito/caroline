## table to dataframe (with names!)
tab2df <- function(x,...){

  is.tabrix <- class(x)[1] %in% c('table','matrix')
  row.nms <- rownames(x)
  
  if(NCOL(x)>1){
    clm.nms <- colnames(x)
    if(is.tabrix)
      x <- lapply(clm.nms, function(i) x[,i]) 
  }else{                                      
    if(is.tabrix){ # single column but matrix output
      clm.nms <- row.nms  # assume we want a 2 clmn dataframe
      row.nms <- NULL
    }else{   # vectors
      clm.nms <- NULL
      if(!is.null(names(x))) #named vector
        row.nms <- names(x) 
      else    # unnamed vector
        row.nms <- 1:length(x)
    }
  }
  
  if(!is.null(clm.nms)){
    x <- as.list(x)
    names(x) <- clm.nms
  }
  df <- data.frame(x,...)
  rownames(df) <- row.nms
  return(df)
  
}




if(FALSE){

  x <- data.frame(a=runif(10),b=runif(10), z=rep(letters[1:5],2))
  as.data.frame(x)
  tab2df(x)
  x <- nv(rnorm(10), letters[1:10])
    as.data.frame(x)
  tab2df(x)
  x <- nv(rnorm(2), c('x.b','y.b'))
    as.data.frame(x)
  tab2df(x)
  x <- nv(rnorm(2), c('b.x','b.y'))
    as.data.frame(x)
  tab2df(x)
  e <- data.frame(a=runif(10),b=runif(10), z=rep(letters[13:17],2))
  x <- as.table(sapply(c('a','b'),function(cc) by(e[,'a'],list(e$z), sum)))
    as.data.frame(x)
  tab2df(x)
  x <- as.table(by(1:10, list(a=rep(1:5,2),b=rep(1:2,5)), sum))
    as.data.frame(x)
  tab2df(x)
  x <- as.table(nv(c(54,34), c('a','b')))
    as.data.frame(x)
  tab2df(x)
}



## turns a named list into objects in the global environment 
delist <- function(L){
  
  names <- names(L)
  
  if(is.null(names))
    stop('list elements must have names')
  
  if(any(nchar(names)==0))
    stop('all list element names must have non zero length')
  
  for(name in names)
    assign(name, L[[name]], envir=.GlobalEnv)

}



nv <- function(x, name){

  if(class(x)=='data.frame'){
    v <- x[,name]
    names(v) <- rownames(x)
  }else{
    v <- as.vector(x)
    names(v) <- name
  }
  v
}
