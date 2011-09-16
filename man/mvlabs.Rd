\name{mvlabs}
\alias{mvlabs}
\title{Move Text Labels Interactively}
\description{
There is no easy way to move point labels around interactively on an a plot in R.  This function allows a point and click way to select (using identify) and move (using locator) points by modifying the underlying dataframe.
}
\usage{
mvlabs(df, n=nrow(df), x='x', y='y', l='lab', cols=colors()[grep("dark",colors())], ...)
}
\arguments{
\item{df}{A dataframe with x and y coordinates and text labels}
\item{n}{the number of points you wish to move}
\item{x}{the column name of the x axis coordinates}
\item{y}{the column name of the y axis coordinates}
\item{l}{the column name of the point labels}
\item{cols}{the color vector to iterate through while assigning new positions.}
\item{...}{other paramters passed on to text}
}
\seealso{
\link{locator},\link{identify},\link{labsegs}
}
\value{a series of violin plots}
\examples{
  x <- rnorm(20); y <- rnorm(20)
  df <- data.frame(x,y, lab=as.character(letters[1:20]))
  plot(df$x, df$y, pch=''); text(df$x, df$y, df$lab)
  ## df <- mvlabs(df, 'x','y','lab', n=3)  
  plot(df$x, df$y, pch=''); text(df$x, df$y, df$lab)
}

