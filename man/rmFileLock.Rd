\name{rmFileLock}
\alias{rmFileLock}
\title{Remove a Lock File}
\description{
A function to remove a lock file (a file with the same name + 'LOCK') to indicate the end of exclusive write access of that file by a process.  Used in conjunction with getLockFile to lock a file up for one processor at a time in a multiprocessor environment.
}
\usage{
rmFileLock(filepath, id)
}
\arguments{
\item{filepath}{The path to the file you wish to lock.}
\item{id}{unique process identifier}
}
\seealso{
\code{\link{getFileLock}}
}


