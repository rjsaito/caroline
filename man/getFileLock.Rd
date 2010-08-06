\name{getFileLock}
\alias{getFileLock}
\title{Check and Set a Lock File}
\description{
A function to set a lock file (a file with the same name + 'LOCK') to indicate exclusive write access of that file by a process.  Used in conjunction with rmLockFile to lock a file up for one processor at a time in a multiprocessor environment.
}
\usage{
getFileLock(filepath, wait.time=.01, id)
}
\arguments{
\item{filepath}{The path to the file you wish to lock.}
\item{wait.time}{The waiting time (in seconds) between checks for an existing processor's exclusive write lock on 'filepath'.}
\item{id}{unique process identifier}
}
\seealso{
\code{\link{rmFileLock}}
}



