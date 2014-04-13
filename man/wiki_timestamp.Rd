\name{wiki_timestamp}
\alias{wiki_timestamp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Timestamp conversion function}
\description{
\code{\link{wiki_timestamp}} converts MediaWiki timestamps to POSIX-compliant timestamps, and vice-versa
}
\usage{
wiki_timestamp(timestamp, to_wiki = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timestamp}{
The timestamp(s) you wish to convert, provided as a vector. If the intent is to convert them into MediaWiki timestamps, they must be POSIX-compliant.
}
  \item{to_wiki}{
Whether the intent is to convert them from POSIX to MediaWiki }
}

\examples{

#MediaWiki to POSIX
wiki_timestamp(timestamp = 20140101010101, to_wiki = FALSE)
[1] "2014-01-01 01:01:01"

#POSIX to MediaWiki
wiki_timestamp(timestamp = "2014-01-01 01:01:01", to_wiki = TRUE)
[1] "20140101010101"
}