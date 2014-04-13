\name{RDataRebuild}
\alias{RDataRebuild}
\title{
Rebuild and update the .RData file used in \code{\link{wiki_con}}}
\description{
RDataRebuild is designed to update wikis.RData, which is an .RData file included in \code{\link{WikipediR}} containing the list of 'active' Wikimedia wikis - those that are not dead, closed or private. This is used in \code{\link{wiki_con}} to validate the data a user provides for constructing a MediaWiki connector object.

While the file will be updated every time WikipediR is, it could still get out of date in the interim. RDataRebuild reconstructs it from publicly-available data.

}
\usage{
RDataRebuild()
}

\seealso{
  \code{\link{wiki_con}}
}