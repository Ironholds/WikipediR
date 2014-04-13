\name{WikipediR-package}
\alias{WikipediR-package}
\alias{WikipediR}
\docType{package}
\title{
WikipediR, the MediaWiki API wrapper
}
\description{
WikipediR is a wrapper for the MediaWiki API. It is primarily oriented around the Wikimedia MediaWiki instances, such as Wikipedia - while it could work with other MediaWiki instances, you'd probably find it painful.
}
\details{
\tabular{ll}{
Package: \tab WikipediR\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2014-04-12\cr
License: \tab MIT\cr
}
The package aims to include wrappers for most of the 'primary' API read functions in MediaWikiAfter constructing a connection object using wiki_con(), plug this into the function that corresponds to the API functionality you're looking for, whether that be diff generation (wiki_diff()), content retrieval (wiki_revision()) or user information (wiki_user()).
}
\author{
Oliver Keyes <okeyes@wikimedia.org>
}
