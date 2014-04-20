\name{wiki_con}
\alias{wiki_con}
\title{
Connector-generating function for WikipediR
}
\description{
wiki_con generates a connector object necessary to use the other API calls in WikipediR
}
\usage{
wiki_con(language, project = c("wikipedia", "commons", "species",
                               "wikisource", "wikiquote", "wikinews",
                               "wikibooks", "wikiversity", "wikivoyage"))
}
\arguments{
  \item{language}{
The two or three-letter project language code - for example, en.wikiquote.org has the language code "en"
}
  \item{project}{
  The project type - currently accepted types are wikipedia, species, commons, wikisource, wikiquote, wikinews, wikibooks, wikiversity and wikivoyage.
  
}
}
\details{
wiki_con is designed to generate a connector object that can be passed into the API-wrapping functions to retrieve data. At the moment, it consists almost entirely of a pre-constructed URL; the intent is to have it allow for both OAuth and direct authentication given time, to take advantage of the higher API limits given by being authenticated.

The language code and project names provided by the user are checked against an object containing all 'legitimate' permutations of languages and project types - in other words, all Wikipedia wikis that are not private, closed, dead, or otherwise restricted. This list is stored as an .RData file, "wikis", which will be updated as the package is updated. To allow for the likelihood that the active projects change in quantity and variety over time, a function, RDataRebuild, is provided in this package to allow the end user to update the wiki listings whenever they want.

Wikidata support is likely to be provided through a different package entirely, because it has a different API structure.

}

\seealso{

\code{\link{RDataRebuild}}

}

\examples{

#Create con object
con <- wiki_con(language = "en", project = "wikisource")

}