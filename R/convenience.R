wikiget <- function(page_name, language='en', project='wikipedia', as_wikitext=TRUE, ...) page_content(page_name=page_name, language=language, project=project, as_wikitext=as_wikitext, ...)


#these should maybe be lists or env's so no () required
wikilist <- function() {
	invisible('https://meta.wikimedia.org/wiki/Complete_list_of_Wikimedia_projects')
	c('wikipedia','wikibooks','wikiquote','wiktionary','wikinews','wikiversity','wikisource','wikimedia','meta','mediawiki','wikimedia commons','wikivoyage','wikijunior','wikispecies','incubator','sep11wiki')
	}

#these should maybe be lists or env's so no () required
properties <- function(){
	invisible('https://www.mediawiki.org/wiki/API:Properties')
	c('pageID', 'namespace','title','language','length','protection','talkid','url','displaytitle')
}







#aliases
wiki.get <- wikiget
wiki_get <- wikiget


wiki.list <- wikilist
wiki_list <- wikilist
