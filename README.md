WikipediR
=========

An R API wrapper for MediaWiki, optimised for the Wikimedia Foundation MediaWiki instances, such as Wikipedia.

__Author:__ Oliver Keyes<br>
__License:__ [MIT](http://opensource.org/licenses/MIT)<br>
__Status:__ In development

Description
======
_WikipediR_ is a wrapper around the MediaWiki API, optimised for the Wikimedia Foundation's production sites, such as Wikipedia. It is written in and for R, a statistical environment and associated programming language in heavy use by HCI researchers that, somehow, escaped having an API wrapper until now.

It is currently in development, although the functionality present (namely diff generation through wiki\_diff, user information retrieval through wiki\_userinfo and page metadata retrieval through wiki\_pagecats) works.

Things you should use WikipediR for
======
* Anything you'd normally use the default MediaWiki API to read from Wikipedia or its sister projects

Things you should not use WikipediR for
======
* Querying Wikidata. Wikidata's API structure is very different from that of MediaWiki in some ways; it will probably be handled in a different package.
* Querying non-Wikimedia MediaWiki instances. This is totally doable, _iff_ you want to manually modify the output of wiki_con, but will become increasingly more difficult as OAuth integration and other things are introduced. This is a Wikimedia wrapper first and foremost.
* Teaching a robot how to love.


Dependencies
======
* R. Doy.
* [httr] (http://cran.r-project.org/web/packages/httr/index.html) and its dependencies;
* [jsonlite] (http://cran.r-project.org/web/packages/jsonlite/index.html) and its dependencies;


To-Do
======
* Introduce a full suite of unit tests;
* Look for a more elegant way of handling the parameter names in the returned JSON block;
* Introduce authentication, via both direct MediaWiki authentication and OAuth

Thanks and misc
======
Thanks to, in no particular order:
* [Adam](https://github.com/Protonk), for being the person who got me through my baby steps in R, and;
* [Hadley](https://github.com/hadley), both for httr and his wonderful API wrapper tutorial, which I have liberally stolen from;
* [Toby](https://meta.wikimedia.org/wiki/User:TNegrin_%28WMF%29), for not firing me for working on this.

An etymological note; while a lot of people reflexively twitch at people using CamelCase in package names, the opportunity here was too good to resist. "WikipediR" is a reference both to the R convention of having package names that consist of [thing the package does] + [reference to R], and the link format in UseModWiki, the predecessor to MediaWiki, that used CamelcaseinG to indicate what the author intended to be a link (this is also where the "WikipediA" logo comes from).