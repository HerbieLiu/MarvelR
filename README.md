---
title: "MarvelR"
author: "He Herbie Liu"
output:
  html_document:
    keep_md: true
---

``

MarvelR is designed to make it quicker and easier to retrievel Marvel Comics information from Marvel API portal. The latest version of MarvelR is 0.1.0. The MarvelR package contains twelve functions: token_to_query(), search_comics(), search_characters(), search_series(), search_creators(), search_stories(), search_events(),get_comics(), get_characters(), get_series(), get_creators(), and get_events(). Marvel API provides users with information on six different types of information, comics, characters, series, creators and stories which can be retrieved easily from the MarvelR package functions.

## Installation

```r
#Install the newest version of MarvelR
'install.packages("MarvelR")'
```


## Usage

```r
token_to_query(token,secret_token,limit = 100)
get_comics(title)
get_characters(name)
get_events(name)
get_series(title)
get_creators(firstname,lastname)
search_characters(id, type)
search_comics(id, type)
search_creators(id, type)
search_events(id, type)
search_series(id, type)
search_stories(id, type)
```

## Details
*token_query()* transits the Marvels Tokens gained from Marvel website to the required format of the API query. All the *"get"* functions search each type by keywods of titles or names. All the *"search"* functions can either return random selected Marvel information of selected type or can be specified to unique ID numbers and a secondary type. 

The specific useage of each function can be found under **Reference**. 

For more information on MarvelR, please go to **Articles**. 
