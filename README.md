---
title: "MarvelR"
author: "He Herbie Liu"
output:
  html_document:
    keep_md: true
---

``

Marvelr is Designed to make it quicker and easier to retrievel Marvel Comics information from Marvel API prtal. The latest version of MarvelR is 0.1.0.

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
