#' Elsevier / Crossref check
library(tidyverse)
library(rcrossref)
library(jsonlite)
elsevier_jns <- readr::read_csv("data/elsevier_jns.csv")

jn_facets <- purrr::map(elsevier_jns$ISSN, .f = purrr::safely(function(x) {
  tmp <- rcrossref::cr_works(
    filter = c(
      issn = x,
      from_pub_date = "2015-01-01",
      until_pub_date = "2018-12-31",
      type = "journal-article"
    ),
    facet = TRUE,
    # less api traffic
    select = "DOI"
  )
  #' Parse the relevant information
  #' - `issn` - issns  found in open apc data set
  #' - `year_published` - published volume per year (Earliest year of publication)
  #' - `license_refs` - facet counts for license URIs of work
  #' - `journal_title` - Crossref journal title (in case of journal name change, we use the most frequent name)
  #' - `publisher` - Crossref publisher (in case of publisher name change, we use the most frequent name)
  #'
  #' To Do: switch to current potential
  if (!is.null(tmp)) {
    tibble::tibble(
      issn = x,
      year_published = list(tmp$facets$published),
      license_refs = list(tmp$facets$license)
      #    journal_title = tt$facets$`container-title`$.id[1],
      #    publisher = tt$facets$publisher$.id[1]
    )
  } else {
    NULL
  }
}))
jn_facets %>%
  map_df("result") %>%
  unnest(year_published) %>%
  write_csv("data/jn_facets_y_published.csv")
jn_facets %>%
  map_df("result") %>%
  unnest(license_refs) -> license_all
write_csv(license_all, "data/license_all.csv")
license_all %>%
  mutate(license_ref = tolower(.id)) %>%
  select(-.id) %>%
  mutate(hybrid_license = ifelse(grepl(
    "creativecommons|open-access",
    license_ref
  ), TRUE, FALSE)) %>%
  filter(hybrid_license == TRUE) -> license_sub
#' remove full oa journals
elsevier_jns %>%
  filter(!`OA model` == "Open Acess") -> elsevier_nonoa_jns
license_sub %>%
  filter(issn %in% elsevier_nonoa_jns$ISSN) %>%
  filter(hybrid_license == TRUE) -> oa_license_df

cr_license <- purrr::map2(oa_license_df$license_ref, oa_license_df$issn,
                          .f = purrr::safely(function(x, y) {
                            u <- x
                            issn <- y
                            tmp <- rcrossref::cr_works(filter = c(issn = issn, 
                                                                  license.url = u, 
                                                                  license.delay = 0,
                                                                  type = "journal-article",
                                                                  from_pub_date = "2015-01-01", 
                                                                  until_pub_date = "2018-12-31"),
                                                       cursor = "*", cursor_max = 5000L, 
                                                       limit = 1000L) 
                            tibble::tibble(
                              issn =  issn,
                              # year_published = list(tmp$facets$published),
                              license = u,
                              md = list(tmp$data)
                            )
                          }))
#' into one data frame!
#' dump it
cr_license_df <- cr_license %>% 
  purrr::map_df("result") 
cr_license_df %>% 
  unnest(md) %>% 
  select(1:27) %>% 
  write_csv("data/hybrid_oa.csv")
