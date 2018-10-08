#' Elsevier journal lists
#' 
#' https://www.elsevier.com/solutions/sciencedirect/content/journal-title-lists
#' 
#' active titles
#' https://www.elsevier.com/__data/promis_misc/sd-content/journals/jnlactive.csv
#'
library(tidyverse)
active_titles <- readr::read_csv("https://www.elsevier.com/__data/promis_misc/sd-content/journals/jnlactive.csv")
#' elsevier freedom collection
frdm <- readr::read_csv("https://www.elsevier.com/__data/promis_misc/sd-content/journals/freedomcoll.csv", skip = 2) %>%
  # only most current
  filter(FC2019 == "Included")
#' imprint
imprints <- readr::read_csv("https://www.elsevier.com/__data/promis_misc/sd-content/journals/jnlimprint.csv")
#' 1. add imprints to active title list
imprints %>%
  filter(Status == "Active") %>%
  select(Imprint, `Product ID`) %>%
  inner_join(., active_titles, by = "Product ID") -> elsevier_jns
# clean ISSNs
str_sub(elsevier_jns$ISSN, 5, 4) <- "-"
str_sub(frdm$ISSN, 5, 4) <- "-"

#' 2. add oa information, use from
#' https://www.elsevier.com/__data/promis_misc/j.custom97.pdf
#' 
apcs <- readr::read_csv("data-raw/tabula-j.custom97.csv")
elsevier_jns %>%
  left_join(apcs, by = "ISSN") %>%
  mutate(freedom_collection = ifelse(ISSN %in% frdm$ISSN, "Freedom Collection", "not included")) -> elsevier_jns
readr::write_csv(elsevier_jns, "data/elsevier_jns.csv")
