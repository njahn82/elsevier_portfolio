#' calculate proportions
library(tidyverse)
active_titles <- readr::read_csv("data/elsevier_jns.csv")
yrl_volume <- readr::read_csv("data/jn_facets_y_published.csv") %>%
  select(1, year = 2, articles = 3) %>%
  left_join(active_titles, by = c("issn" = "ISSN"))
#' add hybrid
hybrid_oa <- readr::read_csv("data/hybrid_oa.csv") %>%
  mutate_at(vars(issued), funs(lubridate::parse_date_time(., c('y', 'ymd', 'ym')))) %>%
  mutate_at(vars(issued), funs(lubridate::year(.))) %>%
  count(issn, issued) %>%
  right_join(yrl_volume, by = c("issn" = "issn", "issued" = "year")) 
oa_df <- hybrid_oa %>%
  mutate(oa_articles = ifelse(`OA model` == "Open Access", articles, NA))
oa_df %>% 
  group_by(issued, Imprint) %>% 
  summarise(oa_full = sum(oa_articles, na.rm = TRUE), oa_hybrid = sum(n, na.rm = TRUE), all = sum(articles, na.rm =TRUE)) %>% 
  mutate(closed = all - (oa_full + oa_hybrid)) %>% 
  select(-all) %>% 
  gather(3:5, key = "oa type", value = "articles") -> oa_counts
readr::write_csv(oa_counts, "data/oa_counts.csv")
