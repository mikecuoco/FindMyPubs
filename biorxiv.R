require(rbiorxiv)
require(RefManageR)
require(purrr)
require(dplyr)
require(lubridate)
require(glue)
require(here)

#' Parse results from biorxiv search
parse_biorxiv <- function(content){
  # remove withdrawn content
  content = content %>%
    filter(type != "withdrawn") %>%
    distinct()

  # prase authors
  authorlist = gsub("\\. ","",content$authors) %>%
    str_replace("\\.","") %>%
    strsplit("; ") %>%
    pluck(nrow(content))

  authorlist = map_chr(authorlist, function(.x){
    split = strsplit(.x,", ") %>% pluck(1)
    author = paste(split[2],split[1]) %>% trimws()
  })

  content$authors = paste(authorlist, collapse = ", ")
  return(content)
}

#' Searh BioRxiv for a given term, cache the results, and return the results as a dataframe
#' @param term The term to search for
search_biorxiv <- function(term) {

  # load cache
  message("Reading cached DOIs")
  cache_file = here::here(".cache/biorxiv_dois.txt")
  dois = read_lines(cache_file)

  # load last run
  message("Detecting last run DOIs")
  date_file = here::here(".cache/last_run.txt")
  last_run = read_lines(date_file) %>% lubridate::as_date() %>% na.omit() %>% max()

  # look for specified DOIs
  if (!is.null(dois)){
    doi_fetch = purrr::map_df(dois, function(.x) {
      prints = rbiorxiv::biorxiv_content(doi = .x, format = "df")
      return(as.list(prints))
    })
  }

  # try to find biorxiv posts, catch errors
  term_fetch = try(
    rbiorxiv::biorxiv_content(from = last_run, to = lubridate::today(), limit = "*", format = "df") %>%
      filter(grepl(term, authors))
  )

  if (is.null(nrow(term_fetch))) { # if none found
    term_fetch = doi_fetch[0,]
  } else if(nrow(term_fetch) == 0){ # if none found
    term_fetch = doi_fetch[0,]
  } else {     # record date
    message("Recording run date")
    write_lines(x = today(), file = date_file, append = T)
  }

  # compile doi and term fetches
  fetch = bind_rows(doi_fetch, term_fetch) %>%
    filter(type != "withdrawn") %>%
    distinct() %>%
    group_by(doi) %>%
    filter(version == max(version)) %>%
    mutate(journal = "BioRxiv", pubdate = date, pubyear = lubridate::year(date), pubmonth = lubridate::month(date), first_author = strsplit(authors,split=",")[[1]][1]) %>%
    arrange(desc(date))

  # cache dois
  message("Caching DOIs")
  write_lines(x = fetch$doi, file = cache_file)

  # make biblatex file
  bib = apply(fetch, 1, function(.x){
      bib = RefManageR::BibEntry("article", key = paste0(.x[["first_author"]], .x[["journal"]], .x[["pubyear"]]),
                                author = str_replace_all(.x[["authors"]], "; ", " and "), keywords="preprint",
                                title = .x[["title"]], year = .x[["pubyear"]], journaltitle = .x[["journal"]],
                                doi = .x[["doi"]], type = .x[["type"]])
      return(bib)
  })

  RefManageR::toBiblatex(bib) %>%
    write_lines("preprints.bib")
}
