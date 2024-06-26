require(rentrez)
require(RefManageR)
require(purrr)
require(dplyr)
require(stringr)
require(readr)
require(lubridate)
require(glue)
require(here)
require(xml2)

#' Parse results from pubmed search
parse_pubmed <- function(fetch){

  # Reformat authors to first name + last name
  authorlist = fetch %>%
    pluck("MedlineCitation","Article","AuthorList") %>%
    keep(~ length(.x) >= 3)
  authors = map_chr(authorlist, function(author) {
    initials = pluck(author, "Initials",1)
    first_name = pluck(author, "ForeName",1)
    last_name = pluck(author, "LastName",1)
    stringsAsFactors = F
    return(paste(first_name,last_name))
  }) %>% unname()

  # grab first author last name
  first_author = pluck(authorlist, 1, 'LastName',1)

  # parse doi
  doi = fetch %>% pluck("PubmedData","ArticleIdList") %>%
    keep(~ attr(.x,"IdType") == "doi") %>%
    pluck("ArticleId",1)

  # parse publication date
  pubmed = fetch %>%
    pluck("PubmedData","History") %>%
    keep(~ attr(.x,"PubStatus") == "pubmed")
  pubyear = pubmed %>% pluck("PubMedPubDate","Year",1)
  pubmonth = pubmed %>% pluck("PubMedPubDate","Month",1)
  pubmonth = ifelse(nchar(pubmonth) > 1, pubmonth, paste0("0",pubmonth))
  pubday = pubmed %>% pluck("PubMedPubDate","Day",1)
  pubdate = glue("{pubyear}{pubmonth}{pubday}") %>% lubridate::as_date()

  # parse journal title
  title = fetch %>%
    pluck("MedlineCitation","Article","ArticleTitle") %>%
    unlist() %>%
    paste0(collapse = "")

  # parse journal info
  journaltitle = fetch %>%
    pluck("MedlineCitation","Article","Journal","Title",1) %>%
    str_replace(":.*$", "")
  journalabbr = fetch %>%
    pluck("MedlineCitation","Article","Journal","ISOAbbreviation",1) %>%
    str_replace(":.*$", "")
  volume = fetch %>%
    pluck("MedlineCitation","Article","Journal","JournalIssue","Volume",1)
  issue = fetch %>%
    pluck("MedlineCitation","Article","Journal","JournalIssue","Issue",1)


  message(glue("Found {title} by {pluck(authorlist, 1, 'LastName',1)} et al. published on {pubdate} in {journaltitle}"))

  # make bibentry
  # FIXME: warning: All formats failed to parse. No formats found.
  bib = RefManageR::BibEntry("article", key = paste0(first_author, str_remove_all(journalabbr," "),pubyear), title = title,
                             journaltitle = journaltitle, shortjournal = journalabbr, langid = "english", keywords = "published",
                             year = pubyear, month = pubmonth, author = paste(authors, collapse = " and "),
                             date = as.character(pubdate), volume = volume, issue = issue, doi = doi)

  return(bib)
}

#' Search pubmed for a given term and return the results as a dataframe
#' @param term The term to search for
search_pubmed <- function(term) {

  # search with input term
  pmids = rentrez::entrez_search("pubmed", term, retmax=1000) %>% pluck("ids")

  # fetch results and parse xml
  fetch = rentrez::entrez_fetch("pubmed", pmids, rettype = "xml", parsed = F) %>%
    xml2::read_xml() %>% xml2::as_list() %>% pluck(1)

  # process each fetch
  bib = map(fetch, parse_pubmed) %>% unname()
  bib = bib[!grepl("Erratum|Correction", bib)] # remove erratums and corrections

  RefManageR::toBiblatex(bib) %>%
    write_lines("pubmed.bib")
}
