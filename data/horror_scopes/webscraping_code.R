# This script adapts John Hermon's webscraping tidytuesday code to add more data
# which I will use in my text modelling case study
# run this script one function at a time to debug

library(tidyverse)
library(here)
library(fs)
library(rvest)
# install.packages("polite")
library(polite)
library(xml2)

working_dir <- here::here("data", "horror_scopes")

url_home <- 
  "https://www.snopes.com/sitemap/"
#archive_main > div.archive_content_wrapper > div:nth-child(2)

session <- bow(url_home, user_agent = "Emmanuel Miiro, MD")

archives_section_url <- scrape(session) %>% 
  html_elements(".archive_box:nth-child(2) a") %>% 
  # html_elements("a") %>%
  html_attr("href") # 76 category URLs!

# write a function to extract categories
# regex pattern to extract categories
regex_pattern <- "/category/([^/]+)"
extract_category <- function(url){
  category_name <- str_match(url, regex_pattern)[,2]
}
categories <- map(archives_section_url, extract_category) |> unlist()
categories

# section_landings <- map(archives_sections, \(section_url){
#   section_home_page <- read_html(section_url)
#   })
  

# urls <- paste0(
#   "https://www.snopes.com/fact-check/category/horrors/?pagenum=",
#   1:15
# )

## use only 5 subpages in each category. Remember that each subpage contains about 20 articles linked to it for each of 76 categories
# category_sub_pages_url <- map(archives_section_url, ~paste0(.x, 1:5)) %>% unlist()

## Or use a function to keep clicking next and grab the info for variable page_lengths
## code from Jeroen Janssens at https://jeroenjanssens.com/scrape/
# html_more_elements <- function(session, css, more_css) {
#   xml2:::xml_nodeset(c(
#     html_elements(session, css),
#     tryCatch({
#       html_more_elements(session_follow_link(session, css = more_css),
#                          css, more_css)
#     }, error = function(e) NULL)
#   ))
# }

# next button
#both-buttons > a.next-button
# more_css <- "#both-buttons > a.next-button"

# create a list of urls to scrape
# install dev version of httr2
# devtools::install_github("https://github.com/r-lib/httr2")
library(httr2)
# article_list_url <- archives_section_url |> map(\(archives_section_url){
#   # more_css <- "#both-buttons > a.next-button"
#   archives_section_url <- nod(session, archives_section_url)
#   url <- scrape(archives_section_url) |>
#     html_element(more_css) |>
#     html_attr("href") |>
#     iterate_with_link_url(rel = "next") |> 
#     resps_data()
# })
# ... use the dev version of httr2 with experimental iterate functions

extract_rating <- function(article_page) {
  rating <- article_page |> 
    rvest::html_element(".rating_title_wrap") |> 
    rvest::html_text2() |> 
    stringr::str_remove("About this rating")
  if (is.na(rating)) {
    rating <- article_page |> 
      rvest::html_element(".status_color") |> 
      rvest::html_text2()
  }
  if (is.na(rating)) {
    rating <- article_page |> 
      rvest::html_elements("noindex") |> 
      rvest::html_text2() |> 
      stringr::str_squish() |> 
      stringr::str_subset("^Status:") |> 
      stringr::str_remove("Status:")
  }
  rating <- tolower(rating) |> 
    stringr::str_squish() |> 
    stringr::str_remove("\\.|\\:")
  rating <- dplyr::case_match(
    rating,
    c(
      "a number of real entries, one unknown, and one fiction",
      "multiple",
      "multiple — see below",
      "two real entries, the others are fiction"
    ) ~ "mixture",
    .default = rating
  )
  return(rating)
}

extract_claim <- function(article_page) {
  claim <- article_page |> 
    rvest::html_element(".claim_cont") |> 
    rvest::html_text2() |> 
    stringr::str_squish()
  if (is.na(claim)) {
    claim <- rvest::html_elements(article_page, "p") |> 
      rvest::html_text2() |> 
      stringr::str_subset("^Claim:") |> 
      stringr::str_remove("Claim:") |> 
      stringr::str_squish()
  }
  return(claim)
}

# define the function that lands on the category homepage, extracts information
extract_begins_at_category <- function(article_list_url){
  category_name <- extract_category(article_list_url)
  article_list_url <- nod(session, article_list_url)
      article_list_url |> 
        scrape() |> 
        rvest::html_elements(".article_wrapper") |> 
        purrr::map(
          \(article) {
            # Grabbbing info from this page can result in truncation. Instead grab the
            # URL and dig into that.
            url <- article |>
              rvest::html_element("a") |>
              rvest::html_attr("href")
            article_page <- rvest::read_html(url)
            tibble::tibble(
              title = article_page |>
                rvest::html_element("h1") |> 
                rvest::html_text2(),
              url = url,
              # Failed for some articles <= 2015-05-16
              rating = extract_rating(article_page),
              subtitle = article_page |>
                rvest::html_element("h2") |> 
                rvest::html_text2(),
              author = article_page |> 
                rvest::html_element(".author_name") |> 
                rvest::html_text() |> 
                stringr::str_squish(),
              published = article |> 
                rvest::html_element(".article_date") |> 
                rvest::html_text2() |> 
                lubridate::mdy(),
              # Failed for some articles <= 2015-05-16
              claim = extract_claim(article_page),
              category_name = category_name
              
            )
          }
        )  |> purrr::list_rbind()
    } 
    # |>  mutate(category = extract_category(article_list_url))
#   ) 
# |> purrr::list_rbind()

# define a function clicks next to iterate over pages that belong to that particular category
finalized_crawler <- function(category_url){
  req <-request(category_url) |> map(extract_begins_at_category)
  resp <- req_perform_iterative(req, next_req = iterate_with_link_url(rel = "next"), on_error = "return")
  resp |> mapply(extract_begins_at_category) |> list_rbind()
}

snopes_articles <- archives_section_url |>  map(finalized_crawler)

readr::write_csv(
  snopes_articles,
  fs::path(working_dir, "snopes_articles.csv")
)