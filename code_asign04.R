# Paths
robotstxt::paths_allowed("https://en.wikipedia.org/wiki/List_of_countries_by_foreign-exchange_reserves")

## EPPS 6302 Methods of Data Collection and Production
## Web scraping 1 — static pages with rvest                     rvest_wiki01.R
##
## Three targets, in order of increasing honesty about what scraping is for:
##   1. a sandbox built to be scraped        (practice, zero ambiguity)
##   2. a Wikipedia table                    (the classic exercise)
##   3. the same numbers from an API         (what you should have done)

install.packages(c("rvest", "dplyr", "tidyr", "stringr","readr", "polite", "janitor", "WDI"))


library(rvest)
library(dplyr)
library(stringr)
library(readr)

## ---- 0. Ask permission first ----------------------------------------------
## Always before the first request. robotstxt::paths_allowed() returns TRUE or
## FALSE for the exact path you intend to fetch.

robotstxt::paths_allowed("https://en.wikipedia.org/wiki/List_of_countries_by_foreign-exchange_reserves")

## ---- 1. The sandbox: books.toscrape.com ------------------------------------
## Built and published for scraping practice. 1,000 books, 50 pages, no
## JavaScript. Learn the verbs here before you point them at anyone's server.

books_pg <- read_html("https://books.toscrape.com/catalogue/page-1.html")

books <- tibble(
  title  = books_pg |> html_elements("article.product_pod h3 a") |> html_attr("title"),
  price  = books_pg |> html_elements("article.product_pod p.price_color") |> html_text2(),
  rating = books_pg |> html_elements("article.product_pod p.star-rating") |>
    html_attr("class") |> str_remove("star-rating ")
) |>
  mutate(price_gbp = parse_number(price))

head(books)

# Extract title, price and rating. 
# page through five pages and bind them into one dataframe.

scrape_books_page <- function(page) {
  
  url <- paste0(
    "https://books.toscrape.com/catalogue/page-",
    page,
    ".html"
  )
  
  pg <- read_html(url)
  
  tibble(
    title = pg |>
      html_elements("article.product_pod h3 a") |>
      html_attr("title"),
    
    price = pg |>
      html_elements("article.product_pod p.price_color") |>
      html_text2(),
    
    rating = pg |>
      html_elements("article.product_pod p.star-rating") |>
      html_attr("class") |>
      str_remove("star-rating ")
  ) |>
    mutate(
      price_gbp = parse_number(price),
      page = page
    )
}
# 5 variables: title, price, rating, price_gbp, page

books_5pages <- lapply(1:5, scrape_books_page) |>
  bind_rows()

head(books_5pages)
tail(books_5pages)


## ---- 2. The Wikipedia table -----------------------------------------------

# 2a.
## html_table() does the parsing. The work is everything after it.

wiki_url <- paste0("https://en.wikipedia.org/wiki/",
                   "List_of_countries_by_foreign-exchange_reserves")

wiki_pg <- read_html(wiki_url)

## Never assume the table you want is [[1]]. Look first.
tabs <- wiki_pg |> html_elements("table.wikitable")
length(tabs)                      # how many candidates? --> 2
tabs |> html_table() |> lapply(\(x) dim(x))

reserves_raw <- tabs[[1]] |> html_table()
glimpse(reserves_raw)

## Clean-up. Wikipedia tables arrive with footnote markers, thin spaces,
## multi-row headers and a stray total row. Expect to redo this when the
## article is edited — which is the point of the exercise.

reserves <- reserves_raw |>
  janitor::clean_names() |>
  mutate(
    across(
      where(is.character),
      ~ .x |>
        str_remove_all("\\[.*?\\]") |>
        str_squish()
    )
  ) |>
  filter(
    !str_detect(
      country_as_recognized_by_the_un,
      regex("total|world", ignore_case = TRUE)
    )
  )
reserves <- reserves[-c(1, 2), ]
names(reserves)
glimpse(reserves)
head(reserves, 10)
tail(reserves, 10)

reserves$last_reporteddate |> head(10)

#Foreign reserves- Foreign-exchange reserves including gold, US$ million

reserves <- reserves |>
  select(
    country_as_recognized_by_the_un,
    continent,
    foreign_exchange_reserves,
    last_reporteddate
  )
reserves <- reserves |>
  mutate(
    foreign_exchange_reserves = parse_number(foreign_exchange_reserves)
  )
head(reserves)
glimpse(reserves)
summary(reserves)

## ---- 2b. A different Wikipedia table --------------------------------------

fifa_url <- "https://en.wikipedia.org/wiki/FIFA_World_Cup"
fifa_pg <- read_html(fifa_url)
fifa_tabs <- fifa_pg |>
  html_elements("table.wikitable")

length(fifa_tabs)

fifa_tabs |>
  html_table() |>
  lapply(\(x) dim(x))

tabs |> html_table() |> lapply(\(x) dim(x))

lapply(fifa_tables, head)

fifa_results_raw <- fifa_tabs[[7]] |>
  html_table()
glimpse(fifa_results_raw)
head(fifa_results_raw, 10)

fifa_results_raw |>
  janitor::clean_names() |>
  filter(
    !str_detect(
      country_as_recognized_by_the_un,
      regex("total|world", ignore_case = TRUE)
    )
  ) 
# The scraping worked. The cleaning code broke because it was written 
# specifically for the structure of the first Wikipedia table. this table doesnt have the same
#There are no summary Total or World rows in this table

#2c.
fifa_results <- fifa_results_raw |>
  janitor::clean_names()

names(fifa_results)
fifa_results <- fifa_results |>
  mutate(
    year = readr::parse_number(world_cup)
  )
fifa_results <- fifa_results |>
  mutate(
    across(
      where(is.character),
      ~ .x |>
        str_remove_all("\\[.*?\\]") |>
        str_squish()
    )
  )
head(fifa_results$year)
fifa_results <- fifa_results |>
  mutate(
    across(
      where(is.character),
      ~ .x |>
        str_remove_all("\\[.*?\\]") |>
        str_squish()
    )
  )

head(fifa_results)
names(fifa_results)

## ---- 3. The same quantity, from an API -------------------------------------
## Foreign reserves are published by the World Bank as FI.RES.TOTL.CD,
## "Total reserves (includes gold, current US$)". One call, versioned,
## documented, and identical for whoever runs it.

## 3a. Get the same quantity from the World Bank -------------------------------

library(WDI)
library(lubridate)

# World Bank indicator:
# FI.RES.TOTL.CD = Total reserves (includes gold, current US$)

reserves_api <- WDI(
  indicator = "FI.RES.TOTL.CD",
  start = 2015,
  end = 2026,
  extra = TRUE
)

head(reserves_api)


## 3b. Prepare the Wikipedia data for comparison -------------------------------

# Convert Wikipedia's reporting date from character to Date
reserves_compare <- reserves |>
  mutate(
    last_reporteddate = dmy(last_reporteddate),
    
    # Wikipedia reports reserves in US$ million,
    # so convert to current US dollars
    wiki_reserves_usd = foreign_exchange_reserves * 1000000,
    
    # Extract the year from the reporting date
    wiki_year = year(last_reporteddate)
  ) |>
  select(
    country = country_as_recognized_by_the_un,
    last_reporteddate,
    wiki_year,
    wiki_reserves_usd
  )

head(reserves_compare)


## Match each country to the World Bank value for the same year ---------------

reserves_comparison <- reserves_compare |>
  left_join(
    reserves_api |>
      select(
        country,
        year,
        world_bank_reserves_usd = FI.RES.TOTL.CD,
        world_bank_last_updated = lastupdated
      ),
    by = c("country" = "country", "wiki_year" = "year")
  ) |>
  mutate(
    difference_usd = wiki_reserves_usd - world_bank_reserves_usd,
    difference_percent =
      (difference_usd / world_bank_reserves_usd) * 100
  ) |>
  arrange(desc(abs(difference_usd)))


## View the comparison table ---------------------------------------------------

reserves_comparison

reserves_comparison <- reserves_compare |>
  left_join(
    reserves_api |>
      select(
        country,
        wb_year = year,
        world_bank_reserves_usd = FI.RES.TOTL.CD,
        world_bank_last_updated = lastupdated
      ),
    by = c("country" = "country", "wiki_year" = "wb_year")
  ) |>
  mutate(
    difference_usd = wiki_reserves_usd - world_bank_reserves_usd,
    difference_percent =
      (difference_usd / world_bank_reserves_usd) * 100
  ) |>
  select(
    country,
    wiki_date = last_reporteddate,
    wiki_year,
    wiki_reserves_usd,
    world_bank_reserves_usd,
    world_bank_last_updated,
    difference_usd,
    difference_percent
  ) |>
  arrange(desc(abs(difference_percent)))
print(reserves_comparison, n = 20)

reserves_compare |>
  filter(country == "United States")

reserves_api |>
  filter(country == "United States") |>
  select(country, year, FI.RES.TOTL.CD, lastupdated)

## ---- 4. Cache what you pulled ----------------------------------------------
write_csv(reserves, paste0("reserves_wiki_", Sys.Date(), ".csv"))

## ---- 5. The question this script exists to raise ---------------------------
## Compare 2 and 3. Same concept, two data-generating processes:
##   - Wikipedia: edited by volunteers, sourced from many places, no schema,
##     no versioning, no uncertainty, changes without notice
##   - World Bank: one compiler, documented methodology, stable indicator code
## Scraping was the wrong tool here. Knowing when it is the *only* tool is the
## skill this course is actually teaching.