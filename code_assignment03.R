# Assignment 3

install.packages(c(
  "tidycensus",
  "tigris",
  "sf",
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr"
))

library(tidycensus)
library(tigris)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

options(tigris_use_cache = TRUE)

vars <- load_variables(2024, "acs5")
vars |> dplyr::filter(grepl("^B19", name)) |> dplyr::slice_head(n = 10)
vars |>
  dplyr::filter(name %in% c(
    "B19013_001",
    "B17001_001",
    "B17001_002"
  ))

vars |>
  filter(grepl("housing costs", label, ignore.case = TRUE)) |>
  select(name, label)

vars |>
  filter(grepl("30 percent", label, ignore.case = TRUE)) |>
  select(name, label)

vars |>
  filter(grepl("gross rent", label, ignore.case = TRUE)) |>
  select(name, label)

vars |>
  filter(grepl("gross rent.*household income", label, ignore.case = TRUE)) |>
  select(name, label)

# What is the distribution of rental housing cost burden across Texas counties?
# What share of renter households in each Texas county are spending a high share of their income on rent?
# Geography: Texas counties
state_abbr <- "TX"
geo_level <- "county"
# Variable 1: Median household income
# Variable 2: households spending ≥30% on rent. The percentage of renter households that spend 30% or more of their household income on gross rent
# variable 3 for the counts rate: total renter households

vars <- load_variables(2024, "acs5")

vars |>
  filter(name %in% c(
    "B25070_001",
    "B25070_007",
    "B25070_008",
    "B25070_009",
    "B25070_010",
    "B19013_001"
  ))

vars |>
  filter(grepl("^B25070", name))

vars |>
  filter(grepl("^B19013", name))

# 3) Parameters 
state_abbr <- "TX"
geo_level <- "county"

my_vars <- c(
  renters = "B25070_001",
  burden_30_35 = "B25070_007",
  burden_35_40 = "B25070_008",
  burden_40_50 = "B25070_009",
  burden_50_plus = "B25070_010",
  income = "B19013_001"
)

year_acs <- 2024
survey <- "acs5"

# 4) Download, wide — one row per area, and the sf class survives
acs_wide <- get_acs(
  geography = geo_level,
  variables = my_vars,
  state = state_abbr,
  year = year_acs,
  survey = survey,
  geometry = TRUE,
  output = "wide"
)


# 5) Derive a rate, and carry its margin of error through
# rent-burden count: each county has a count of renter households spending at least 30% of income on rent
acs_wide <- acs_wide |>
  mutate(burden_30_plusE = burden_30_35E + burden_35_40E + burden_40_50E + burden_50_plusE)

#rate calculation
acs_wide <- acs_wide |>
  mutate(burden_rate = burden_30_plusE / rentersE)

#moe calculation
acs_wide <- acs_wide |>
  rowwise() |>
  mutate(
    burden_30_plusM = moe_sum(
      c(
        burden_30_35M,
        burden_35_40M,
        burden_40_50M,
        burden_50_plusM
      ),
      estimate = c(
        burden_30_35E,
        burden_35_40E,
        burden_40_50E,
        burden_50_plusE
      )
    )
  ) |>
  ungroup()

acs_wide <- acs_wide |>
  mutate(
    burden_moe = moe_prop(
      burden_30_plusE,
      rentersE,
      burden_30_plusM,
      rentersM
    )
  )

#get data in percentages
acs_wide <- acs_wide |>
  mutate(
    burden_pct = burden_rate * 100,
    burden_moe_pct = burden_moe * 100
  )


# 6) Map (edit titles/theme)
rent_burden_map <- ggplot(acs_wide) +
  geom_sf(
    aes(fill = burden_pct),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_gradient(
    low = "#F5E6D3",
    high = "#8C2D04",
    name = "Renter households\nspending ≥30% on rent",
    labels = function(x) paste0(x, "%")
  ) +
  labs(title = paste0("Rental Housing Cost Burden Across Texas Counties ", year_acs), subtitle = "Share of renter households spending 30% or more of income on gross rent",
       caption = "Source: U.S. Census Bureau via tidycensus") +
  theme_void() +
  theme(
    text = element_text(family = "Arial"),
    
    plot.title = element_text(
      family = "Georgia",
      size = 20,
      face = "bold",
      hjust = 0,
      margin = margin(b = 12)
    ),
    
    plot.subtitle = element_text(
      family = "Arial",
      size = 10.5,
      color = "gray30",
      hjust = 0,
      margin = margin(b = 12)
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      family = "Palatino",
      size = 9,
      face = "bold"
    ),
    
    legend.text = element_text(
      family = "Arial",
      size = 8
    ),
    
    plot.caption = element_text(
      family = "Arial",
      size = 8,
      color = "gray40",
      hjust = 0
    ),
    
    plot.margin = margin(15, 15, 15, 15)
  )
ggsave(
  "texas_rent_burden_map.png",
  plot = rent_burden_map,
  width = 10,
  height = 6.5,
  dpi = 300
)

# 7) Table (top/bottom by poverty rate, with MOE)
top10 <- acs_wide |>
  st_drop_geometry() |>
  arrange(desc(incomeE)) |>
  select(
    NAME,
    incomeE,
    incomeM,
    burden_pct,
    burden_moe_pct
  ) |>
  slice_head(n = 10)

bottom10 <- acs_wide |>
  st_drop_geometry() |>
  arrange(incomeE) |>
  select(
    NAME,
    incomeE,
    incomeM,
    burden_pct,
    burden_moe_pct
  ) |>
  slice_head(n = 10)
top10
bottom10

uncertainty_check <- top10_income |>
  mutate(
    lower = burden_pct - burden_moe_pct,
    upper = burden_pct + burden_moe_pct
  ) |>
  mutate(
    next_county = lead(NAME),
    next_lower = lead(lower),
    next_upper = lead(upper),
    overlap = lower <= next_upper & upper >= next_lower
  ) |>
  filter(overlap) |>
  slice_head(n = 1)

uncertainty_check
# 8) Save outputs
write_csv(st_drop_geometry(acs_wide),
          paste0("acs_", state_abbr, "_", year_acs, ".csv"))


# 10) Table — top and bottom 10 counties by median household income

top10 <- acs_wide |>
  st_drop_geometry() |>
  arrange(desc(incomeE)) |>
  transmute(
    Group = "Top 10",
    County = NAME,
    `Median household income` = incomeE,
    `Income MOE` = incomeM,
    `Rent burden (%)` = burden_pct,
    `MOE (%)` = burden_moe_pct
  ) |>
  slice_head(n = 10)

bottom10 <- acs_wide |>
  st_drop_geometry() |>
  arrange(incomeE) |>
  transmute(
    Group = "Bottom 10",
    County = NAME,
    `Median household income` = incomeE,
    `Income MOE` = incomeM,
    `Rent burden (%)` = burden_pct,
    `MOE (%)` = burden_moe_pct
  ) |>
  slice_head(n = 10)

income_table <- bind_rows(top10, bottom10) |>
  mutate(
    `Median household income` =
      paste0("$", format(round(`Median household income`), big.mark = ",")),
    `Income MOE` =
      paste0("±$", format(round(`Income MOE`), big.mark = ",")),
    `Rent burden (%)` =
      round(`Rent burden (%)`, 1),
    `MOE (%)` =
      round(`MOE (%)`, 1)
  )

knitr::kable(
  income_table,
  caption = "Texas Counties with the Highest and Lowest Median Household Incomes"
)

