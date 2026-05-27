#packages--------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(assertthat)
library(plotly)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
#constants----------------
forecast_years <- 2026:2036
industry_folder <- "industry_new"
industry_pattern <-  "IndustryEmploymentBC"
industry_sheets <- excel_sheets(list.files(here("data", industry_folder), full.names = TRUE))
macro_folder <- "macro_new"
macro_paths <- sort(list.files(here("data", macro_folder), full.names = TRUE))
macro_sheets <- c(rep("LabourMarket2", length(macro_paths)-1),"Labour Market")
macro_regions <- c("VIC","MSW","TOK","KOO","CAR","NCN","NE", "BC")
#functions-------------------------
get_industry_data <- function(sheet){
  missing_colname <- read_excel(list.files(here("data", industry_folder),
                                           pattern = industry_pattern,
                                           full.names = TRUE),
                                sheet = sheet,
                                skip=1,
                                col_types = "text")|>
    pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "industry_cut")|>
    mutate(industry_cut=as.numeric(industry_cut),
           year=as.numeric(year))|>
    filter(year %in% forecast_years)|>
    na.omit()

  colnames(missing_colname)[1] <- "lmo_detailed_industry"

  missing_colname|>
    mutate(lmo_detailed_industry=str_replace_all(lmo_detailed_industry, "online shopping",""))|> #too different for stringdist_join
    fuzzyjoin::stringdist_join(mapping, by = join_by(lmo_detailed_industry))|>
    summarize(industry_cut=sum(industry_cut, na.rm = TRUE), .by = c(stokes_industry, year))
}

get_macro_data <- function(path, sheet){
  missing_colname <- read_excel(path=path, sheet=sheet, skip=2, col_types="text")|>
    pivot_longer(cols = starts_with("2"), names_to = "year", values_to = "macro_cut")|>
    mutate(macro_cut=as.numeric(macro_cut),
           year=as.numeric(year))|>
    filter(year %in% forecast_years)

  colnames(missing_colname)[1] <- "stokes_industry"

  semi_join(missing_colname, stokes_industries, by = join_by(stokes_industry))
}
#get data------------------------
mapping <- read_excel(list.files(here("data"), pattern = "industry_mapping_with_stokes_agg", full.names = TRUE))|>
  select(lmo_detailed_industry, stokes_industry)|>
  distinct()
stokes_industries <- tibble(stokes_industry=unique(mapping$stokes_industry))

industry_data <- tibble(region=industry_sheets)|>
  mutate(data=map(industry_sheets, get_industry_data))|>
  unnest(data)

macro_data <- tibble(path=macro_paths, sheet=macro_sheets, region=macro_regions)|>
  mutate(data=map2(path, sheet, get_macro_data))|>
  select(-path, -sheet)|>
  unnest(data)

both <- full_join(industry_data, macro_data)

industry_vs_macro <- ggplot(both, aes(industry_cut,
                        macro_cut,
                        text=paste("Region:",
                                    region,
                                    "\n Stokes Industry:",
                                   stokes_industry,
                                   "\n Year:",
                                   year,
                                   "\n Industry Cut",
                                   industry_cut,
                                   "\n Macro Cut:",
                                   macro_cut)
                        )
              )+
  geom_abline(slope = 1, intercept = 0, colour="white", lwd=2)+
  geom_point(alpha=.5)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  labs(title="Comparison between final macro and first industry cuts",
    x="Industry Cut",
       y="Macro Cut")

plotly::ggplotly(industry_vs_macro, tooltip = "text")




