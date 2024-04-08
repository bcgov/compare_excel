
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(assertthat)
library(conflicted)
conflicts_prefer(dplyr::filter)

#functions----------------------
read_sheet <- function(which_file, sheet, sub_directory){
  path <- here("data", sub_directory, which_file)
  tbbl <- read_excel(path=path,
                     sheet=sheet,
                     skip = 2,
                     na = "NA",
                     col_types = c("text", rep("numeric", 24)))
  colnames(tbbl)[1] <- "variable" #missing name of the series identifier
  #' Column "variable" is not machine usable: multiple rows with the same identifier.
  #' We make the following modifications so each row in a table has a unique identifier
  #' 1) If there is a row with only the variable (no data), this is a broad category that applies to all
  #' rows below until the next broad category.
  #' 2) If variable is one of % Change, Share %, % of Total, this row is referring to the row above.

  tbbl <- tbbl|>
    filter(!is.na(variable))|>#gets rid of empty rows
    mutate(variable=if_else(variable=="Annual Wage Rate ($000s)","Annual Wage Rate", variable))|>
    mutate(broad_category=variable, .after="variable")|> #Broad categories are the rows that do not have data.
    mutate(broad_category=if_else(broad_category %in% c("Trend Labour Force Participation Rates by Age & Sex (%)",
                                                        "Trend Estimated Labour Force by Age & Sex (000s)",
                                                        "Broad Population Age Groups (000s)",
                                                        "Population by 5-Year Age/Sex Groups (000s)",
                                                        "Females",
                                                        "Males",
                                                        "Households",
                                                        "Real ($2012 Millions)",
                                                        "Nominal ($Millions)",
                                                        "Income ($Millions)",
                                                        "AGRICULTURE",
                                                        "OTHER OTHER PRIMARY",
                                                        "OIL & GAS",
                                                        "MANUFACTURING",
                                                        "CONSTRUCTION",
                                                        "UTILITIES",
                                                        "TRANSPORTATION & WAREHOUSING",
                                                        "TRADE",
                                                        "FIRE",
                                                        "PROFESSIONAL, SCIENTIFIC, MANAGERIAL",
                                                        "ACCOMMODATION & FOOD SERVICES",
                                                        "OTHER SERVICES",
                                                        "EDUCATION SERVICES",
                                                        "HEALTH & SOCIAL SERVICES",
                                                        "GOVERNMENT SERVICES"
    ),
    broad_category,
    NA_character_),
    percent_or_level=variable, .after="variable",
    variable=if_else(str_detect(variable, "% Change|Share %|% of Total|Natural Growth Rate"),
                     NA_character_, #replace these with NA (make space for down-fill)
                     variable),
    percent_or_level=if_else(str_detect(percent_or_level,
                                        "% Change|Share %|% of Total|Natural Growth Rate|Unemployment Rate"),
                             percent_or_level,
                             "level"), #if not one of these it is a level
    )|>
    fill(variable, .direction="down")|> #fills in the space we emptied out (getting rid of % Change, Share %, % of Total above)
    fill(broad_category, .direction="down")|> # broad categories apply to all data below, until next broad category.
    unite(variable, variable, percent_or_level, sep=": ", na.rm = TRUE)|> #append level or % to the variable name.
    mutate(variable=if_else(is.na(broad_category), # prepend the broad category, if there is a broad category.
                            variable,
                            paste(broad_category, variable, sep=": ")))|>
    select(-broad_category)|>
    filter(!variable %in% c("Income ($Millions): Other: level",
                            "Income ($Millions): Other: % Change"))|> #still redundancy in the naming... filter it out.
    pivot_longer(cols = -variable,
                 names_to = "year",
                 values_to = paste(sub_directory,"value", sep="_"))
}

add_change <- function(tbbl){
  tbbl <- tbbl|>
    mutate(change=if_else(str_detect(
      variable,
      "% Change|Share %|% of Total|Natural Growth Rate|Trend Labour Force Participation Rates|Unemployment Rate"),
      new_value-original_value,
      (new_value/original_value-1)*100
    ))|>
    select(-new_value, -original_value)|>
    filter(!str_detect(variable,"PROFESSIONAL, SCIENTIFIC, MANAGERIAL")) #not sure why this not working?
}

get_sheets <- function(path){
  tibble(sheet=excel_sheets(path))
}

# the program-------------------------------------

original_tbbl <- tibble(which_file=list.files(here("data", "original")),
                        path=here("data", "original", which_file)
)|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  mutate(original_data=map2(which_file, sheet, read_sheet, "original"))|>
  select(-path)

new_tbbl <- tibble(which_file=list.files(here("data", "new")),
                   path=here("data", "new", which_file)
)|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  mutate(new_data=map2(which_file, sheet, read_sheet, "new"))|>
  select(-path)

joined <- full_join(original_tbbl, new_tbbl)|>
  mutate(joined=map2(new_data, original_data, inner_join),
         joined=map(joined, add_change))|>
  arrange(desc(which_file))

write_rds(joined, here("out","joined.rds"))
