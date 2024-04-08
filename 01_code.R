# Copyright 2024 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(assertthat)
library(conflicted)
conflicts_prefer(dplyr::filter)

#functions----------------------
read_sheet <- function(which_file, sheet){
  path <- here("data",paste0(which_file,".xlsx"))
  tbbl <- read_excel(path=path,
                     sheet=sheet,
                     skip = 2,
                     na = "NA",
                     col_types = c("text", rep("numeric", 24)))
  colnames(tbbl)[1] <- "variable" #missing the first column name
  tbbl <- tbbl|>
    mutate(broad_category=variable, .after="variable")|>
    mutate(broad_category=if_else(broad_category %in% c("Trend Labour Force Participation Rates by Age & Sex (%)",
                                                        "Trend Estimated Labour Force by Age & Sex (000s)",
                                                        "Broad Population Age Groups (000s)",
                                                        "Population by 5-Year Age/Sex Groups (000s)",
                                                        "Females",
                                                        "Males",
                                                        "Households",
                                                        "Real ($2012 Millions)",
                                                        "Nominal ($Millions)",
                                                        "Income ($Millions)"),
                                  broad_category,
                                  NA_character_),
           percent_or_level=variable, .after="variable",
           variable=if_else(str_detect(variable, "% Change|Share %|% of Total"), NA_character_, variable),
           percent_or_level=if_else(str_detect(percent_or_level,"% Change|Share %|% of Total"), percent_or_level, "level"),
           )|>
    fill(variable, .direction="down")|>
    fill(broad_category, .direction="down")|>
    unite(variable, variable, percent_or_level, sep=": ", na.rm = TRUE)|>
    mutate(variable=if_else(is.na(broad_category),variable, paste(broad_category, variable, sep=": ")))|>
    select(-broad_category)|>
    filter(!variable %in% c("Income ($Millions): Other: level","Income ($Millions): Other: % Change"))|> #hack
    na.omit()|>
    pivot_longer(cols=-variable, names_to = "year", values_to = paste(which_file,"value", sep="_"))
}

add_change <- function(tbbl){
  tbbl|>
    mutate(change=if_else(str_detect(variable, "% Change"),
                          new_value-original_value,
                          (new_value/original_value-1)*100
    ))|>
    select(-new_value, -original_value)|>
    pivot_wider(names_from = "year", values_from = "change")|>
    na.omit()|>
    column_to_rownames("variable")
}

# the program-------------------------------------

original_tbbl <- tibble(which_file="original", sheet=excel_sheets(here("data", paste0(which_file, ".xlsx"))))|>
  mutate(original_data=map2(which_file, sheet, read_sheet))|>
  select(-which_file)

new_tbbl <- tibble(which_file="new", sheet=excel_sheets(here("data",paste0(which_file,".xlsx"))))|>
  mutate(new_data=map2(which_file, sheet, read_sheet))|>
  select(-which_file)

# sanity check------------------------
assert_that(identical(original_tbbl$sheet, new_tbbl$sheet), msg = "Original and new workbooks must have same sheets")

joined <- full_join(original_tbbl, new_tbbl)|>
  mutate(joined=map2(new_data, original_data, inner_join),
         joined=map(joined, add_change))




