#libraries-----------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(assertthat)
library(conflicted)
conflicts_prefer(dplyr::filter)
skip=2 #how many rows to skip at top of excel files
#functions----------------------
read_sheet <- function(which_file, sheet, sub_directory){
  path <- here("data", sub_directory, which_file)
  tbbl <- read_excel(path=path,
                     sheet=sheet,
                     skip = skip,
                     na = "NA",
                     col_types = c("text", rep("numeric", 24)))
  colnames(tbbl)[1] <- "variable" #missing name of the series identifier

  tbbl <- tbbl|>
    mutate(variable=paste0((row_number()+skip+1),": ", variable))|>
    filter(!is.na(variable))|>
    pivot_longer(cols = -variable,
                 names_to = "year",
                 values_to = paste(sub_directory,"value", sep="_"))
}

get_sheets <- function(path){
  tibble(sheet=excel_sheets(path))
}
# the program-------------------------------------

original_tbbl <- tibble(which_file=list.files(here("data", "original")),
                        path=here("data", "original", which_file))|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  mutate(original_data=map2(which_file, sheet, read_sheet, "original"))|>
  select(-path)

new_tbbl <- tibble(which_file=list.files(here("data", "new")),
                   path=here("data", "new", which_file))|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  mutate(new_data=map2(which_file, sheet, read_sheet, "new"))|>
  select(-path)

joined <- full_join(original_tbbl, new_tbbl)|>
  mutate(joined=map2(new_data, original_data, inner_join))|>
  select(-new_data, -original_data)

write_rds(joined, here("out","joined.rds"))




