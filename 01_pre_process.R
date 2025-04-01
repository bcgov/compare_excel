#' you need to set the cut type:
cut <- "macro"
#cut <- "industry"

#' NOTE: the files that are being compared need to be quite similar:
#' they need to have identical file names (between versions)
#' they need to have the same sheet names (between versions)
#' they need to have the same series names aka row identifiers (between versions)
#' the only thing that should differ is the numeric data.
#' there is a fuzzyjoin around line 130... this should be checked manually.

#libraries-----------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(assertthat)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
# constants---------------

if(cut=="macro"){
  new_folder = "macro_new"
  old_folder = "macro_old"
  pattern = "BritishColumbiaTables" #this is for comparing stokes to internal
  sheet = "Labour Market" #this is for comparing stokes to internal
  skip = 2
  add_rows = 3
}else if(cut=="industry"){
  new_folder = "industry_new"
  old_folder = "industry_old"
  pattern = "IndustryEmploymentBC"#this is for comparing stokes to internal
  sheet = "BC"#this is for comparing stokes to internal
  skip = 1
  add_rows = 2
}else{
  stop()
}

#functions----------------------
read_sheet <- function(which_file, sheet, sub_directory, prepend){
  path <- here("data", sub_directory, which_file)

  num_columns <- ncol(read_excel(path=path,
                     sheet=sheet,
                     skip = skip,
                     na = "NA",
                     n_max = 10))

  tbbl <- read_excel(path=path,
                     sheet=sheet,
                     skip = skip,
                     na = "NA",
                     col_types = c("text", rep("numeric", num_columns-1)))
  colnames(tbbl)[1] <- "variable" #missing name of the series identifier

  tbbl <- tbbl|>
    mutate(variable=paste0((row_number()+add_rows),": ", variable))|>
    filter(!is.na(variable),
           !str_detect(variable, "%")
           )|>
    pivot_longer(cols = -variable,
                 names_to = "year",
                 values_to = paste(prepend, "value", sep="_"))
}

get_sheets <- function(path){
  tibble(sheet=excel_sheets(path))
}

get_cagrs <- function(tbbl, column){
  val_now <- tbbl[[column]][tbbl$name==(max(tbbl$name)-10)]
  val_fyfn <- tbbl[[column]][tbbl$name==(max(tbbl$name)-5)]
  val_tyfn <- tbbl[[column]][tbbl$name==max(tbbl$name)]
  cagr_ffy <- ((val_fyfn/val_now)^.2-1)
  cagr_sfy <- ((val_tyfn/val_fyfn)^.2-1)
  cagr_ty <- ((val_tyfn/val_now)^.1-1)
  tibble(cagr_ffy=cagr_ffy, cagr_sfy=cagr_sfy,cagr_ty=cagr_ty)
}


# the program-------------------------------------

original_tbbl <- tibble(which_file=list.files(here("data", old_folder)),
                        path=here("data", old_folder, which_file))|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  mutate(original_data=map2(which_file, sheet, read_sheet, old_folder, "original"))|>
  select(-path)

new_tbbl <- tibble(which_file=list.files(here("data", new_folder)),
                   path=here("data", new_folder, which_file))|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  mutate(new_data=map2(which_file, sheet, read_sheet, new_folder, "new"))|>
  select(-path)

joined <- full_join(original_tbbl, new_tbbl)|>
  mutate(joined=map2(new_data, original_data, inner_join))|>
  select(-new_data, -original_data)

write_rds(joined, here("out","joined.rds"))

############ internal_vs_stokes------------------------

mapping <- read_excel(here("data", "industry_mapping_2025_with_stokes_agg.xlsx"))

detailed_to_stokes <- mapping|>
  select(lmo_detailed_industry, stokes_industry)|>
  distinct()

internal <- read_excel(here("data",
                            "Preliminary Industry Forecast for LMO 2025 Edition.xlsx"),
                       skip=0)|>
  select(-contains("CAGR"))|>
  pivot_longer(cols=-industry, values_to = "internal")|>
  separate(industry, into=c("code", "industry"), sep=": ")|>
  select(-code)

if(cut=="macro"){
  internal <- inner_join(internal, detailed_to_stokes, by=c("industry"="lmo_detailed_industry"))|>
    group_by(name, industry=stokes_industry)|>
    summarize(internal=sum(internal))
}

stokes_cut <- read_excel(here("data",
                              new_folder,
                              list.files(here("data",new_folder), pattern = pattern)),
                         sheet = sheet,
                         skip =skip)

colnames(stokes_cut)[1] <- "industry"

internal_vs_stokes_wrong <- stokes_cut|>
  pivot_longer(cols=-industry, values_to = "stokes_cut")|>
  filter(!industry %in% c("Total", "% Change"),
         !is.na(industry))|>
  group_by(industry)|>
  mutate(stokes_cut=1000*stokes_cut,
         stokes_growth=stokes_cut/lag(stokes_cut)-1)|>
  nest()|>
  fuzzyjoin::stringdist_join(internal|>
                               ungroup()|>
                               select(industry)|>
                               distinct())

#manually check fuzzy join
internal_vs_stokes_wrong[internal_vs_stokes_wrong$industry.x!=internal_vs_stokes_wrong$industry.y, ]

internal_vs_stokes <-internal_vs_stokes_wrong|>
  rename(industry=industry.y)|>
  ungroup()|>
  select(-industry.x)|>
  unnest(data)|>
  inner_join(internal)

internal_vs_stokes_totals <- internal_vs_stokes|>
  group_by(name)|>
  summarize(stokes_cut=sum(stokes_cut),
            internal=sum(internal),
            stokes_growth=mean(stokes_growth))|>
  mutate(industry="Total")

internal_vs_stokes <- bind_rows(internal_vs_stokes, internal_vs_stokes_totals)

write_rds(internal_vs_stokes, here("out","internal_vs_stokes.rds"))

# CAGR stuff----------------------------------------

cagrs <- internal_vs_stokes|>
  mutate(name=as.numeric(name))|>
  group_by(industry)|>
  nest()|>
  mutate(internal=map(data, get_cagrs, "internal"),
         stokes=map(data, get_cagrs, "stokes_cut")
         )|>
  select(-data)|>
  unnest(internal, names_sep = "_")|>
  unnest(stokes, names_sep = "_")

write_rds(cagrs, here("out","cagrs.rds"))

# comparing to LFS data-----------------------------------

lfs_files <- list.files(here("data"), pattern = "lfsstat4digNAICS")

lfs_data <- vroom::vroom(here("data", lfs_files))|>
  na.omit()|>
  filter(LF_STAT=="Employed")|>
  #calculate the monthly totals to filter out 0s at end of LFS data
  group_by(SYEAR,SMTH)|>
  mutate(total=sum(`_COUNT_`))|>
  filter(total>0)|>
  select(-total)|>
  #done
  inner_join(mapping, by=c("NAICS_5"="naics_5"))|>
  group_by(lmo_detailed_industry, SYEAR, SMTH)|>
  summarise(value=sum(`_COUNT_`, na.rm=TRUE))|>
  mutate(date=ym(paste(SYEAR, SMTH, sep="/")),
         series="LFS Data")|>
  ungroup()|>
  select(-SYEAR,-SMTH)

if(cut=="macro"){
  lfs_data <- inner_join(lfs_data, detailed_to_stokes)|>
    group_by(date, industry=stokes_industry, series)|>
    summarize(value=sum(value))
}else{
  lfs_data <- lfs_data|>
    select(industry=lmo_detailed_industry, value, series, date)
}

lfs_data_totals <- lfs_data|>
  group_by(date, series)|>
  summarize(value=sum(value))|>
  mutate(industry="Total")

lfs_data <- bind_rows(lfs_data, lfs_data_totals)

write_rds(lfs_data, here("out","lfs_data.rds"))

if(cut=="macro") source("02_macro_shares.R")
if(cut=="industry") source("03_industry_shares.R")





