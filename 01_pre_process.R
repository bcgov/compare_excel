#' you need to set the cut type:
cut <- "macro"
#cut <- "industry"

#' NOTE: the files that are being compared need to be quite similar:
#' they need to have identical file names (between versions)
#' they need to have the same sheet names (between versions)
#' they need to have the same series names aka row identifiers (between versions)
#' the only thing that should differ is the numeric data.
#' there is a fuzzyjoin around line 144... this should be checked manually.

#libraries-----------------------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(assertthat)
library(fpp3)
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

symmetric_change <- function(old, new) {
  denom <- (old + new) / 2
  result <- ifelse(denom == 0 & new != old, 200 * sign(new - old),
                   ifelse(denom == 0, 0, (new - old) / denom * 100))
  return(result)
}

stl_decomp <- function(tbbl){
  tbbl |>
    model(STL(value))|>
    components()|>
    tibble()|>
    select(date, LFS=value, `LFS trend`=trend)|>
    pivot_longer(-date, names_to="series", values_to="value")
}

get_monthly_cagr <- function(tbbl){
  end <- tbbl$value[tbbl$date==max(tbbl$date)]
  start <- tbbl$value[ym(tbbl$date)==(ym(max(tbbl$date))-years(10))]
  (end/start)^(1/10)-1
}

get_cagr <- function(tbbl){
  end <- tbbl$value[tbbl$date==max(tbbl$date)]
  start <- tbbl$value[tbbl$date==max(tbbl$date)-years(10)]
  cagr <- (end/start)^(1/10)-1
}

get_cagr2 <- function(tbbl){ #ONLY PASS THE 10 YEAR DATA TO THIS FUNCTION
  tbbl <- tbbl|>
    mutate(year=as.numeric(year))
  end <- tbbl$value[tbbl$year==max(tbbl$year)]
  start <- tbbl$value[tbbl$year==min(tbbl$year)]
  cagr <- (end/start)^(1/(max(tbbl$year)-min(tbbl$year)))-1
}
get_rmse <- function(tbbl){
  sqrt(mean((tbbl$new_value-tbbl$original_value)^2))
}

get_smape <- function(tbbl){
  mean(abs(tbbl$new_value-tbbl$original_value)/(abs(tbbl$new_value)+abs(tbbl$original_value)))
}

get_mean <- function(tbbl){
  (mean(tbbl$new_value)+mean(tbbl$original_value))/2
}

clean_it <- function(tbbl){
  tbbl|>
    filter(year>=year(today()))|>
    na.omit()
}

skip_meta <- function(path_to_file, sheet=1, meta_less_than=100, cutoff=.5){
  extension <- tools::file_ext(path_to_file)
  reader <- switch(extension,
                   "csv" = readr::read_csv,
                   "xlsx" = readxl::read_excel,
                   "xls" = readxl::read_excel,
                   stop("file needs to be either csv or excel")
  )
  temp <- reader(path_to_file, sheet, n_max = meta_less_than, col_names = FALSE) #read in first meta_less_than lines
  skip <- which(rowSums(is.na(temp))<cutoff*ncol(temp))[1]-1 #which last mostly empty row?
  reader(path_to_file, sheet, skip=skip, col_types="text")
}

correct_typos <- function(tbbl){
  colnames(tbbl)[1] <- "variable" #first column missing name
  tbbl|>
    dplyr::mutate(variable=stringr::str_replace_all(variable, "MSW", "VIC"),
                  variable=stringr::str_replace_all(variable, "TOK", "VIC"),
                  variable=stringr::str_replace_all(variable, "KOO", "VIC"),
                  variable=stringr::str_replace_all(variable, "CAR","VIC"),
                  variable=stringr::str_replace_all(variable, "NCN", "VIC"),
                  variable=stringr::str_replace_all(variable, "NE", "VIC"))
}

add_category <- function(tbbl){
  tbbl|>
    dplyr::mutate(category=grepl("^[A-Z &,\\-]+$", variable), #is value of variable in all caps?
                  category=dplyr::if_else(category, variable, NA_character_), .before=variable)|> #if in all caps, category=variable, else NA
    tidyr::fill(category, .direction = "down")|> #fill all the NAs downwards
    na.omit()|>
    mutate(category=str_replace_all(category, "BUSIVICSS", "BUSINESS"),
           variable=str_replace_all(variable, "BUSIVICSS", "BUSINESS"),
    )|>
    mutate(across(starts_with("2"), as.numeric))|>
    unite(variable, category, variable, sep = ": ")
 }

read_sheet <- function(which_file, sheet, sub_directory){
  tbbl <- read_excel(path=here("data", sub_directory, which_file),
                     sheet=sheet,
                     skip = skip,
                     na = "NA",
                     col_types= "text")

  colnames(tbbl)[1] <- "variable" #missing name of the series identifier

  tbbl[-1] <- lapply(tbbl[-1], as.numeric) #convert 2nd--last column to numeric
  tbbl
}

make_long <- function(tbbl, prepend){
  tbbl|>
    mutate(variable=paste0((row_number()+add_rows),": ", variable))|> #number the rows
    filter(!str_detect(variable, "%"))|>
    pivot_longer(cols = -variable,
                 names_to = "year",
                 values_to = paste(prepend, "value", sep="_"))
}

get_sheets <- function(path){
  tibble(sheet=excel_sheets(path))
}

get_cagrs <- function(tbbl, column){
  val_now <- tbbl[[column]][tbbl$date==(max(tbbl$date)-years(10))]
  val_fyfn <- tbbl[[column]][tbbl$date==(max(tbbl$date)-years(5))]
  val_tyfn <- tbbl[[column]][tbbl$date==max(tbbl$date)]
  cagr_ffy <- ((val_fyfn/val_now)^.2-1)
  cagr_sfy <- ((val_tyfn/val_fyfn)^.2-1)
  cagr_ty <- ((val_tyfn/val_now)^.1-1)
  tibble(cagr_ffy=cagr_ffy, cagr_sfy=cagr_sfy,cagr_ty=cagr_ty)
}

# the program-------------------------------------

#comparison between the cuts-----------------------------------------

original_tbbl <- tibble(which_file=list.files(here("data", old_folder)),
                        path=here("data", old_folder, which_file))|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  filter(!(sheet=="Investment" & which_file != "BritishColumbiaTables.xlsx"))|> #regional investment wonky (macro cut)
  mutate(original_data=map2(which_file, sheet, read_sheet, old_folder))|>
  select(-path)

new_tbbl <- tibble(which_file=list.files(here("data", new_folder)),
                   path=here("data", new_folder, which_file))|>
  mutate(sheet=map(path, get_sheets))|>
  unnest(sheet)|>
  filter(!(sheet=="Investment" & which_file != "BritishColumbiaTables.xlsx"))|> #regional investment wonky (macro cut)
  mutate(new_data=map2(which_file, sheet, read_sheet, new_folder))|>
  select(-path)

#Regional investment sheets a bit wonky...

if(cut=="macro"){

ri_files <- list.files(here("data",
                            old_folder))[str_detect(list.files(here("data",
                                                                   old_folder)),
                                                   "British",
                                                   negate = TRUE)]

ri_original <- tibble(which_file=ri_files,
                      path=here("data", old_folder, which_file))|>
  mutate(sheet="Investment",
         original_data=map(path, skip_meta, sheet="Investment"),
         original_data=map(original_data, correct_typos),
         original_data=map(original_data, add_category))|>
  select(-path)

original_tbbl <- bind_rows(original_tbbl, ri_original)

ri_new <- tibble(which_file=ri_files,
                      path=here("data", new_folder, which_file))|>
  mutate(sheet="Investment",
         new_data=map(path, skip_meta, sheet="Investment"),
         new_data=map(new_data, correct_typos),
         new_data=map(new_data, add_category))|>
  select(-path)

new_tbbl <- bind_rows(new_tbbl, ri_new)
}

if(cut=="industry"){
  original_tbbl <- original_tbbl|>
    mutate(which_file= word(which_file, sep="-"))
  new_tbbl <- new_tbbl|>
    mutate(which_file= word(which_file, sep="-"))
}

joined <- inner_join(original_tbbl, new_tbbl)|>
  mutate(original_data=map(original_data, make_long, "original"),
         new_data=map(new_data, make_long, "new"))|>
  mutate(joined=map2(new_data, original_data, inner_join),
         joined=map(joined, clean_it))|>
  select(-new_data, -original_data)|>
  unnest(joined)|>
  mutate(percent_change=symmetric_change(original_value, new_value))|>
  pivot_longer(cols=c("new_value", "original_value"), names_to = "series")|>
  group_by(which_file, sheet, variable, series)|>
  nest()|>
  mutate(cagr=map_dbl(data, get_cagr2))|> #note that this assumes only passing data you need.
  unnest(data)

write_rds(joined, here("out", "joined.rds"))

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
  mutate(stokes_cut=1000*stokes_cut)|>
  nest()|>
  fuzzyjoin::stringdist_join(internal|>
                               ungroup()|>
                               select(industry)|>
                               distinct())

#manually check fuzzy join
internal_vs_stokes_wrong[internal_vs_stokes_wrong$industry.x!=internal_vs_stokes_wrong$industry.y, c(1,3)]

internal_vs_stokes <-internal_vs_stokes_wrong|>
  rename(industry=industry.y)|>
  ungroup()|>
  select(-industry.x)|>
  unnest(data)|>
  inner_join(internal)

internal_vs_stokes_totals <- internal_vs_stokes|>
  group_by(name)|>
  summarize(stokes_cut=sum(stokes_cut),
            internal=sum(internal))|>
  mutate(industry="Total")

internal_vs_stokes <- bind_rows(internal_vs_stokes, internal_vs_stokes_totals)|>
  mutate(name=as.numeric(name))|>
  filter(name>=(max(name)-10))|>
  mutate(name=as.numeric(name))|>
  filter(name>=(max(name)-10))|> #only the forecast period
  mutate(percent_change=symmetric_change(stokes_cut, internal))|>
  pivot_longer(cols=c(stokes_cut, internal), names_to = "series", values_to = "value")|>
  mutate(date=ymd(paste(name, "06","01", sep="/")))|>
  select(-name)|>
  group_by(industry, series)|>
  nest()|>
  mutate(cagr=map_dbl(data, get_cagr))|>
  unnest(data)

write_rds(internal_vs_stokes, here("out","internal_vs_stokes.rds"))

# CAGR stuff----------------------------------------

cagrs <- internal_vs_stokes|>
  select(-cagr)|>
  pivot_wider(names_from = series, values_from = value)|>
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
  #zeros gone
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

#add in the trend and cagrs

lfs_data <- bind_rows(lfs_data, lfs_data_totals)|>
  mutate(date=yearmonth(date))|>
  group_by(industry) |>
  nest()|>
  mutate(data=map(data, tsibble, index=date),
         data=map(data, stl_decomp))|>
  unnest(data)|>
  group_by(industry, series) |>
  nest()|>
  mutate(cagr=map_dbl(data, get_monthly_cagr),
         alpha=if_else(series=="LFS", .25, 1))|>
  unnest(data)|>
  mutate(date=ym(date))

write_rds(lfs_data, here("out","lfs_data.rds"))

if(cut=="macro") source("02_macro_shares.R")
if(cut=="industry") source("03_industry_shares.R")





