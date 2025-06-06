#' you need to set the cut type:
#cut <- "macro"
cut <- "industry"

#' NOTE: the files that are being compared need to be quite similar:
#' they need to have identical file names (between versions)
#' they need to have the same sheet names (between versions)
#' they need to have the same series names aka row identifiers (between versions)
#' the only thing that should differ is the numeric data.
#' there is a fuzzyjoin around line 160... this should be checked manually.

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
# clean up intermediate objects--------------------
int_obj <- list.files(here("out"), full.names = TRUE)
file.remove(int_obj)
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
source(here("R","functions.R"))
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

#Regional investment sheets (macro cut) a bit wonky...

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
#cut garbage off industry file name---------------------------------
if(cut=="industry"){
  original_tbbl <- original_tbbl|>
    mutate(which_file= word(which_file, sep="-"))
  new_tbbl <- new_tbbl|>
    mutate(which_file= word(which_file, sep="-"))
}

joined <- inner_join(original_tbbl, new_tbbl)|>
  mutate(original_data=map(original_data, make_long, "original"),
         new_data=map(new_data, make_long, "new"),
         joined=map2(new_data, original_data, full_join) #full join because years might not match
         )|>
  select(-new_data, -original_data)|>
  unnest(joined)|>
  mutate(percent_change=symmetric_change(original_value, new_value))|>
  pivot_longer(cols=c("new_value", "original_value"), names_to = "series")|>
  group_by(which_file, sheet, variable, series)|>
  nest()|>
  mutate(cagr=map_dbl(data, get_10_cagr))|>
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
  pivot_longer(cols=-industry, names_to = "when", values_to = "internal")|>
  separate(industry, into=c("code", "industry"), sep=": ")|>
  select(-code)|>
  mutate(when=as.numeric(when))|>
  filter(when>=(max(when)-10)) #only the forecast period

if(cut=="macro"){
  internal <- inner_join(internal, detailed_to_stokes, by=c("industry"="lmo_detailed_industry"))|>
    group_by(when, industry=stokes_industry)|>
    summarize(internal=sum(internal))
}

stokes_cut <- read_excel(here("data",
                              new_folder,
                              list.files(here("data",new_folder), pattern = pattern)),
                         sheet = sheet,
                         skip =skip)

colnames(stokes_cut)[1] <- "industry"

stokes_cut <- stokes_cut|>
  mutate(industry=str_replace_all(industry,", online shopping", ""))

internal_vs_stokes_wrong <- stokes_cut|>
  mutate(across(-industry, ~as.numeric(.)))|>
  pivot_longer(cols=-industry, names_to = "when", values_to = "stokes_cut")|>
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
  mutate(when=as.numeric(when))|>
  filter(when>=(max(when)-10))|> #only the forecast period
  full_join(internal)

internal_vs_stokes_totals <- internal_vs_stokes|>
  group_by(when)|>
  summarize(stokes_cut=sum(stokes_cut),
            internal=sum(internal))|>
  mutate(industry="Total")

internal_vs_stokes <- bind_rows(internal_vs_stokes, internal_vs_stokes_totals)|>
  mutate(percent_change=symmetric_change(stokes_cut, internal))|>
  pivot_longer(cols=c(stokes_cut, internal), names_to = "series", values_to = "value")|>
  group_by(industry, series)|>
  nest()|>
  mutate(data=map(data, no_na_value))|>
  mutate(cagr=map_dbl(data, get_10_cagr))|>
  unnest(data)|>
  mutate(when=ymd(paste(when, "06","01", sep="/")))

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

# LFS data-----------------------------------

rtra_files <- list.files(here("data"), pattern = "lfsstat4digNAICS")

rtra_data <- vroom::vroom(here("data", rtra_files))|>
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
  mutate(when=ym(paste(SYEAR, SMTH, sep="/")),
         series="LFS Data")|>
  ungroup()|>
  select(-SYEAR,-SMTH)

if(cut=="macro"){
  rtra_data <- inner_join(rtra_data, detailed_to_stokes)|>
    group_by(when, industry=stokes_industry, series)|>
    summarize(value=sum(value))
}else{
  rtra_data <- rtra_data|>
    select(industry=lmo_detailed_industry, value, series, when)
}

rtra_data_totals <- rtra_data|>
  group_by(when, series)|>
  summarize(value=sum(value))|>
  mutate(industry="Total")

#add in the trend and cagrs

rtra_data <- bind_rows(rtra_data, rtra_data_totals)|>
  mutate(when=yearmonth(when))|>
  group_by(industry) |>
  nest()|>
  mutate(data=map(data, tsibble, index=when),
         data=map(data, stl_decomp))|>
  unnest(data)|>
  group_by(industry, series) |>
  nest()|>
  mutate(cagr=map_dbl(data, get_10_cagr),
         alpha=if_else(series=="LFS", .25, 1))|>
  unnest(data)|>
  mutate(when=ym(when))

write_rds(rtra_data, here("out","rtra_data.rds"))

# pull code from ...shares.R files into this file---------------------

detailed_to_stokes <- mapping|>
  select(lmo_detailed_industry, stokes_industry)|>
  distinct()

correct_names <- mapping|>
  select(lmo_industry_name=lmo_detailed_industry)|>
  distinct()

file_path <-here("data", "Employment for 64 LMO Industries 2000-2024.xlsx")
sheet_names <- excel_sheets(file_path)[-c(1,5,6)] #could fail

annual_lfs <- tibble(
  bc_region = sheet_names,
  data = map(sheet_names, ~ read_excel(file_path, sheet = .x, skip=3)))|>
  unnest(data)|>
  pivot_longer(cols=starts_with("2"), names_to = "when", values_to = "count")|>
  clean_names()|>
  filter(str_detect(lmo_ind_code, "ind"))|>
  mutate(series="lfs",
         when=as.numeric(when))

if(cut=="macro"){
  annual_lfs <- inner_join(annual_lfs, detailed_to_stokes)|>
    group_by(when, bc_region, industry=stokes_industry, series)|>
    summarize(count=sum(count, na.rm = TRUE))

  lfs_region_names <- annual_lfs|> #LFS has correct naming: use to correct naming in stokes data with fuzzyjoin below
    ungroup()|>
    select(bc_region)|>
    distinct()

  stokes_industries <- mapping|>
    select(stokes_industry)|>
    distinct()|>
    pull()
  stokes_data <- regional_macro_cut("macro_new")

}else{
  annual_lfs <- annual_lfs|>
    select(-lmo_ind_code)|>
    rename(industry=lmo_detailed_industry)

  file_name <- list.files(here("data","industry_new"), pattern = "IndustryEmploymentBC")
  stokes_data <- tibble(sheet=excel_sheets(here("data",
                                                "industry_new",
                                                file_name))[-1])|>
    mutate(data=map(sheet, get_sheet, "industry_new"))|>
    arrange(sheet)|>
    mutate(bc_region=sort(unique(annual_lfs$bc_region)))|>
    select(-sheet)|>
    unnest(data)|>
    mutate(series="new",
           when=as.numeric(when))|>
    filter(when>=max(when)-10)|>
    fuzzyjoin::stringdist_join(correct_names)

  stokes_data[stokes_data$lmo_industry_name.x!=stokes_data$lmo_industry_name.y,c(1,6)]|>distinct()

  stokes_data <- stokes_data|>
    select(-lmo_industry_name.x)|>
    rename(industry=lmo_industry_name.y)
}

all_data <- bind_rows(annual_lfs, stokes_data)

#aggregate the data-----------------------------------------

#by region---------------------------------

by_region <- all_data|>
  group_by(when, bc_region, series)|>
  mutate(share=count/sum(count, na.rm = TRUE))

all_regions <- all_data|>
  group_by(when, industry, series)|>
  summarize(count=sum(count))|>
  group_by(when, series)|>
  mutate(share=count/sum(count, na.rm = TRUE),
         bc_region="British Columbia")

by_region <- bind_rows(all_regions, by_region)|>
  pivot_longer(cols=c("count","share"))

by_region%>%
  split(.$series) %>%
  list2env(envir = .GlobalEnv)

region_shares <- lfs|>
  group_by(bc_region, industry, name) |>
  nest()|>
  mutate(data=map(data, as_tsibble, index=when),
         data=map(data, stl_decomp))|>
  unnest(data)|>
  bind_rows(new)|>
  group_by(industry, bc_region, name, series) |>
  nest()|>
  mutate(cagr=map_dbl(data, get_10_cagr),
         alpha=if_else(series=="LFS", .25, 1))|>
  unnest(data)

#by industry

by_industry <- all_data|>
  group_by(when, industry, series)|>
  mutate(share=count/sum(count, na.rm = TRUE)) #annual regional shares by industry

all_industries <- all_data|>
  group_by(when, bc_region, series)|>
  summarize(count=sum(count))|>
  group_by(when, series)|>
  mutate(share=count/sum(count, na.rm = TRUE),
         industry="All industries")

by_industry <- bind_rows(by_industry, all_industries)|>
  pivot_longer(cols=c("count","share"))

#smooth and add cagrs---------------------------------

by_industry%>%
  split(.$series) %>%
  list2env(envir = .GlobalEnv)

industry_shares <- lfs|>
  group_by(bc_region, industry, name) |>
  nest()|>
  mutate(data=map(data, as_tsibble, index=when),
         data=map(data, stl_decomp))|>
  unnest(data)|>
  bind_rows(new)|>
  group_by(industry, bc_region, name, series) |>
  nest()|>
  mutate(cagr=map_dbl(data, get_10_cagr),
         alpha=if_else(series=="LFS", .25, 1))|>
  unnest(data)

#Sazid regional stuff------------------------

if(cut=="macro"){
  stokes_data_old <- regional_macro_cut("macro_old")|>
    pivot_wider(names_from = series, values_from = count)

  stokes_data_new <- regional_macro_cut("macro_new")|>
    pivot_wider(names_from = series, values_from = count)

  stokes_regional <- full_join(stokes_data_old, stokes_data_new)

  stokes_bc <- stokes_regional|>
    group_by(industry, when)|>
    summarize(new=sum(new),
              old=sum(old))|>
    mutate(bc_region="Subtotals")

  stokes_bc_new <- get_bc_data("macro_new")|>
    pivot_wider(names_from = source, values_from = count)
  stokes_bc_old <- get_bc_data("macro_old")|>
    pivot_wider(names_from = source, values_from = count)
  stokes_bc_total <- full_join(stokes_bc_new, stokes_bc_old)
}else{
  stokes_data_new <- regional_industry_cut("industry_new")
  stokes_data_old <- regional_industry_cut("industry_old")

  stokes_all <- full_join(stokes_data_new, stokes_data_old)|>
    rename(industry=lmo_industry_name,
           bc_region=sheet)

  stokes_bc <- stokes_all|>
    filter(bc_region=="BC")

  stokes_regional <- stokes_all|>
    filter(bc_region!="BC")

  stokes_bc_total <- stokes_regional|>
    group_by(industry, when)|>
    summarize(new=sum(new),
              old=sum(old))|>
    mutate(bc_region="Subtotals")
}

stokes_regional_diff <- bind_rows(stokes_regional, stokes_bc)|>
  bind_rows(stokes_bc_total)|>
  mutate(difference=new-old,
         scaled_difference=log10(abs(difference)+1),
         scaled_difference=if_else(difference>0, scaled_difference, -scaled_difference),
         percent_difference=new/old-1)

if(cut=="macro"){
  stokes_regional_diff <- stokes_regional_diff|>
    filter(bc_region!="British Columbia")|>
    mutate(bc_region=factor(bc_region,
                            ordered = TRUE,
                            levels=c(
                              "BritishColumbiaTables",
                              "Subtotals",
                              "Northeast",
                              "North Coast & Nechako",
                              "Cariboo",
                              "Kootenay",
                              "Thompson-Okanagan",
                              "Vancouver Island And Coast",
                              "Lower Mainland-Southwest")))
} else {
  stokes_regional_diff <- stokes_regional_diff|>
    mutate(bc_region=factor(bc_region,
                     ordered = TRUE,
                     levels=c(
                     "BC",
                     "Subtotals",
                     "NE",
                     "NCN",
                     "CAR",
                     "KOO",
                     "TOK",
                     "VIC",
                     "MSW")
                     )
           )
}
#write to disk------------------------------
write_rds(industry_shares, here("out","industry_shares.rds"))
write_rds(region_shares, here("out","region_shares.rds"))
write_rds(stokes_regional_diff, here("out","stokes_regional_diff.rds"))
