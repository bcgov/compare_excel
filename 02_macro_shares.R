library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(fpp3)
library(conflicted)
conflicts_prefer(dplyr::filter)
#functions--------------------------
stl_decomp <- function(tbbl, var){
  tbbl |>
    model(STL({{  var  }}))|>
    components()|>
    tibble()|>
    select(year, LFS={{  var  }}, `LFS trend`=trend)|>
    pivot_longer(-year, names_to="series", values_to="value")
}

get_cagr <- function(tbbl, var){
  end <- tbbl|>
    filter(year==max(year)) |>
    pull({{ var }})
  start <- tbbl|>
    filter(year==max(year)-10) |>
    pull({{ var }})
  cagr <- (end/start)^(1/10)-1
}

tidy_up <- function(tbbl){
  colnames(tbbl)[1] <- "industry"
  tbbl|>
    filter(industry %in% stokes_industries)|>
    pivot_longer(cols=-industry, names_to = "year", values_to = "count")|>
    filter(year>=year(today()))|>
    mutate(count=as.numeric(count),
           count=count*1000,
           year=as.numeric(year))
}
get_regional_data <- function(folder_name){
  stokes_regional_files <- list.files(here("data", folder_name), pattern = "9", full.names = TRUE)
  tibble(bc_region=stokes_regional_files)|>
    mutate(data=map(bc_region, read_excel, skip=2, sheet="LabourMarket2", na="NA", col_types="text"),
           data=map(data, tidy_up),
           bc_region=unlist(qdapRegex::ex_between(stokes_regional_files, ")", ".")), #filename contains region between ) and .
           bc_region=str_replace_all(bc_region, "&"," and "))|>
    fuzzyjoin::stringdist_full_join(lfs_region_names, by = "bc_region", max_dist=4)|>
    unnest(data)|>
    mutate(source=str_split(folder_name, "_")[[1]][[2]])|>
    ungroup()|>
    rename(bc_region=bc_region.y)|>
    select(-bc_region.x)|>
    na.omit()
}

get_bc_data <- function(folder_name){
  stokes_files <- list.files(here("data", folder_name), pattern = "British", full.names = TRUE)
  tibble(bc_region=stokes_files)|>
    mutate(data=map(bc_region, read_excel, skip=2, sheet="Labour Market", na="NA", col_types="text"),
           data=map(data, tidy_up),
           bc_region="BritishColumbiaTables")|>
    unnest(data)|>
    mutate(source=str_split(folder_name, "_")[[1]][[2]])|>
    na.omit()
}

#the program----------------------
mapping <- read_excel(here("data","industry_mapping_2025_with_stokes_agg.xlsx"))

detailed_to_stokes <- mapping|>
  select(lmo_detailed_industry, stokes_industry)|>
  distinct()

stokes_industries <- mapping|> #the names of the stokes industries as a vector
  select(stokes_industry)|>
  distinct()|>
  pull()
#get the data--------------------------------

file_path <-here("data", "Employment for 64 LMO Industries 2000-2024.xlsx")
sheet_names <- excel_sheets(file_path)[-c(1,5,6)] #could fail

lfs_data <- tibble(
  bc_region = sheet_names,
  data = map(sheet_names, ~ read_excel(file_path, sheet = .x, skip=3)))|>
  unnest(data)|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "count")|>
  clean_names()|>
  filter(str_detect(lmo_ind_code, "ind"))|>
  inner_join(detailed_to_stokes)|>
  mutate(source="lfs",
         year=as.numeric(year))|>
  group_by(year, bc_region, industry=stokes_industry, source)|>
  summarize(count=sum(count, na.rm = TRUE))

lfs_region_names <- lfs_data|> #LFS has correct naming: use to correct naming in stokes data with fuzzyjoin below
  ungroup()|>
  select(bc_region)|>
  distinct()

stokes_data <- get_regional_data("macro_new")

all_data <- bind_rows(lfs_data, stokes_data)

#aggregate

by_region <- all_data|>
  group_by(year, bc_region, source)|>
  mutate(share=count/sum(count, na.rm = TRUE))

all_regions <- all_data|>
  group_by(year, industry, source)|>
  summarize(count=sum(count))|>
  group_by(year, source)|>
  mutate(share=count/sum(count, na.rm = TRUE),
         bc_region="British Columbia")

by_region <- bind_rows(all_regions, by_region)

#smooth and add cagrs---------------------------------

by_region%>%
  split(.$source) %>%
  list2env(envir = .GlobalEnv)

new <- new|>
  mutate(series="Stokes")|>
  ungroup()|>
  select(-source)

lfs <- lfs|>
  group_by(bc_region, industry) |>
  nest()|>
  mutate(data=map(data, as_tsibble, index=year),
         counts=map(data, stl_decomp, count),
         shares=map(data, stl_decomp, share))|>
  select(industry, bc_region, shares, counts)

region_shares <- lfs|>
  unnest(c(shares, counts), names_sep = "_")|>
  select(year=shares_year, industry, bc_region, series= shares_series, share=shares_value, count= counts_value)|>
  bind_rows(new)|>
  group_by(industry, bc_region, series) |>
  nest()|>
  mutate(count_cagr=map_dbl(data, get_cagr, "count"),
         share_cagr=map_dbl(data, get_cagr, "share"),
         alpha=if_else(series=="LFS", .25, 1))|>
  unnest(data)

# by industry --------------------------------------

by_industry <- all_data|>
  group_by(year, industry, source)|>
  mutate(share=count/sum(count, na.rm = TRUE)) #annual regional shares by industry

all_industries <- all_data|>
  group_by(year, bc_region, source)|>
  summarize(count=sum(count))|>
  group_by(year, source)|>
  mutate(share=count/sum(count, na.rm = TRUE),
         industry="All industries")

by_industry <- bind_rows(by_industry, all_industries)

#smooth and add cagrs---------------------------------

by_industry%>%
  split(.$source) %>%
  list2env(envir = .GlobalEnv)

new <- new|>
  mutate(series="Stokes")|>
  ungroup()|>
  select(-source)

lfs <- lfs|>
  group_by(bc_region, industry) |>
  nest()|>
  mutate(data=map(data, as_tsibble, index=year),
         counts=map(data, stl_decomp, count),
         shares=map(data, stl_decomp, share))|>
  select(industry, bc_region, shares, counts)

industry_shares <- lfs|>
  unnest(c(shares, counts), names_sep = "_")|>
  select(year=shares_year, industry, bc_region, series= shares_series, share=shares_value, count= counts_value)|>
  bind_rows(new)|>
  group_by(industry, bc_region, series) |>
  nest()|>
  mutate(count_cagr=map_dbl(data, get_cagr, "count"),
         share_cagr=map_dbl(data, get_cagr, "share"),
         alpha=if_else(series=="LFS", .25, 1))|>
  unnest(data)

#Sazid regional stuff------------------------
stokes_data_old <- get_regional_data("macro_old")|>
  pivot_wider(names_from = source, values_from = count)

stokes_regional <- stokes_data|>
  pivot_wider(names_from = source, values_from = count)|>
  full_join(stokes_data_old)

stokes_bc <- stokes_regional|>
  group_by(industry, year)|>
  summarize(new=sum(new),
            old=sum(old))|>
  mutate(bc_region="Subtotals")

stokes_bc_new <- get_bc_data("macro_new")|>
  pivot_wider(names_from = source, values_from = count)
stokes_bc_old <- get_bc_data("macro_old")|>
  pivot_wider(names_from = source, values_from = count)
stokes_bc_total <- full_join(stokes_bc_new, stokes_bc_old)

stokes_regional_diff <- bind_rows(stokes_regional, stokes_bc)|>
  bind_rows(stokes_bc_total)|>
  mutate(difference=new-old,
         scaled_difference=log10(abs(difference)+1),
         scaled_difference=if_else(difference>0, scaled_difference, -scaled_difference),
         percent_difference=new/old-1,
         bc_region=factor(bc_region,
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
                            "Lower Mainland-Southwest"
                            )
                          )
         )


#write to disk------------------------------
write_rds(industry_shares, here("out","industry_shares.rds"))
write_rds(region_shares, here("out","region_shares.rds"))
write_rds(stokes_regional_diff, here("out","stokes_regional_diff.rds"))
