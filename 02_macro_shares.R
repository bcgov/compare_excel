library(tidyverse)
library(here)
library(readxl)
#functions--------------------------

tidy_up <- function(tbbl){
  colnames(tbbl)[1] <- "industry"
  tbbl|>
    filter(industry %in% stokes_industries)|>
    pivot_longer(cols=-industry, names_to = "year", values_to = "count")|>
    filter(year>=year(today()))|>
    mutate(count=count*1000,
           year=as.numeric(year))
}

get_share <- function(tbbl){
  tbbl|>
    group_by(year)|>
    mutate(share=count/sum(count, na.rm = TRUE))
}

#the program----------------------

mapping <- read_csv(here("data","lmo64_agg_stokes_mapping.csv"))|>
  select(-aggregate_industry)

stokes_industries <- mapping|>
  select(stokes_industry)|>
  distinct()|>
  pull()

lfs_data <- read_csv(here("data","lfs_emp_by_reg_and_lmo64_long.csv"))|>
  rename(year=syear)|>
  full_join(mapping, by=c("lmo_ind_code","lmo_detailed_industry"="lmo_industry_name"))|>
  group_by(year)|>
  mutate(bc_region = case_when(bc_region=="North Coast" ~ "North Coast and Nechako",#stokes aggregates these regions
                               bc_region=="Nechako" ~ "North Coast and Nechako",
                               TRUE ~ bc_region),
         source="lfs")|>
  group_by(year, bc_region, industry=stokes_industry, source)|>
  summarize(count=sum(count, na.rm = TRUE))

lfs_region_bc <- lfs_data|>
  group_by(year, industry, source)|>
  summarize(count=sum(count, na.rm = TRUE))|>
  mutate(bc_region="British Columbia")|>
  group_by(year, source, bc_region, .add = FALSE)|>
  mutate(share=count/sum(count, na.rm = TRUE))

lfs_region <- lfs_data|>
  group_by(bc_region)|>
  nest()|>
  mutate(data=map(data, get_share))|>
  unnest(data)|>
  full_join(lfs_region_bc)

lfs_industry_all <- lfs_data|>
  group_by(year, bc_region, source)|>
  summarize(count=sum(count, na.rm = TRUE))|>
  mutate(industry="All industries")|>
  group_by(year, source, industry, .add=FALSE)|>
  mutate(share=count/sum(count, na.rm = TRUE))

lfs_industry <- lfs_data|>
  group_by(industry)|>
  nest()|>
  mutate(data=map(data, get_share))|>
  unnest(data)|>
  full_join(lfs_industry_all)

lfs_region_names <- lfs_region|>
  select(bc_region)|>
  distinct()

#get regional data for macro cuts----------------------------

stokes_regional_files <- list.files(here("data","macro_new"), pattern = "9", full.names = TRUE)

stokes <- tibble(bc_region=stokes_regional_files)|>
  mutate(data=map(bc_region, read_excel, skip=2, sheet="LabourMarket2", na="NA", col_types=c("text",rep("numeric",25))),
         data=map(data, tidy_up),
         bc_region=str_match(bc_region, "\\)\\s*(.*?)\\s*\\.")[,2],
         bc_region=str_replace_all(bc_region, "&"," and ")
  )|>
  unnest(data)|>
  mutate(source="stokes")|>
  fuzzyjoin::stringdist_full_join(lfs_region_names, by = "bc_region")|> #stokes has f'd up region names
  ungroup()|>
  rename(bc_region=bc_region.y)|>
  select(-bc_region.x)|>
  na.omit()

stokes_region_bc <- stokes|>
  group_by(year, industry, source)|>
  summarize(count=sum(count, na.rm = TRUE))|>
  mutate(bc_region="British Columbia")|>
  group_by(year, source, bc_region, .add=FALSE)|>
  mutate(share=count/sum(count, na.rm = TRUE))

stokes_region <- stokes|>
  group_by(bc_region)|>
  nest()|>
  mutate(data=map(data, get_share))|>
  unnest(data)|>
  full_join(stokes_region_bc)

stokes_industry_all <- stokes|>
  group_by(year, bc_region, source)|>
  summarize(count=sum(count, na.rm = TRUE))|>
  mutate(industry="All industries")|>
  group_by(year, source, industry, .add=FALSE)|>
  mutate(share=count/sum(count, na.rm = TRUE))

stokes_industry <- stokes|>
  group_by(industry)|>
  nest()|>
  mutate(data=map(data, get_share))|>
  unnest(data)|>
  full_join(stokes_industry_all)

by_industry <- full_join(lfs_industry, stokes_industry)
by_region <- full_join(lfs_region, stokes_region)

write_csv(by_industry, here("out","industry_shares.csv"))
write_csv(by_region, here("out","region_shares.csv"))
