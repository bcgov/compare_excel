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
  mutate(share=count/sum(count),
         bc_region=case_when(bc_region=="North Coast" ~ "North Coast and Nechako",#stokes aggregates these regions
                             bc_region=="Nechako" ~ "North Coast and Nechako",
                             TRUE ~ bc_region),
         source="lfs")

lfs_region <- lfs_data|>
  group_by(year, bc_region, source)|>
  summarize(share=sum(share))|>
  group_by(bc_region, .add = FALSE)|>
  nest()

lfs_industry <- lfs_data|>
  group_by(year, industry=stokes_industry, source)|>
  summarize(share=sum(share))

#get regional data for macro cuts----------------------------

stokes_regional_files <- list.files(here("data","macro_new"), pattern = "9", full.names = TRUE)

stokes <- tibble(bc_region=stokes_regional_files)|>
  mutate(data=map(bc_region, read_excel, skip=2, sheet="LabourMarket2", na="NA", col_types=c("text",rep("numeric",24))),
         data=map(data, tidy_up),
         bc_region=str_match(bc_region, "\\)\\s*(.*?)\\s*\\.")[,2],
         bc_region=str_replace_all(bc_region, "&"," and ")
  )|>
  unnest(data)|>
  group_by(year)|>
  mutate(share=count/sum(count),
         source="stokes")

stokes_region <- stokes|>
  group_by(year, bc_region, source)|>
  summarize(share=sum(share))|>
  group_by(bc_region, .add = FALSE)|>
  nest()|>
  fuzzyjoin::stringdist_full_join(lfs_region, by = "bc_region")|> #stokes has f'd up region names
  ungroup()|>
  select(data.x, bc_region=bc_region.y)|>
  unnest(data.x)

stokes_industry <- stokes|>
  group_by(year, industry, source)|>
  summarize(share=sum(share))

by_industry <- full_join(lfs_industry, stokes_industry)

by_region <- lfs_region|>
  unnest(data)|>
  full_join(stokes_region)

write_csv(by_industry, here("out","industry_shares.csv"))
write_csv(by_region, here("out","region_shares.csv"))
