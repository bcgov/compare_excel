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
  select(-aggregate_industry) #do not need lmo aggregate industry

stokes_industries <- mapping|> #the names of the stokes industries as a vector
  select(stokes_industry)|>
  distinct()|>
  pull()
#get the data--------------------------------
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

lfs_region_names <- lfs_data|> #LFS has correct naming: use to correct naming in stokes data with fuzzyjoin below
  ungroup()|>
  select(bc_region)|>
  distinct()

stokes_regional_files <- list.files(here("data","macro_new"), pattern = "9", full.names = TRUE)

stokes_data <- tibble(bc_region=stokes_regional_files)|>
  mutate(data=map(bc_region, read_excel, skip=2, sheet="LabourMarket2", na="NA", col_types=c("text",rep("numeric",25))),
         data=map(data, tidy_up),
         bc_region=unlist(qdapRegex::ex_between(stokes_regional_files, ")", ".")), #filename contains region between ) and .
         bc_region=str_replace_all(bc_region, "&"," and "))|> # necessary for fuzzyjoin below
  unnest(data)|>
  mutate(source="stokes")|>
  fuzzyjoin::stringdist_full_join(lfs_region_names, by = "bc_region")|> #stokes has f'd up region names
  ungroup()|>
  rename(bc_region=bc_region.y)|>
  select(-bc_region.x)|>
  na.omit()

all_data <- full_join(lfs_data, stokes_data)
#aggregate the data-----------------------------------------
region_bc <- all_data|>
  group_by(year, industry, source)|>
  summarize(count=sum(count, na.rm = TRUE))|> #BC employment by year and industry (aggregated across regions)
  group_by(year, source, .add = FALSE)|>
  mutate(share=count/sum(count, na.rm = TRUE), #industry shares by year
         bc_region="British Columbia")

by_region <- all_data|>
  group_by(year, bc_region)|>
  mutate(share=count/sum(count, na.rm = TRUE))|>
  full_join(region_bc)

industry_all <- all_data|>
  group_by(year, bc_region, source)|> #BC employment by year and region (aggregated across industries)
  summarize(count=sum(count, na.rm = TRUE))|>
  group_by(year, source, .add=FALSE)|>
  mutate(share=count/sum(count, na.rm = TRUE), #region shares by year
         industry="All industries")

by_industry <- all_data|>
  group_by(year, industry)|>
  mutate(share=count/sum(count, na.rm = TRUE))|>
  full_join(industry_all)
#write to disk------------------------------
write_csv(by_industry, here("out","industry_shares.csv"))
write_csv(by_region, here("out","region_shares.csv"))
