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
get_regional_data <- function(folder_name){
  stokes_regional_files <- list.files(here("data", folder_name), pattern = "9", full.names = TRUE)
  tibble(bc_region=stokes_regional_files)|>
    mutate(data=map(bc_region, read_excel, skip=2, sheet="LabourMarket2", na="NA", col_types=c("text",rep("numeric",25))),
           data=map(data, tidy_up),
           bc_region=unlist(qdapRegex::ex_between(stokes_regional_files, ")", ".")), #filename contains region between ) and .
           bc_region=str_replace_all(bc_region, "&"," and "))|> # necessary for fuzzyjoin below
    unnest(data)|>
    mutate(source=str_split(folder_name, "_")[[1]][[2]])|>
    fuzzyjoin::stringdist_full_join(lfs_region_names, by = "bc_region")|> #stokes has f'd up region names
    ungroup()|>
    rename(bc_region=bc_region.y)|>
    select(-bc_region.x)|>
    na.omit()
}

get_bc_data <- function(folder_name){
  stokes_files <- list.files(here("data", folder_name), pattern = "British", full.names = TRUE)
  tibble(bc_region=stokes_files)|>
    mutate(data=map(bc_region, read_excel, skip=2, sheet="Labour Market", na="NA", col_types=c("text",rep("numeric",25))),
           data=map(data, tidy_up),
           bc_region="BritishColumbiaTables")|>
    unnest(data)|>
    mutate(source=str_split(folder_name, "_")[[1]][[2]])|>
    na.omit()
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

stokes_data <- get_regional_data("macro_new")

all_data <- full_join(lfs_data, stokes_data)
#aggregate the data-----------------------------------------
region_bc <- all_data|>
  group_by(year, industry, source)|>
  summarize(count=sum(count, na.rm = TRUE))|> #annual industry employment (for British Columbia)
  group_by(year, source, .add = FALSE)|> #remove the industry grouping
  mutate(share=count/sum(count, na.rm = TRUE), #annual industry shares (for British Columbia)
         bc_region="British Columbia")

by_region <- all_data|>
  group_by(year, bc_region, .add=FALSE)|>
  mutate(share=count/sum(count, na.rm = TRUE))|> #annual industry shares by region
  full_join(region_bc)

industry_all <- all_data|>
  group_by(year, bc_region, source)|>
  summarize(count=sum(count, na.rm = TRUE))|> #annual regional employment (for all industries)
  group_by(year, source, .add=FALSE)|> #remove the regional grouping
  mutate(share=count/sum(count, na.rm = TRUE), #annual regional shares (for all industries)
         industry="All industries")

by_industry <- all_data|>
  group_by(year, industry)|>
  mutate(share=count/sum(count, na.rm = TRUE))|> #annual regional shares by industry
  full_join(industry_all)

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

stokes_regional_diff <- full_join(stokes_regional, stokes_bc)|>
  full_join(stokes_bc_total)|>
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
                            "North Coast and Nechako",
                            "Cariboo",
                            "Kootenay",
                            "Thompson-Okanagan",
                            "Vancouver Island and Coast",
                            "Lower Mainland-Southwest"
                            )
                          )
         )


#write to disk------------------------------
write_csv(by_industry, here("out","industry_shares.csv"))
write_csv(by_region, here("out","region_shares.csv"))
write_rds(stokes_regional_diff, here("out","stokes_regional_diff.rds"))
