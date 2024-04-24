library(tidyverse)
library(here)
library(readxl)
#functions--------------------------
get_sheet <- function(sheet){
  tbbl <- read_excel(here("data",
                          "industry_new",
                          "IndustryEmploymentBC.xlsx"),
                     sheet=sheet,
                     skip=1,
                     na="NA",
                     col_types = c("text", rep("numeric", 22)))|>
    janitor::remove_empty("rows")
  colnames(tbbl)[1] <- "industry"
  tbbl|>
    fuzzyjoin::stringdist_semi_join(correct_names, by=c("industry" = "lmo_industry_name"))|>
    pivot_longer(cols=-industry, names_to = "year", values_to = "count")|>
    mutate(year=as.numeric(year),
           count=count*1000)
}
#the program----------------------

correct_names <- read_csv(here("data","lmo64_agg_stokes_mapping.csv"))|>
  select(lmo_industry_name)

lfs_data <- read_csv(here("data","lfs_emp_by_reg_and_lmo64_long.csv"))|>
  rename(year=syear)|>
  mutate(bc_region = case_when(bc_region=="North Coast" ~ "North Coast and Nechako",#stokes aggregates these regions
                               bc_region=="Nechako" ~ "North Coast and Nechako",
                               TRUE ~ bc_region),
         source="lfs")|>
  rename(industry=lmo_detailed_industry)|>
  select(-lmo_ind_code)

stokes_data <- tibble(sheet=excel_sheets(here("data",
                                              "industry_new",
                                              "IndustryEmploymentBC.xlsx"))[-1])|>
  mutate(data=map(sheet, get_sheet))|>
  arrange(sheet)|>
  mutate(bc_region=sort(unique(lfs_data$bc_region)))|>
  select(-sheet)|>
  unnest(data)|>
  mutate(source="stokes")|>
  filter(year>=year(today()))

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

write_csv(by_industry, here("out","industry_shares.csv"))
write_csv(by_region, here("out","region_shares.csv"))

