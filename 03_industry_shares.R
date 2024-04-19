library(tidyverse)
library(here)
library(readxl)
#functions--------------------------
share_plot <- function(tbbl, facet_var){
  ggplot(tbbl, aes(year, share, colour=source))+
    geom_line()+
    scale_y_continuous(labels = scales::percent)+
    facet_wrap(~fct_reorder(get(facet_var), share, .desc = TRUE), scales = "free")
}
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
    fuzzyjoin::stringdist_semi_join(mapping, by=c("industry"="lmo_industry_name"))|>
    pivot_longer(cols=-industry, names_to = "year", values_to = "count")|>
    mutate(year=as.numeric(year),
           count=count*1000)
}
#the program----------------------

mapping <- read_csv(here("data","lmo64_agg_stokes_mapping.csv"))|>
  select(-aggregate_industry)

correct_names <- mapping|>
  select(lmo_industry_name)

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
  group_by(year, industry=lmo_detailed_industry, source)|>
  summarize(share=sum(share))

stokes <- tibble(sheet=excel_sheets(here("data",
                                        "industry_new",
                                        "IndustryEmploymentBC.xlsx"))[-1])|>
  mutate(data=map(sheet, get_sheet))|>
  arrange(sheet)|>
  mutate(bc_region=sort(unique(lfs_data$bc_region)))|>
  select(-sheet)|>
  unnest(data)|>
  mutate(source="stokes")|>
  group_by(year)|>
  mutate(share=count/sum(count))|>
  filter(year>=year(today()))

stokes_region <- stokes|>
  group_by(year, bc_region, source)|>
  summarize(share=sum(share))

stokes_industry <- stokes|>
  group_by(year, industry, source)|>
  summarize(share=sum(share))|>
  fuzzyjoin::stringdist_full_join(correct_names, by=c("industry" = "lmo_industry_name"))|>
  ungroup()|>
  select(-industry)|>
  rename(industry=lmo_industry_name)

by_industry <- full_join(lfs_industry, stokes_industry)

by_region <- lfs_region|>
  unnest(data)|>
  full_join(stokes_region)

write_csv(by_industry, here("out","industry_shares.csv"))
write_csv(by_region, here("out","region_shares.csv"))

