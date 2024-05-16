library(tidyverse)
library(here)
library(readxl)
#functions--------------------------
get_sheet <- function(sheet, folder_name, numeric_columns){
  file <- list.files(here("data",folder_name), pattern = "IndustryEmploymentBC")
  tbbl <- read_excel(here("data",
                          folder_name,
                          file),
                     sheet=sheet,
                     skip=1,
                     na="NA",
                     col_types = c("text", rep("numeric", numeric_columns)))|>
    janitor::remove_empty("rows")
  colnames(tbbl)[1] <- "lmo_industry_name"
  tbbl|>
    fuzzyjoin::stringdist_semi_join(correct_names)|>
    pivot_longer(cols=-lmo_industry_name, names_to = "year", values_to = "count")|>
    mutate(year=as.numeric(year),
           count=count*1000)
}
get_regional_data <- function(folder_name, numeric_columns){
  file <- list.files(here("data",folder_name), pattern = "IndustryEmploymentBC")
  tibble(sheet=excel_sheets(here("data",
                                 folder_name,
                                 file)))|>
    mutate(data=map(sheet, get_sheet, folder_name, numeric_columns))|>
    unnest(data)|>
    mutate(source=str_split(folder_name, "_")[[1]][[2]])|>
    filter(year>=year(today()))|>
    fuzzyjoin::stringdist_join(correct_names)|>
    select(-lmo_industry_name.x)|>
    rename(lmo_industry_name=lmo_industry_name.y)|>
    pivot_wider(names_from = source, values_from = count)
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
  rename(lmo_industry_name=lmo_detailed_industry)|>
  select(-lmo_ind_code)|>
  group_by(year, bc_region, lmo_industry_name, source)|>
  summarize(count=sum(count, na.rm = TRUE))

file <- list.files(here("data","industry_new"), pattern = "IndustryEmploymentBC")
stokes_data <- tibble(sheet=excel_sheets(here("data",
                                              "industry_new",
                                              file))[-1])|>
  mutate(data=map(sheet, get_sheet, "industry_new", numeric_columns))|>
  arrange(sheet)|>
  mutate(bc_region=sort(unique(lfs_data$bc_region)))|>
  select(-sheet)|>
  unnest(data)|>
  mutate(source="stokes")|>
  filter(year>=year(today()))|>
  fuzzyjoin::stringdist_join(correct_names)|>
  select(-lmo_industry_name.x)|>
  rename(lmo_industry_name=lmo_industry_name.y)

all_data <- full_join(lfs_data, stokes_data)|>
  rename(industry=lmo_industry_name)

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

# regional stuff for sazid

stokes_data_new <- get_regional_data("industry_new", numeric_columns)
stokes_data_old <- get_regional_data("industry_old", numeric_columns=22)

stokes_all <- full_join(stokes_data_new, stokes_data_old)|>
  rename(industry=lmo_industry_name,
         bc_region=sheet)

stokes_bc <- stokes_all|>
  filter(bc_region=="BC")

stokes_regional <- stokes_all|>
  filter(bc_region!="BC")

stokes_subtotal <- stokes_regional|>
  group_by(industry, year)|>
  summarize(new=sum(new),
            old=sum(old))|>
  mutate(bc_region="Subtotals")

stokes_regional_diff <- full_join(stokes_regional, stokes_bc)|>
  full_join(stokes_subtotal)|>
  mutate(difference=new-old,
         scaled_difference=log10(abs(difference)+1),
         scaled_difference=if_else(difference>0, scaled_difference, -scaled_difference),
         percent_difference=new/old-1,
         bc_region=factor(bc_region,
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
                            "MSW"
                            )
                          )
         )


#write to disk------------------------

write_csv(by_industry, here("out","industry_shares.csv"))
write_csv(by_region, here("out","region_shares.csv"))
write_rds(stokes_regional_diff, here("out","stokes_regional_diff.rds"))
