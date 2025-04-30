# code specific to industry cut

correct_names <- read_excel(here("data","industry_mapping_2025_with_stokes_agg.xlsx"))|>
  select(lmo_industry_name=lmo_detailed_industry)|>
  distinct()

file_path <-here("data", "Employment for 64 LMO Industries 2000-2024.xlsx")
sheet_names <- excel_sheets(file_path)[-c(1,5,6)] #could fail

lfs_data <- tibble(
  bc_region = sheet_names,
  data = map(sheet_names, ~ read_excel(file_path, sheet = .x, skip=3)))|>
  unnest(data)|>
  pivot_longer(cols=starts_with("2"), names_to = "when", values_to = "count")|>
  clean_names()|>
  filter(str_detect(lmo_ind_code, "ind"))|>
  mutate(series="lfs",
         when=as.numeric(when))|>
  select(-lmo_ind_code)|>
  rename(lmo_industry_name=lmo_detailed_industry)


file_name <- list.files(here("data","industry_new"), pattern = "IndustryEmploymentBC")
stokes_data <- tibble(sheet=excel_sheets(here("data",
                                              "industry_new",
                                              file_name))[-1])|>
  mutate(data=map(sheet, get_sheet, "industry_new"))|>
  arrange(sheet)|>
  mutate(bc_region=sort(unique(lfs_data$bc_region)))|>
  select(-sheet)|>
  unnest(data)|>
  mutate(series="new")|>
  filter(when>=year(today()))|>
  fuzzyjoin::stringdist_join(correct_names)

stokes_data[stokes_data$lmo_industry_name.x!=stokes_data$lmo_industry_name.y,c(1,6)]|>distinct()

stokes_data <- stokes_data|>
  select(-lmo_industry_name.x)|>
  rename(lmo_industry_name=lmo_industry_name.y)

all_data <- bind_rows(lfs_data, stokes_data)|>
  rename(industry=lmo_industry_name)

#aggregate the data-----------------------------------------
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
  mutate(cagr=map_dbl(data, get_10ish_cagr),
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
  mutate(cagr=map_dbl(data, get_10ish_cagr),
         alpha=if_else(series=="LFS", .25, 1))|>
  unnest(data)

# regional stuff for sazid

stokes_data_new <- regional_industry_cut("industry_new")
stokes_data_old <- regional_industry_cut("industry_old")

stokes_all <- full_join(stokes_data_new, stokes_data_old)|>
  rename(industry=lmo_industry_name,
         bc_region=sheet)

stokes_bc <- stokes_all|>
  filter(bc_region=="BC")

stokes_regional <- stokes_all|>
  filter(bc_region!="BC")

stokes_subtotal <- stokes_regional|>
  group_by(industry, when)|>
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

write_rds(industry_shares, here("out","industry_shares.rds"))
write_rds(region_shares, here("out","region_shares.rds"))
write_rds(stokes_regional_diff, here("out","stokes_regional_diff.rds"))
