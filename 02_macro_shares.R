#code specific to the macro cut.  Requires running preprocess file first.

detailed_to_stokes <- mapping|>
  select(lmo_detailed_industry, stokes_industry)|>
  distinct()

stokes_industries <- mapping|>
  select(stokes_industry)|>
  distinct()|>
  pull()
#get the data--------------------------------

file_path <-here("data", "Employment for 64 LMO Industries 2000-2024.xlsx")
sheet_names <- excel_sheets(file_path)[-c(1,5,6)] #could fail

annual_lfs <- tibble(
  bc_region = sheet_names,
  data = map(sheet_names, ~ read_excel(file_path, sheet = .x, skip=3)))|>
  unnest(data)|>
  pivot_longer(cols=starts_with("2"), names_to = "when", values_to = "count")|>
  clean_names()|>
  filter(str_detect(lmo_ind_code, "ind"))|>
  inner_join(detailed_to_stokes)|>
  mutate(source="lfs",
         when=as.numeric(when))|>
  group_by(when, bc_region, industry=stokes_industry, source)|>
  summarize(count=sum(count, na.rm = TRUE))

lfs_region_names <- annual_lfs|> #LFS has correct naming: use to correct naming in stokes data with fuzzyjoin below
  ungroup()|>
  select(bc_region)|>
  distinct()

stokes_data <- regional_macro_cut("macro_new")

all_data <- bind_rows(annual_lfs, stokes_data)

#aggregate by region

by_region <- all_data|>
  group_by(when, bc_region, source)|>
  mutate(share=count/sum(count, na.rm = TRUE))

all_regions <- all_data|>
  group_by(when, industry, source)|>
  summarize(count=sum(count))|>
  group_by(when, source)|>
  mutate(share=count/sum(count, na.rm = TRUE),
         bc_region="British Columbia")

by_region <- bind_rows(all_regions, by_region)|>
  pivot_longer(cols=c("count","share"))|>
  rename(series=source)


#smooth and add cagrs---------------------------------

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

# by industry --------------------------------------

by_industry <- all_data|>
  group_by(when, industry, source)|>
  mutate(share=count/sum(count, na.rm = TRUE)) #annual regional shares by industry

all_industries <- all_data|>
  group_by(when, bc_region, source)|>
  summarize(count=sum(count))|>
  group_by(when, source)|>
  mutate(share=count/sum(count, na.rm = TRUE),
         industry="All industries")

by_industry <- bind_rows(by_industry, all_industries)|>
  pivot_longer(cols=c("count","share"))|>
  rename(series=source)

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


#Sazid regional stuff------------------------
stokes_data_old <- regional_macro_cut("macro_old")|>
  pivot_wider(names_from = source, values_from = count)

stokes_regional <- stokes_data|>
  pivot_wider(names_from = source, values_from = count)|>
  full_join(stokes_data_old)

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
