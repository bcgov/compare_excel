symmetric_change <- function(old, new) {
  denom <- (old + new) / 2
  result <- ifelse(denom == 0 & new != old, 200 * sign(new - old),
                   ifelse(denom == 0, 0, (new - old) / denom * 100))
  return(result)
}

stl_decomp <- function(tbbl){
  tbbl |>
    model(STL(value))|>
    components()|>
    tibble()|>
    select(when, LFS=value, `LFS trend`=trend)|>
    pivot_longer(-when, names_to="series", values_to="value")
}

# get_cagr_yearmonth <- function(tbbl){
#   end <- tbbl$value[tbbl$when==max(tbbl$when)]
#   start <- tbbl$value[ym(tbbl$when)==(ym(max(tbbl$when))-years(10))]
#   cagr <- (end/start)^(1/10)-1
# }

get_10ish_cagr <- function(tbbl){
  when_formats <- c("Date", "yearmonth", "character", "numeric")
  assertthat::assert_that("when" %in% names(tbbl), msg = "function requires a column called 'when'")
  assertthat::assert_that("value" %in% names(tbbl), msg = "function requires a column called 'value'")
  assertthat::assert_that(nrow(tbbl)>0, msg = "function requires a non-empty data frame")
  assertthat::assert_that(class(tbbl$when)[1] %in% when_formats, msg = "'when' column must be of class Date, yearmonth, character or numeric")
  assertthat::assert_that(class(tbbl$value)[1] %in% c("numeric", "integer"), msg = "'value' column must be of class numeric or integer")
  when_type <- class(tbbl$when)[1]
  switch(when_type,
         "Date" = tbbl <- tbbl,
         "yearmonth" = tbbl <- tbbl |> mutate(when = ym(when)),
         "character" = tbbl <- tbbl |> mutate(when = ymd(paste(when,"06", "01", sep = "/"))),
         "numeric" = tbbl <- tbbl |> mutate(when = ymd(paste(when,"06", "01", sep = "/")))
         )
  span <- lubridate::interval(min(tbbl$when), max(tbbl$when))/years(1)
  if(span>=10){
    end <- tbbl$value[tbbl$when==max(tbbl$when)]
    start <- tbbl$value[tbbl$when==max(tbbl$when)-years(10)]
    elapsed <- 10
    (end/start)^(1/elapsed)-1
  } else if (span>8){
    end <- tbbl$value[tbbl$when==max(tbbl$when)]
    start <- tbbl$value[tbbl$when==min(tbbl$when)]
    elapsed <- span
    (end/start)^(1/elapsed)-1
  } else {
    stop("Not enough data to calculate a 10ish year CAGR")
  }
}

# get_cagr_year <- function(tbbl){#sometimes only 9 years available.
#  # browser()
#   tbbl <- tbbl|>
#     mutate(when=as.numeric(when))
#   span <- max(tbbl$when) - min(tbbl$when)
#   if(span>=10){
#     end <- tbbl$value[tbbl$when==max(tbbl$when)]
#     start <- tbbl$value[tbbl$when==max(tbbl$when)-10]
#     elapsed <- 10
#   } else {
#     end <- tbbl$value[tbbl$when==max(tbbl$when)]
#     start <- tbbl$value[tbbl$when==min(tbbl$when)]
#     elapsed <- max(tbbl$when) - min(tbbl$when)
#   }
#   (end/start)^(1/elapsed)-1
# }

get_cagrs <- function(tbbl, column){
  val_now <- tbbl[[column]][tbbl$when==(max(tbbl$when)-years(10))]
  val_fyfn <- tbbl[[column]][tbbl$when==(max(tbbl$when)-years(5))]
  val_tyfn <- tbbl[[column]][tbbl$when==max(tbbl$when)]
  cagr_ffy <- ((val_fyfn/val_now)^.2-1)
  cagr_sfy <- ((val_tyfn/val_fyfn)^.2-1)
  cagr_ty <- ((val_tyfn/val_now)^.1-1)
  tibble(cagr_ffy=cagr_ffy, cagr_sfy=cagr_sfy,cagr_ty=cagr_ty)
}


get_rmse <- function(tbbl){
  sqrt(mean((tbbl$new_value-tbbl$original_value)^2))
}

get_smape <- function(tbbl){
  mean(abs(tbbl$new_value-tbbl$original_value)/(abs(tbbl$new_value)+abs(tbbl$original_value)))
}

get_mean <- function(tbbl){
  (mean(tbbl$new_value)+mean(tbbl$original_value))/2
}

clean_it <- function(tbbl){
  tbbl|>
    filter(when>=year(today()))|>
    na.omit()
}

tidy_up <- function(tbbl){
  colnames(tbbl)[1] <- "industry"
  tbbl|>
    filter(industry %in% stokes_industries)|>
    pivot_longer(cols=-industry, names_to = "when", values_to = "count")|>
    filter(when>=year(today()))|>
    mutate(count=as.numeric(count),
           count=count*1000,
           when=as.numeric(when))
}

skip_meta <- function(path_to_file, sheet=1, meta_less_than=100, cutoff=.5){
  extension <- tools::file_ext(path_to_file)
  reader <- switch(extension,
                   "csv" = readr::read_csv,
                   "xlsx" = readxl::read_excel,
                   "xls" = readxl::read_excel,
                   stop("file needs to be either csv or excel")
  )
  temp <- reader(path_to_file, sheet, n_max = meta_less_than, col_names = FALSE) #read in first meta_less_than lines
  skip <- which(rowSums(is.na(temp))<cutoff*ncol(temp))[1]-1 #which last mostly empty row?
  reader(path_to_file, sheet, skip=skip, col_types="text")
}

correct_typos <- function(tbbl){
  colnames(tbbl)[1] <- "variable" #first column missing name
  tbbl|>
    dplyr::mutate(variable=stringr::str_replace_all(variable, "MSW", "VIC"),
                  variable=stringr::str_replace_all(variable, "TOK", "VIC"),
                  variable=stringr::str_replace_all(variable, "KOO", "VIC"),
                  variable=stringr::str_replace_all(variable, "CAR","VIC"),
                  variable=stringr::str_replace_all(variable, "NCN", "VIC"),
                  variable=stringr::str_replace_all(variable, "NE", "VIC"))
}

add_category <- function(tbbl){
  tbbl|>
    dplyr::mutate(category=grepl("^[A-Z &,\\-]+$", variable), #is value of variable in all caps?
                  category=dplyr::if_else(category, variable, NA_character_), .before=variable)|> #if in all caps, category=variable, else NA
    tidyr::fill(category, .direction = "down")|> #fill all the NAs downwards
    na.omit()|>
    mutate(category=str_replace_all(category, "BUSIVICSS", "BUSINESS"),
           variable=str_replace_all(variable, "BUSIVICSS", "BUSINESS"),
    )|>
    mutate(across(starts_with("2"), as.numeric))|>
    unite(variable, category, variable, sep = ": ")
}

read_sheet <- function(which_file, sheet, sub_directory){
  tbbl <- read_excel(path=here("data", sub_directory, which_file),
                     sheet=sheet,
                     skip = skip,
                     na = "NA",
                     col_types= "text")

  colnames(tbbl)[1] <- "variable" #missing name of the series identifier

  tbbl[-1] <- lapply(tbbl[-1], as.numeric) #convert 2nd--last column to numeric
  tbbl
}

make_long <- function(tbbl, prepend){
  tbbl|>
    mutate(variable=paste0((row_number()+add_rows),": ", variable))|> #number the rows
    filter(!str_detect(variable, "%"))|>
    pivot_longer(cols = -variable,
                 names_to = "when",
                 values_to = paste(prepend, "value", sep="_"))
}

get_sheets <- function(path){
  tibble(sheet=excel_sheets(path))
}

get_sheet <- function(sheet, folder_name){
  file <- list.files(here("data",folder_name), pattern = "IndustryEmploymentBC")

  tbbl <- read_excel(here("data",
                          folder_name,
                          file),
                     sheet=sheet,
                     skip=1,
                     na="NA",
                     col_types = "text")|>
    janitor::remove_empty("rows")
  colnames(tbbl)[1] <- "lmo_industry_name"

  tbbl[-1] <- lapply(tbbl[-1], as.numeric) #convert 2nd--last column to numeric

  tbbl <- tbbl|>
    mutate(lmo_industry_name=str_replace_all(lmo_industry_name,
                                    ", online shopping",
                                    ""), .after=lmo_industry_name)|>
    fuzzyjoin::stringdist_semi_join(correct_names)|>
    pivot_longer(cols=-lmo_industry_name, names_to = "when", values_to = "count")|>
    mutate(when=as.numeric(when),
           count=count*1000)
}

regional_industry_cut <- function(folder_name){
  file <- list.files(here("data",folder_name), pattern = "IndustryEmploymentBC")
  tibble(sheet=excel_sheets(here("data",
                                 folder_name,
                                 file)))|>
    mutate(data=map(sheet, get_sheet, folder_name))|>
    unnest(data)|>
    mutate(source=str_split(folder_name, "_")[[1]][[2]])|>
    filter(when>=year(today()))|>
    fuzzyjoin::stringdist_join(correct_names)|>
    select(-lmo_industry_name.x)|>
    rename(lmo_industry_name=lmo_industry_name.y)|>
    pivot_wider(names_from = source, values_from = count)
}

regional_macro_cut <- function(folder_name){
  stokes_regional_files <- list.files(here("data", folder_name), pattern = "9", full.names = TRUE)
  tibble(bc_region=stokes_regional_files)|>
    mutate(data=map(bc_region, read_excel, skip=2, sheet="LabourMarket2", na="NA", col_types="text"),
           data=map(data, tidy_up),
           bc_region=unlist(qdapRegex::ex_between(stokes_regional_files, ")", ".")), #filename contains region between ) and .
           bc_region=str_replace_all(bc_region, "&"," and "))|>
    fuzzyjoin::stringdist_full_join(lfs_region_names, by = "bc_region", max_dist=4)|>
    unnest(data)|>
    mutate(series=str_split(folder_name, "_")[[1]][[2]])|>
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




