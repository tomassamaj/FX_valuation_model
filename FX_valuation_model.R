### Load required packages
library(Rblpapi)
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)
library(purrr)
library(tibble)

blpConnect()

### Define list of currencies

curr <- c(
  # Emerging Markets
  "INR",  # India
  "CNH",  # China
  "EGP",  # Egypt
  "ZAR",  # South Africa
  "KRW",  # South Korea
  "TWD",  # Taiwan
  "THB",  # Thailand
  "CZK",  # Czech Republic
  "PLN",  # Poland
  "ARS",  # Argentina
  "CLP",  # Chile
  "TRY",  # Turkey
  "MXN", # Mexico
  "BRL", # Brazil
  
  # Developed Markets
  "CAD",  # Canada
  "USD",  # USA
  "EUR",  
  "ILS",  # Israel
  "SEK",  # Sweden
  "CHF",  # Switzerland
  "GBP",  # United Kingdom
  "AUD",  # Australia
  "HKD",  # Hong Kong
  "JPY",  # Japan
  "SGD"   # Singapore
)


end_date <- as.Date(Sys.Date())  # most recent available

###############
### 1. REER ###
###############


### Define REER tickers
broad_REER <- paste0("CTTWBR", substr(curr, 1, 2), " Index")
narrow_REER <- paste0("CTTWNR", substr(curr, 1, 2), " Index")


### Fetch REER data using Bloomberg API
broad_REER_data <- list()
narrow_REER_data <- list()

for (i in 1:length(broad_REER)) {
  broad_REER_data[[i]] <- bdh(broad_REER[i], "PX_LAST", start.date = as.Date("2020-01-01"), end.date = end_date)  
}
for (i in 1:length(narrow_REER)) {
  narrow_REER_data[[i]] <- bdh(narrow_REER[i], "PX_LAST", start.date = as.Date("2020-01-01"), end.date = end_date)  
}

names(broad_REER_data) <- curr
names(narrow_REER_data) <- curr

broad_date <- c(as.Date(broad_REER_data[["USD"]]$date))
narrow_date <- as.Date(narrow_REER_data[["USD"]]$date)

### Combine data into a two data frames and add date as index
broad_REER_df <- data.frame(
  Date = broad_REER_data[[1]]$date,  # assume all have same date vector
  do.call(cbind, lapply(broad_REER_data, function(x) x$PX_LAST))
)

narrow_REER_df <- data.frame(
  Date = narrow_REER_data[[1]]$date,
  do.call(cbind, lapply(narrow_REER_data, function(x) x$PX_LAST))
)

### Output table
reference_dates <- c(
  "Today" = end_date,
  "t-1W" = end_date - weeks(1),
  "t-1M" = end_date %m-% months(1),
  "t-3M" = end_date %m-% months(3),
  "t-6M" = end_date %m-% months(6),
  "t-1Y" = end_date %m-% years(1),
  "t-3Y" = end_date %m-% years(3),
  "t-5Y" = end_date %m-% years(5)
)

# Find closest available date in data
get_closest_row <- function(target_date, df) {
  closest_idx <- which.min(abs(df$Date - target_date))
  df[closest_idx, ]
}

# Apply for each reference date
broad_table <- lapply(reference_dates, get_closest_row, df = broad_REER_df)
broad_output_df <- bind_rows(broad_table, .id = "Period")
broad_output_df <- broad_output_df[, c("Period", names(broad_REER_df)[names(broad_REER_df) != "Date"])]
rownames(broad_output_df) <- broad_output_df$Period
broad_output_df <- broad_output_df[, -1] 

broad_output_df["Δ1W(%)",] <- (broad_output_df["Today",] / broad_output_df["t-1W",] - 1) * 100
broad_output_df["Δ1M(%)",] <- (broad_output_df["Today",] / broad_output_df["t-1M",] - 1) * 100
broad_output_df["Δ3M(%)",] <- (broad_output_df["Today",] / broad_output_df["t-3M",] - 1) * 100
broad_output_df["Δ6M(%)",] <- (broad_output_df["Today",] / broad_output_df["t-6M",] - 1) * 100
broad_output_df["Δ1Y(%)",] <- (broad_output_df["Today",] / broad_output_df["t-1Y",] - 1) * 100
broad_output_df["Δ3Y(%)",] <- (broad_output_df["Today",] / broad_output_df["t-3Y",] - 1) * 100
broad_output_df["Δ5Y(%)",] <- (broad_output_df["Today",] / broad_output_df["t-5Y",] - 1) * 100
broad_output_df <- round(broad_output_df, 2)
print(broad_output_df)


# Narrow REER table
narrow_table <- lapply(reference_dates, get_closest_row, df = narrow_REER_df)
narrow_output_df <- bind_rows(narrow_table, .id = "Period")
narrow_output_df <- narrow_output_df[, c("Period", names(narrow_REER_df)[names(narrow_REER_df) != "Date"])]
rownames(narrow_output_df) <- narrow_output_df$Period
narrow_output_df <- narrow_output_df[, -1]

narrow_output_df["Δ1W(%)",] <- (narrow_output_df["Today",] / narrow_output_df["t-1W",] - 1) * 100
narrow_output_df["Δ1M(%)",] <- (narrow_output_df["Today",] / narrow_output_df["t-1M",] - 1) * 100
narrow_output_df["Δ3M(%)",] <- (narrow_output_df["Today",] / narrow_output_df["t-3M",] - 1) * 100
narrow_output_df["Δ6M(%)",] <- (narrow_output_df["Today",] / narrow_output_df["t-6M",] - 1) * 100
narrow_output_df["Δ1Y(%)",] <- (narrow_output_df["Today",] / narrow_output_df["t-1Y",] - 1) * 100
narrow_output_df["Δ3Y(%)",] <- (narrow_output_df["Today",] / narrow_output_df["t-3Y",] - 1) * 100
narrow_output_df["Δ5Y(%)",] <- (narrow_output_df["Today",] / narrow_output_df["t-5Y",] - 1) * 100
narrow_output_df <- round(narrow_output_df, 2)
print(narrow_output_df)

### Print formatted outputs of REER analysis
broad_formatted_df <- broad_output_df
broad_change_rows <- rownames(broad_formatted_df)[grepl("Δ", rownames(broad_formatted_df))]

# Apply conditional formatting row-wise
for (row in broad_change_rows) {
  vals <- as.numeric(broad_formatted_df[row, ])
  
  # Scale values between -1 and 1 based on max absolute value
  max_abs_val <- max(abs(vals), na.rm = TRUE)
  scaled <- vals / max_abs_val
  
  # RGB coloring: red → white → green
  colors <- rgb(
    red   = ifelse(scaled < 0, 1, 1 - scaled),
    green = ifelse(scaled > 0, 1, 1 + scaled),
    blue  = ifelse(scaled < 0, 1 + scaled, 1 - scaled)
  )
  
  # Apply formatting
  broad_formatted_df[row, ] <- cell_spec(vals, color = "black", background = colors, format = "html")
}

# Print styled table
kable(broad_formatted_df, 
      format = "html", 
      escape = FALSE,  # Enable HTML rendering in cells
      caption = "Citi Broad REER Index (2017 = 100)",
      align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#007ACC")



narrow_formatted_df <- narrow_output_df
narrow_change_rows <- rownames(narrow_formatted_df)[grepl("Δ", rownames(narrow_formatted_df))]

# Apply conditional formatting row-wise
for (row in narrow_change_rows) {
  vals <- as.numeric(narrow_formatted_df[row, ])
  
  # Scale values between -1 and 1 based on max absolute value
  max_abs_val <- max(abs(vals), na.rm = TRUE)
  scaled <- vals / max_abs_val
  
  # RGB coloring: red → white → green
  colors <- rgb(
    red   = ifelse(scaled < 0, 1, 1 - scaled),
    green = ifelse(scaled > 0, 1, 1 + scaled),
    blue  = ifelse(scaled < 0, 1 + scaled, 1 - scaled)
  )
  
  # Apply formatting
  narrow_formatted_df[row, ] <- cell_spec(vals, color = "black", background = colors, format = "html")
}

# Print styled table
kable(narrow_formatted_df, 
      format = "html", 
      escape = FALSE,  # Enable HTML rendering in cells
      caption = "Citi Narrow REER Index (2017 = 100)",
      align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#007ACC")













##########################
### 2. FORWARD PREMIUM ###
##########################

### Define outright forwards tickers
curr_outrt <- curr[!curr %in% c("USD", "CZK", "PLN", "CAD", "EUR", "ILS", "SEK", "CHF", "GBP", "AUD", "HKD", "SGD")]
curr_outrt <- dplyr::case_when(
  curr_outrt == "TWD" ~ "NTN",
  curr_outrt == "CNY" ~ "CCN",
  curr_outrt == "ARS" ~ "APN",
  curr_outrt == "CLP" ~ "CHN",
  curr_outrt == "INR" ~ "IRN",
  curr_outrt == "KRW" ~ "KWN",
  curr_outrt == "EGP" ~ "EPN",
  curr_outrt == "BRL" ~ "BCN",
  TRUE ~ curr_outrt
)

fwd_1M_outrt <- paste0(curr_outrt,"+1M Curncy")

### Define forward points tickers for currencies with no outright forwards
#no outright: CZK, PLN, CAD, EUR, ILS, SEK, CHF, GBP, AUD, HKD, SGD
curr_fwd_points <- c("CZK", "PLN", "CAD", "EUR", "ILS", "SEK", "CHF", "GBP", "AUD", "HKD", "SGD") 
fwd_1M_points <- paste0(curr_fwd_points, "1M Curncy")


### Fetch forward data using Bloomberg API
fwd_1M_outrt_data <- list()
for (i in 1:length(fwd_1M_outrt)) {
  fwd_1M_outrt_data[[i]] <- bdh(fwd_1M_outrt[i], "PX_LAST", start.date = as.Date("2020-01-01"), end.date = end_date)  
  }
names(fwd_1M_outrt_data) <- curr_outrt


fwd_1M_points_data <- list()
for (i in 1:length(fwd_1M_points)) {
  fwd_1M_points_data[[i]] <- bdh(fwd_1M_points[i], "PX_LAST", start.date = as.Date("2020-01-01"), end.date = end_date)  
}
names(fwd_1M_points_data) <- curr_fwd_points

### Melt listed data into 2 dfs
outrt_1M_longest_df_name <- names(fwd_1M_outrt_data)[which.max(sapply(fwd_1M_outrt_data, nrow))]
outrt_master_1M_dates_df <- fwd_1M_outrt_data[[outrt_1M_longest_df_name]] %>%
  select(date) %>%
  distinct() %>%
  arrange(date) # Optional: ensure dates are sorted, though they likely are.

outrt_1M_processed_list_of_dfs <- imap(fwd_1M_outrt_data, ~ {
  .x %>%
    rename(!!.y := PX_LAST) %>% # Dynamically set column name to currency code
    select(date, !!.y)         # Select only date and the new currency column
})

outrt_final_1M_combined_df <- reduce(
  .x = outrt_1M_processed_list_of_dfs, # The list of dataframes to join (each now has date and one currency column)
  .f = left_join,             # The function to apply for merging
  .init = outrt_master_1M_dates_df,     # The initial dataframe to start with (our master set of dates)
  by = "date"                  # The common column to join by
)


fwd_points_1M_longest_df_name <- names(fwd_1M_points_data)[which.max(sapply(fwd_1M_points_data, nrow))]
fwd_points_1M_master_dates_df <- fwd_1M_points_data[[fwd_points_1M_longest_df_name]] %>%
  select(date) %>%
  distinct() %>%
  arrange(date) # Optional: ensure dates are sorted, though they likely are.

fwd_points_1M_processed_list_of_dfs <- imap(fwd_1M_points_data, ~ {
  .x %>%
    rename(!!.y := PX_LAST) %>% # Dynamically set column name to currency code
    select(date, !!.y)         # Select only date and the new currency column
})

fwd_points_1M_final_combined_df <- reduce(
  .x = fwd_points_1M_processed_list_of_dfs, # The list of dataframes to join (each now has date and one currency column)
  .f = left_join,             # The function to apply for merging
  .init = fwd_points_1M_master_dates_df,     # The initial dataframe to start with (our master set of dates)
  by = "date"                  # The common column to join by
)

### Download spot rates
spot_fwd_points <- paste0("USD", curr_fwd_points, " Curncy")
spot_fwd_points_df <- list()
for (i in 1:length(spot_fwd_points)) {
  spot_fwd_points_df[[i]] <- bdh(spot_fwd_points[i], "PX_LAST", start.date = as.Date("2020-01-01"), end.date = end_date)  
}
names(spot_fwd_points_df) <- paste0(curr_fwd_points, "_SPOT")


spot_fwd_points_longest_df_name <- names(spot_fwd_points_df)[which.max(sapply(spot_fwd_points_df, nrow))]
spot_fwd_points_master_dates_df <- spot_fwd_points_df[[spot_fwd_points_longest_df_name]] %>%
  select(date) %>%
  distinct() %>%
  arrange(date) # Optional: ensure dates are sorted, though they likely are.

spot_fwd_points_processed_list_of_dfs <- imap(spot_fwd_points_df, ~ {
  .x %>%
    rename(!!.y := PX_LAST) %>% # Dynamically set column name to currency code
    select(date, !!.y)         # Select only date and the new currency column
})

spot_fwd_points_final_combined_df <- reduce(
  .x = spot_fwd_points_processed_list_of_dfs, # The list of dataframes to join (each now has date and one currency column)
  .f = left_join,             # The function to apply for merging
  .init = spot_fwd_points_master_dates_df,     # The initial dataframe to start with (our master set of dates)
  by = "date"                  # The common column to join by
)


final_spot_fwd_points_1M_df <- left_join(spot_fwd_points_final_combined_df, fwd_points_1M_final_combined_df, by = "date")

scale_tbl_1M <- bdp(fwd_1M_points, "FWD_SCALE") |>
  tibble::deframe()   

fwd_points_1M_df <- final_spot_fwd_points_1M_df %>% select(date)

j <- 1
for (ccy in curr_fwd_points) {
  
  k_spot      <- paste0(ccy, "_SPOT")              # column with spot
  scale       <- scale_tbl_1M[j]
  fwd_points_1M_df[[ccy]] <-
    final_spot_fwd_points_1M_df[[k_spot]] +
    final_spot_fwd_points_1M_df[[ccy]] / 10^scale
  j <- j+1
}

### Combine outrights with forward point rates
fwd_1M <- left_join(fwd_points_1M_df, outrt_final_1M_combined_df, by = "date")

colnames(fwd_1M) <- sapply(colnames(fwd_1M), function(x) {
  dplyr::case_when(
    x == "NTN" ~ "TWD",
    x == "CCN" ~ "CNY",
    x == "APN" ~ "ARS",
    x == "CHN" ~ "CLP",
    x == "IRN" ~ "INR",
    x == "KWN" ~ "KRW",
    x == "EPN" ~ "EGP",
    x == "EPN" ~ "EGP",
    x == "BCN" ~ "BRL",    
    TRUE ~ x
  )
})




### Download all spot FX rates
curr_ex_USD <- curr[!curr %in% c("USD")]
spot_tickers <- paste0("USD", curr_ex_USD, " Curncy") 
spot <- list()
for (i in 1:length(spot_tickers)) {
  spot[[i]] <- bdh(spot_tickers[i], "PX_LAST", start.date = as.Date("2020-01-01"), end.date = end_date)  
}
names(spot) <- curr_ex_USD

spot_longest_df_name <- names(spot)[which.max(sapply(spot, nrow))]
spot_master_dates_df <- spot[[spot_longest_df_name]] %>%
  select(date) %>%
  distinct() %>%
  arrange(date) # Optional: ensure dates are sorted, though they likely are.

spot_processed_list_of_dfs <- imap(spot, ~ {
  .x %>%
    rename(!!.y := PX_LAST) %>% # Dynamically set column name to currency code
    select(date, !!.y)         # Select only date and the new currency column
})

spot_final_combined_df <- reduce(
  .x = spot_processed_list_of_dfs, # The list of dataframes to join (each now has date and one currency column)
  .f = left_join,             # The function to apply for merging
  .init = spot_master_dates_df,     # The initial dataframe to start with (our master set of dates)
  by = "date"                  # The common column to join by
)


### Calculate 1M forward premium
merged_1M_df <- left_join(fwd_1M, spot_final_combined_df, by = "date")
fwd_1M_premium_df <- merged_1M_df %>%select(date)
for (i in curr_ex_USD) {
  fwd_1M_premium_df[i] <- (merged_1M_df[paste0(i,".x")] - merged_1M_df[paste0(i,".y")]) / merged_1M_df[paste0(i,".y")] * 100
}
# Rename date column to Date
fwd_1M_premium_df <- fwd_1M_premium_df %>%rename(Date = date)



### Output tables
fwd_1M_premium_table <- lapply(reference_dates, get_closest_row, df = fwd_1M_premium_df)
fwd_1M_premium_output_df <- bind_rows(fwd_1M_premium_table, .id = "Period")
fwd_1M_premium_output_df <- fwd_1M_premium_output_df[, c("Period", names(fwd_1M_premium_df)[names(fwd_1M_premium_df) != "Date"])]
rownames(fwd_1M_premium_output_df) <- fwd_1M_premium_output_df$Period
fwd_1M_premium_output_df <- fwd_1M_premium_output_df[, -1] 
fwd_1M_premium_output_df


### Print formatted outputs of Fwd Premium analysis
fwd_1M_premium_formatted_df <- fwd_1M_premium_output_df
fwd_1M_premium_formatted_df <- round(fwd_1M_premium_formatted_df, 2)
fwd_1M_premium_change_rows <- "Today"

# Apply conditional formatting row-wise
for (row in fwd_1M_premium_change_rows) {
  vals <- as.numeric(fwd_1M_premium_formatted_df[row, ])
  
  # Create a logical vector to identify NAs
  is_na <- is.na(vals)
  
  # Initialize colors vector, defaulting to white for NAs
  colors <- rep("#FFFFFF", length(vals)) # Default to white background
  
  # Only apply scaling and color logic to non-NA values
  if (any(!is_na)) { # Check if there are any non-NA values to process
    non_na_vals <- vals[!is_na]
    
    # Scale values between -1 and 1 based on max absolute value for non-NA values
    max_abs_val <- max(abs(non_na_vals), na.rm = TRUE)
    scaled <- non_na_vals / max_abs_val
    
    # RGB coloring: red → white → green for non-NA values
    non_na_colors <- rgb(
      red   = ifelse(scaled < 0, 1, 1 - scaled),
      green = ifelse(scaled > 0, 1, 1 + scaled),
      blue  = ifelse(scaled < 0, 1 + scaled, 1 - scaled)
    )
    
    # Assign the calculated colors back to the 'colors' vector for non-NA positions
    colors[!is_na] <- non_na_colors
  }
  
  # Apply formatting
  # For NA values, cell_spec will still print "NA" as text, but with a white background.
  # For non-NA values, it will print the value with the calculated color.
  fwd_1M_premium_formatted_df[row, ] <- cell_spec(vals, color = "black", background = colors, format = "html")
}

# Print styled table
kable(fwd_1M_premium_formatted_df,
      format = "html",
      escape = FALSE, # Enable HTML rendering in cells
      caption = "Forward Premium (1M; against USD) in %",
      align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#007ACC")










##############################################################
### 2-bis.  FORWARD-PREMIUM (1 Y) – revised for TRY & THB  ###
##############################################################

## ---------- 1.  1-Y forward instruments -------------------
##   • all quotes remain USD/CCY                             ##
##   • TRY & THB now treated as forward-points pairs         ##

# list that DOES have 12-month outrights
curr_outrt_1Y   <- setdiff(curr_outrt, c("TRY", "THB"))
fwd_1Y_outrt    <- paste0(curr_outrt_1Y, "+12M Curncy")

# list that is ONLY forward-points
curr_fwd_pts_1Y <- c(curr_fwd_points, "TRY", "THB")
fwd_1Y_points   <- paste0(curr_fwd_pts_1Y, "12M Curncy")

## ---------- 2.  Download forward data (same as before) ----
fwd_1Y_outrt_data  <- lapply(fwd_1Y_outrt,  \(t)
                             bdh(t, "PX_LAST",
                                 start.date = as.Date("2020-01-01"),
                                 end.date   = end_date)) |>
  rlang::set_names(curr_outrt_1Y)

fwd_1Y_points_data <- lapply(fwd_1Y_points, \(t)
                             bdh(t, "PX_LAST",
                                 start.date = as.Date("2020-01-01"),
                                 end.date   = end_date)) |>
  rlang::set_names(curr_fwd_pts_1Y)

## ---------- 3.  Melt to wide data frames -------------------

## helper to bind all series by date ---------------------------------
bind_series <- function(lst){
  longest <- names(lst)[which.max(sapply(lst, nrow))]
  master  <- lst[[longest]][, "date", drop = FALSE] |>
    distinct() |> arrange(date)
  purrr::imap(lst, \(x, nm) dplyr::rename(x, !!nm := PX_LAST)) |>
    purrr::reduce(.init = master, .f = dplyr::left_join, by = "date")
}

outrt_1Y_df       <- bind_series(fwd_1Y_outrt_data)      # outright prices
fwd_points_1Y_df  <- bind_series(fwd_1Y_points_data)     # points (not yet scaled)

## ---------- 4.  Convert points → outright ------------------

## we already have spot_fwd_points_final_combined_df from the 1 M block
## (spot quoted USD/CCY).  Use it; no need to re-download.
## Merge spot + points, then add points / 10^scale

scale_tbl_1Y <- bdp(fwd_1Y_points, "FWD_SCALE") |> tibble::deframe()

# spot_final_combined_df already has every USD/CCY spot, including TRY & THB
spot_extra <- spot_final_combined_df %>%
  select(date, TRY, THB) %>%
  rename(TRY_SPOT = TRY,
         THB_SPOT = THB)

# append only if the columns are not present yet
spot_fwd_points_final_combined_df <-
  spot_fwd_points_final_combined_df %>%
  dplyr::left_join(spot_extra, by = "date")

## ---- add TRY_SPOT & THB_SPOT to the points-spot panel ----
spot_fwd_pts_full <- spot_fwd_points_final_combined_df       # from 1-M block
if (!all(c("TRY_SPOT","THB_SPOT") %in% names(spot_fwd_pts_full))) {
  spot_extra <- spot_final_combined_df |>                    # big USD/CCY spot
    select(date, TRY, THB) |>
    rename(TRY_SPOT = TRY, THB_SPOT = THB)
  spot_fwd_pts_full <- left_join(spot_fwd_pts_full,
                                 spot_extra, by = "date")
}


## start with the spot columns only
fwd_add_1Y_df <- spot_fwd_pts_full %>% select(date)

k <- 1
for (ccy in curr_fwd_pts_1Y){
  scale  <- scale_tbl_1Y[k]
  spot_c <- paste0(ccy, "_SPOT")
  fwd_add_1Y_df[[ccy]] <-
    spot_fwd_points_final_combined_df[[spot_c]] +
    fwd_points_1Y_df[[ccy]] / 10^scale
  k <- k+1
}

## ---------- 5.  Combine: “synthetic” + outright ------------
fwd_1Y <- dplyr::left_join(fwd_add_1Y_df, outrt_1Y_df, by = "date")

## Rename the exotic Bloomberg root codes back to ISO ccy tags
colnames(fwd_1Y) <- sapply(colnames(fwd_1Y), \(x)
                           dplyr::case_when(
                             x == "NTN" ~ "TWD",
                             x == "CCN" ~ "CNY",
                             x == "APN" ~ "ARS",
                             x == "CHN" ~ "CLP",
                             x == "IRN" ~ "INR",
                             x == "KWN" ~ "KRW",
                             x == "EPN" ~ "EGP",
                             x == "EPN" ~ "EGP",
                             x == "BCN" ~ "BRL",
                             TRUE ~ x
                           ))

## ---------- 6.  Forward-premium calculation ----------------

## spot_final_combined_df already exists (USD/CCY)
merged_1Y_df <- dplyr::left_join(fwd_1Y, spot_final_combined_df, by = "date")

fwd_1Y_premium_df <- merged_1Y_df %>% select(date)

for (ccy in curr_ex_USD){
  fwd_1Y_premium_df[[ccy]] <-
    (merged_1Y_df[[paste0(ccy, ".x")]] /
       merged_1Y_df[[paste0(ccy, ".y")]] - 1) * 100   # same formula, clearer
}

fwd_1Y_premium_df <- dplyr::rename(fwd_1Y_premium_df, Date = date)

## ---------- 7.  Format / print table -----------------------

fwd_1Y_premium_output_df <- lapply(reference_dates, get_closest_row,
                                   df = fwd_1Y_premium_df) |>
  bind_rows(.id = "Period") |>
  select(-Date) |>
  column_to_rownames("Period")

fwd_1Y_premium_formatted_df <- round(fwd_1Y_premium_output_df, 2)
row_to_colour <- "Today"

## --- colour scale identical to 1-M block, but NA-safe ---------
vals   <- as.numeric(fwd_1Y_premium_formatted_df[row_to_colour, ])

# 1. start with all-white
cols <- rep("#FFFFFF", length(vals))

# 2. work only on the cells that are not NA
idx   <- !is.na(vals)

if (any(idx)) {
  max_abs <- max(abs(vals[idx]), na.rm = TRUE)
  if (max_abs == 0) max_abs <- 1          # avoid division by zero
  
  scaled  <- vals[idx] / max_abs
  
  cols[idx] <- rgb(
    red   = ifelse(scaled < 0, 1, 1 - scaled),
    green = ifelse(scaled > 0, 1, 1 + scaled),
    blue  = ifelse(scaled < 0, 1 + scaled, 1 - scaled)
  )
}

# 3. write the row back with the colours
fwd_1Y_premium_formatted_df[row_to_colour, ] <-
  cell_spec(vals, color = "black", background = cols, format = "html")

kable(fwd_1Y_premium_formatted_df,
      format  = "html",
      escape  = FALSE,
      caption = "Forward Premium (1 Y; against USD) in %",
      align   = "r") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed",
                                      "responsive"),
                full_width = FALSE,
                position   = "center") |>
  row_spec(0, bold = TRUE, color = "white", background = "#007ACC")




