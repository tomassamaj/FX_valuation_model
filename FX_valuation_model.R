### Load required packages
library(Rblpapi)
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)
library(purrr)

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
  broad_REER_data[[i]] <- bdh(broad_REER[i], "PX_LAST", start.date = as.Date("2010-01-01"), end.date = Sys.Date())  
}
for (i in 1:length(narrow_REER)) {
  narrow_REER_data[[i]] <- bdh(narrow_REER[i], "PX_LAST", start.date = as.Date("2010-01-01"), end.date = Sys.Date())  
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
end_date <- Sys.Date()  # most recent available
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
      caption = "Citi Broad REER Index",
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
      caption = "Citi Narrow REER Index",
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
fwd_1M_outrt <- paste0(
  dplyr::case_when(
    curr_outrt == "TWD" ~ "NTN",
    curr_outrt == "CNY" ~ "CCN",
    curr_outrt == "ARS" ~ "APN",
    curr_outrt == "CLP" ~ "CHN",
    curr_outrt == "INR" ~ "IRN",
    curr_outrt == "KRW" ~ "KWN",
    curr_outrt == "EGP" ~ "EPN",
    TRUE ~ curr_outrt
  ),
  "+1M Curncy"
)

### Define forward points tickers for currnecies with no outright forwards
#no outright: CZK, PLN, CAD, EUR, ILS, SEK, CHF, GBP, AUD, HKD, SGD
curr_fwd_points <- c("CZK", "PLN", "CAD", "EUR", "ILS", "SEK", "CHF", "GBP", "AUD", "HKD", "SGD") 
fwd_1M_points <- paste0(curr_fwd_points, "1M Curncy")

### Fetch forward data using Bloomberg API
fwd_1M_outrt_data <- list()
for (i in 1:length(fwd_1M_outrt)) {
  fwd_1M_outrt_data[[i]] <- bdh(fwd_1M_outrt[i], "PX_LAST", start.date = as.Date("2010-01-01"), end.date = Sys.Date())  
  }
names(fwd_1M_outrt_data) <- curr_outrt


fwd_1M_points_data <- list()
for (i in 1:length(fwd_1M_points)) {
  fwd_1M_points_data[[i]] <- bdh(fwd_1M_points[i], "PX_LAST", start.date = as.Date("2010-01-01"), end.date = Sys.Date())  
}
names(fwd_1M_points_data) <- curr_fwd_points


### Melt listed data into 2 dfs
outrt_longest_df_name <- names(fwd_1M_outrt_data)[which.max(sapply(fwd_1M_outrt_data, nrow))]
outrt_master_dates_df <- fwd_1M_outrt_data[[outrt_longest_df_name]] %>%
  select(date) %>%
  distinct() %>%
  arrange(date) # Optional: ensure dates are sorted, though they likely are.

outrt_processed_list_of_dfs <- imap(fwd_1M_outrt_data, ~ {
  .x %>%
    rename(!!.y := PX_LAST) %>% # Dynamically set column name to currency code
    select(date, !!.y)         # Select only date and the new currency column
})

outrt_final_combined_df <- reduce(
  .x = outrt_processed_list_of_dfs, # The list of dataframes to join (each now has date and one currency column)
  .f = left_join,             # The function to apply for merging
  .init = outrt_master_dates_df,     # The initial dataframe to start with (our master set of dates)
  by = "date"                  # The common column to join by
)


fwd_points_longest_df_name <- names(fwd_1M_points_data)[which.max(sapply(fwd_1M_points_data, nrow))]
fwd_points_master_dates_df <- fwd_1M_points_data[[fwd_points_longest_df_name]] %>%
  select(date) %>%
  distinct() %>%
  arrange(date) # Optional: ensure dates are sorted, though they likely are.

fwd_points_processed_list_of_dfs <- imap(fwd_1M_points_data, ~ {
  .x %>%
    rename(!!.y := PX_LAST) %>% # Dynamically set column name to currency code
    select(date, !!.y)         # Select only date and the new currency column
})

fwd_points_final_combined_df <- reduce(
  .x = fwd_points_processed_list_of_dfs, # The list of dataframes to join (each now has date and one currency column)
  .f = left_join,             # The function to apply for merging
  .init = fwd_points_master_dates_df,     # The initial dataframe to start with (our master set of dates)
  by = "date"                  # The common column to join by
)

### Download spot rates
spot_fwd_points <- paste0("USD",curr_fwd_points, " Curncy")
spot_df <- list()
for (i in 1:length(spot_fwd_points)) {
  spot_df[[i]] <- bdh(spot_fwd_points[i], "PX_LAST", start.date = as.Date("2010-01-01"), end.date = Sys.Date())  
}
names(spot_df) <- paste0(curr_fwd_points, "_SPOT")



spot_longest_df_name <- names(spot_df)[which.max(sapply(spot_df, nrow))]
spot_master_dates_df <- spot_df[[spot_longest_df_name]] %>%
  select(date) %>%
  distinct() %>%
  arrange(date) # Optional: ensure dates are sorted, though they likely are.

spot_processed_list_of_dfs <- imap(spot_df, ~ {
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


final_spot_fwd_points_df <- left_join(spot_final_combined_df, fwd_points_final_combined_df, by = "date")

fwd_points_df <- final_spot_fwd_points_df %>% select(date)
for (i in curr_fwd_points){
  fwd_points_df[i] <- final_spot_fwd_points_df[paste0(i,"_SPOT")]+final_spot_fwd_points_df[i]/10000
} 

### Combine outrights with forward point rates
fwd_1M <- left_join(fwd_points_df, outrt_final_combined_df, by = "date")

### Download all spot FX rates
curr_ex_USD <- curr[!curr %in% c("USD")]
spot_tickers <- paste0("USD", curr_ex_USD, " Curncy")
spot <- list()
for (i in 1:length(spot_tickers)) {
  spot[[i]] <- bdh(spot_tickers[i], "PX_LAST", start.date = as.Date("2010-01-01"), end.date = Sys.Date())  
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


### Calculate forward premium
merged_df <- left_join(fwd_1M, spot_final_combined_df, by = "date")
fwd_premium_df <- merged_df %>%select(date)
for (i in curr_ex_USD) {
  fwd_premium_df[i] <- (merged_df[paste0(i,".x")] - merged_df[paste0(i,".y")]) / merged_df[paste0(i,".y")] * 100
}
# Rename date column to Date
fwd_premium_df <- fwd_premium_df %>%rename(Date = date)



### Output table
fwd_premium_table <- lapply(reference_dates, get_closest_row, df = fwd_premium_df)
fwd_premium_output_df <- bind_rows(fwd_premium_table, .id = "Period")
fwd_premium_output_df <- fwd_premium_output_df[, c("Period", names(fwd_premium_df)[names(fwd_premium_df) != "Date"])]
rownames(fwd_premium_output_df) <- fwd_premium_output_df$Period
fwd_premium_output_df <- fwd_premium_output_df[, -1] 
fwd_premium_output_df


### Print formatted outputs of Fwd Premium analysis
fwd_premium_formatted_df <- fwd_premium_output_df
fwd_premium_formatted_df <- round(fwd_premium_formatted_df, 2)
fwd_premium_change_rows <- "Today"

# Apply conditional formatting row-wise
for (row in fwd_premium_change_rows) {
  vals <- as.numeric(fwd_premium_formatted_df[row, ])
  
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
  fwd_premium_formatted_df[row, ] <- cell_spec(vals, color = "black", background = colors, format = "html")
}

# Print styled table
kable(fwd_premium_formatted_df,
      format = "html",
      escape = FALSE, # Enable HTML rendering in cells
      caption = "Forward Premium (1M; against USD) in %",
      align = "r") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F,
                position = "center") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#007ACC")
