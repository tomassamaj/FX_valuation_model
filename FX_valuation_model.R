### Load required packages
library(Rblpapi)
library(dplyr)
library(lubridate)
library(knitr)
library(kableExtra)

blpConnect()

### Define currencies and REER tickers
curr <- c(
  # Emerging Markets
  "INR",  # India
  "CNY",  # China
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
end_date <- max(broad_REER_df$Date)  # most recent available
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












### Print formatted outputs of REER analysis ###
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

