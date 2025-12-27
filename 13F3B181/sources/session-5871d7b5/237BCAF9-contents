library(readr)
library(dplyr)
library(ggplot2)

setwd("D:/Learning/Immunoassay Project")
path <- "Medical_Device_Manufacturing_Dataset.csv"

rtd <- read_csv("D:/Learning/Immunoassay Project/Medical_Device_Manufacturing_Dataset.csv", show_col_types = FALSE)
stopifnot(file.exists(path))
rtd <- read_csv(path, show_col_types = FALSE)

cat("Columns in dataset:\n")
cat("\nBasic structure:\n")
cat("Rows:", nrow(rtd), "Cols:", ncol(rtd), "\n")
print(names(rtd))
head(rtd)
summary(rtd)
cat("\nGlimpse of data types:\n")
print(glimpse(rtd))

cat("\nHead and summary:\n")
print(head(rtd))
print(summary(rtd))

# QC: missing values by column
cat("\nMissing values by column:\n")
missing_summary <- sapply(rtd, function(x) sum(is.na(x)))
print(sort(missing_summary, decreasing = TRUE))

# QC: duplicate record ID check if available
if ("Record ID" %in% names(rtd)) {
  dup_count <- sum(duplicated(rtd[["Record ID"]]))
  cat("\nDuplicate Record ID rows:", dup_count, "\n")
}

# QC: timestamp parsing and coverage
if ("Timestamp" %in% names(rtd)) {
  ts <- rtd$Timestamp
  parsed <- as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  fallback <- as.POSIXct(ts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  parsed[is.na(parsed)] <- fallback[is.na(parsed)]
  
  bad_ts <- is.na(parsed)
  cat("\nTimestamp parse issues:", sum(bad_ts), "rows could not be parsed.\n")
  
  if (all(!bad_ts)) {
    cat("Timestamp range (UTC):", format(min(parsed)), "to", format(max(parsed)), "\n")
  }
}

# QC: numeric distribution summary (mean, sd, 5th/95th percentiles)
num_cols <- rtd %>% select(where(is.numeric))
if (ncol(num_cols) > 0) {
  numeric_profile <- bind_rows(Map(function(x, nm) {
    dplyr::tibble(
      column = nm,
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      p05 = quantile(x, 0.05, na.rm = TRUE, names = FALSE),
      p95 = quantile(x, 0.95, na.rm = TRUE, names = FALSE),
      missing = sum(is.na(x))
    )
  }, num_cols, names(num_cols)))
  
  cat("\nNumeric profile (mean/sd/p05/p95/missing):\n")
  print(numeric_profile)
  
  # Quick outlier flagging via Tukey fences
  cat("\nPotential outliers (beyond 1.5*IQR):\n")
  outlier_counts <- bind_rows(Map(function(x, nm) {
    qs <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
    iqr <- qs[2] - qs[1]
    low <- qs[1] - 1.5 * iqr
    high <- qs[2] + 1.5 * iqr
    dplyr::tibble(
      column = nm,
      low_cutoff = low,
      high_cutoff = high,
      flagged = sum(x < low | x > high, na.rm = TRUE)
    )
  }, num_cols, names(num_cols)))
  print(outlier_counts)
}

# QC: categorical mode counts for quick sanity checks
cat_cols <- rtd %>% select(where(~!is.numeric(.x) && !inherits(.x, "POSIXct")))
if (ncol(cat_cols) > 0) {
  cat("\nTop categories (first 5 levels per column):\n")
  top_counts <- lapply(cat_cols, function(x) {
    head(sort(table(x), decreasing = TRUE), 5)
  })
  print(top_counts)
}

# QC: non-negative constraints for common manufacturing metrics
non_negative_cols <- intersect(c("n Inspected", "Defectives (np)", "Defects (c)", "Opportunities"), names(rtd))
if (length(non_negative_cols) > 0) {
  cat("\nNon-negative check (rows < 0):\n")
  nnc <- bind_rows(Map(function(x, nm) {
    dplyr::tibble(
      column = nm,
      negatives = sum(x < 0, na.rm = TRUE)
    )
  }, rtd[non_negative_cols], non_negative_cols))
  print(nnc)
}