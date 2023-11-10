input_file_path <- "survey_results_public.csv"
data <- read.csv(input_file_path)

# Filter rows to only include developers in the United States
data <- data[data$Country == "United States of America", ]

# Include columns we're interested in
data <- data[, c("CompTotal", "ICorPM", "CodingActivities", "Age", "Employment", "DevType", "OrgSize", "EdLevel", "RemoteWork", "PurchaseInfluence", "WorkExp", "YearsCode", "YearsCodePro", "Industry")]

# Remove rows with missing data
data <- na.omit(data)

# Remove rows with no compensation
data <- data[data$CompTotal > 0, ]

# Remove outliers in compensation using the IQR method since we're dealing with a non-normal distribution and care about predicting the compensation of the average

# Step 1: Calculate the IQR
q1 <- quantile(data$CompTotal, 0.25, na.rm = TRUE)
q3 <- quantile(data$CompTotal, 0.75, na.rm = TRUE)
iqr <- q3 - q1

# Step 2: Calculate the lower and upper bounds
lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

# Step 3: Filter out the outliers
data <- data[data$CompTotal > lower_bound & data$CompTotal < upper_bound, ]

# Transform years coding professionally and years coding to numeric

# Step 1: Replace "Less than 1 year" with 0
data$YearsCodePro[data$YearsCodePro == "Less than 1 year"] <- 0
data$YearsCode[data$YearsCode == "Less than 1 year"] <- 0

# Step 2: Replace "More than 50 years" with 50
data$YearsCodePro[data$YearsCodePro == "More than 50 years"] <- 50
data$YearsCode[data$YearsCode == "More than 50 years"] <- 50

# Generate CSV file
write.csv(data, "cleaned_data.csv", row.names = FALSE)
