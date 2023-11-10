input_file_path <- "survey_results_public.csv"
data <- read.csv(input_file_path)

# Filter rows to only include developers
data <- data[data$MainBranch == "I am a developer by profession", ]

# Filter rows to only include developers in the United States
data <- data[data$Country == "United States of America", ]

# Include columns we're interested in
data <- data[, c("CompTotal", "ICorPM", "CodingActivities", "Age", "Employment", "DevType", "OrgSize", "EdLevel", "RemoteWork", "PurchaseInfluence", "WorkExp", "YearsCode", "YearsCodePro", "Industry")]

# Remove rows with missing data
data <- na.omit(data)

# Filter rows with unusually low compensation < $50,000 which according to the U.S. Census Bureau is what the lowest 10% of developers make
data <- data[data$CompTotal >= 50000, ]

# Dummy code CodingActivities
coding_activites <- strsplit(data$CodingActivities, ";") # Split the CodingActivities column by semicolon
coding_activites <- unlist(coding_activites) # Convert the list to a vector
coding_activites <- unique(coding_activites) # Remove duplicates
coding_activites <- coding_activites[coding_activites != "Other (please specify):"] # Remove "Other (please specify):" since it's not a coding activity
coding_activites <- coding_activites[coding_activites != "I don’t code outside of work"] # Remove "I don’t code outside of work" since it's not a coding activity

# Create a column for each coding activity
for (coding_activity in coding_activites) {
    data[[coding_activity]] <- grepl(coding_activity, data$CodingActivities)
}

# Remove CodingActivities column
data$CodingActivities <- NULL

# Rename columns
colnames(data)[colnames(data) == "Hobby"] <- "CodesAsHobbyOnSide"
colnames(data)[colnames(data) == "Contribute to open-source projects"] <- "ContributesToOpenSourceOnSide"
colnames(data)[colnames(data) == "Bootstrapping a business"] <- "BootstrapsBusinessOnSide"
colnames(data)[colnames(data) == "Professional development or self-paced learning from online courses"] <- "LearnsCodeProfessionallyOnlineOnSide"
colnames(data)[colnames(data) == "Freelance/contract work"] <- "DoesFreelanceCodingOnSide"
colnames(data)[colnames(data) == "School or academic work"] <- "DoesAcademicCodingOnSide"


# Remove outliers in compensation using the IQR method since we're dealing with a non-normal distribution and care about predicting the compensation of the averagec developer

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

# Step 3: Convert to numeric
data$YearsCodePro <- as.numeric(data$YearsCodePro)
data$YearsCode <- as.numeric(data$YearsCode)


# Generate CSV file
write.csv(data, "cleaned_data.csv", row.names = FALSE)
