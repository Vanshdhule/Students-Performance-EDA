# ------------------------------------------------------------
# Exploratory Data Analysis (EDA)
# Dataset: Students Performance in Exams
# Objective: Inspect structure, summarize statistics,
# visualize distributions and relationships.
# ------------------------------------------------------------

# STEP 1: Load Dataset (Using Full File Path)
students_data <- read.csv("C:/Users/rkjey/Downloads/StudentsPerformance.csv")

# Clean column names (remove spaces and special characters)
colnames(students_data) <- make.names(colnames(students_data))

# STEP 2: View First Few Records
head(students_data)

# STEP 3: Structure of Dataset
str(students_data)

# STEP 4: Summary Statistics
summary(students_data)

# STEP 5: Check for Missing Values
colSums(is.na(students_data))

# STEP 6: Basic Statistical Measures (Mean of Scores)
colMeans(students_data[, c("math.score",
                           "reading.score",
                           "writing.score")])

# ------------------------------------------------------------
# DATA VISUALIZATION
# ------------------------------------------------------------

# Histogram - Math Scores
hist(students_data$math.score,
     main = "Distribution of Math Scores",
     xlab = "Math Score",
     col = "lightblue",
     border = "black")

# Histogram - Reading Scores
hist(students_data$reading.score,
     main = "Distribution of Reading Scores",
     xlab = "Reading Score",
     col = "lightgreen",
     border = "black")

# Histogram - Writing Scores
hist(students_data$writing.score,
     main = "Distribution of Writing Scores",
     xlab = "Writing Score",
     col = "lightpink",
     border = "black")

# Boxplot - All Scores (Detect Outliers)
boxplot(students_data[, c("math.score",
                          "reading.score",
                          "writing.score")],
        main = "Boxplot of Exam Scores",
        col = c("red", "green", "blue"))

# Scatter Plot - Math vs Reading
plot(students_data$math.score,
     students_data$reading.score,
     main = "Math vs Reading Scores",
     xlab = "Math Score",
     ylab = "Reading Score",
     col = "purple",
     pch = 19)

# Pairwise Scatter Plot
pairs(students_data[, c("math.score",
                        "reading.score",
                        "writing.score")],
      main = "Pairwise Relationship Between Scores")

# ------------------------------------------------------------
# CORRELATION ANALYSIS
# ------------------------------------------------------------

cor_matrix <- cor(students_data[, c("math.score",
                                    "reading.score",
                                    "writing.score")])

print(cor_matrix)

# Heatmap of Correlation Matrix
heatmap(cor_matrix,
        main = "Correlation Heatmap")

# ------------------------------------------------------------
# CATEGORICAL VARIABLE ANALYSIS
# ------------------------------------------------------------

# Gender Distribution
gender_table <- table(students_data$gender)
print(gender_table)

barplot(gender_table,
        main = "Gender Distribution",
        col = c("orange", "skyblue"))

# Test Preparation Course Distribution
prep_table <- table(students_data$test.preparation.course)
print(prep_table)

barplot(prep_table,
        main = "Test Preparation Course Distribution",
        col = c("red", "green"))

# ------------------------------------------------------------
# GROUP-WISE AVERAGE SCORES
# ------------------------------------------------------------

# Average Scores by Gender
aggregate(students_data[, c("math.score",
                            "reading.score",
                            "writing.score")],
          by = list(Gender = students_data$gender),
          mean)

# Average Scores by Test Preparation Course
aggregate(students_data[, c("math.score",
                            "reading.score",
                            "writing.score")],
          by = list(TestPrep = students_data$test.preparation.course),
          mean)