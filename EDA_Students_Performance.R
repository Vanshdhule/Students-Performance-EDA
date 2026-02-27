# ------------------------------------------------------------
# Exploratory Data Analysis (EDA)
# Dataset: Students Performance in Exams
# ------------------------------------------------------------

# STEP 1: Load Dataset
students_data <- read.csv("C:/Users/rkjey/Downloads/StudentsPerformance.csv")

# Clean column names
colnames(students_data) <- make.names(colnames(students_data))

# ------------------------------------------------------------
# DATA UNDERSTANDING
# ------------------------------------------------------------

# View first few records
head(students_data)

# Structure of dataset (Rows & Columns)
str(students_data)

# Summary statistics
summary(students_data)

# Check missing values
colSums(is.na(students_data))

# Mean of scores
colMeans(students_data[, c("math.score",
                           "reading.score",
                           "writing.score")])

# Standard Deviation (Variability Check)
apply(students_data[, c("math.score",
                        "reading.score",
                        "writing.score")], 2, sd)

# ------------------------------------------------------------
# CREATE NEW COLUMNS
# ------------------------------------------------------------

# Total Score
students_data$Total_Score <- students_data$math.score +
                             students_data$reading.score +
                             students_data$writing.score

# Performance Band Categorization
students_data$Performance_Band <- cut(students_data$Total_Score,
                                      breaks = c(0,150,200,250,300),
                                      labels = c("Poor","Average","Good","Excellent"),
                                      include.lowest = TRUE)

# Count students in each band
table(students_data$Performance_Band)

# ------------------------------------------------------------
# DATA VISUALIZATION
# ------------------------------------------------------------

# Histogram - Math
hist(students_data$math.score,
     main="Distribution of Math Scores",
     xlab="Math Score",
     col="lightblue",
     border="black")

# Histogram - Reading
hist(students_data$reading.score,
     main="Distribution of Reading Scores",
     xlab="Reading Score",
     col="lightgreen",
     border="black")

# Histogram - Writing
hist(students_data$writing.score,
     main="Distribution of Writing Scores",
     xlab="Writing Score",
     col="lightpink",
     border="black")

# Boxplot (Outlier Detection)
boxplot(students_data[, c("math.score",
                          "reading.score",
                          "writing.score")],
        main="Boxplot of Exam Scores",
        col=c("red","green","blue"))

# Scatter Plot - Math vs Reading
plot(students_data$math.score,
     students_data$reading.score,
     main="Math vs Reading Scores",
     xlab="Math Score",
     ylab="Reading Score",
     col="purple",
     pch=19)

# Pairwise Scatter Plot
pairs(students_data[, c("math.score",
                        "reading.score",
                        "writing.score")],
      main="Pairwise Relationship Between Scores")

# ------------------------------------------------------------
# CORRELATION ANALYSIS
# ------------------------------------------------------------

cor_matrix <- cor(students_data[, c("math.score",
                                    "reading.score",
                                    "writing.score")])

print(cor_matrix)

heatmap(cor_matrix,
        main="Correlation Heatmap")

# ------------------------------------------------------------
# CATEGORICAL ANALYSIS
# ------------------------------------------------------------

# Gender Distribution
gender_table <- table(students_data$gender)
print(gender_table)

barplot(gender_table,
        main="Gender Distribution",
        col=c("orange","skyblue"))

# Test Preparation Distribution
prep_table <- table(students_data$test.preparation.course)
print(prep_table)

barplot(prep_table,
        main="Test Preparation Course Distribution",
        col=c("red","green"))

# ------------------------------------------------------------
# GROUP-WISE ANALYSIS
# ------------------------------------------------------------

# Average Scores by Gender
aggregate(students_data[, c("math.score",
                            "reading.score",
                            "writing.score")],
          by=list(Gender=students_data$gender),
          mean)

# Average Scores by Test Preparation
aggregate(students_data[, c("math.score",
                            "reading.score",
                            "writing.score")],
          by=list(TestPrep=students_data$test.preparation.course),
          mean)

# Average Total Score by Performance Band
aggregate(students_data$Total_Score,
          by=list(Performance_Band=students_data$Performance_Band),
          mean)
