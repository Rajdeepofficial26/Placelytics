# CELL 1 – Project Title

cat("Placelytics: A Data-Driven Analysis of Student Placement Patterns")

# CELL 2 – Load Libraries

library(readr)
library(dplyr)
library(ggplot2)

# CELL 3 – Load Dataset

data <- read.csv("dataset_aiplacement.csv")

head(data)

# CELL 4 – Dataset Structure

str(data)

# CELL 5 – Summary Statistics

summary(data)

# CELL 6 – Column Names

colnames(data)

# CELL 7 – Missing Values Check

colSums(is.na(data))

# CELL 8 – Remove Missing Values

data <- na.omit(data)

# CELL 9 – Placement Status Count

table(data$PlacementStatus)

# CELL 10 – Placement Distribution Graph

ggplot(data, aes(x = PlacementStatus)) +
  geom_bar(fill = "blue") +
  ggtitle("Placement Status Distribution") +
  xlab("Placement Status") +
  ylab("Count")

# CELL 11 – Histogram of CGPA

hist(data$CGPA,
     col="skyblue",
     main="Distribution of Student CGPA",
     xlab="CGPA")

# CELL 12 – Boxplot of CGPA

boxplot(data$CGPA,
        col="lightgreen",
        main="Boxplot of CGPA",
        ylab="CGPA")

# CELL 13 – CGPA vs Placement Status

ggplot(data,
       aes(x = PlacementStatus,
           y = CGPA)) +
  geom_boxplot(fill = "green") +
  ggtitle("CGPA vs Placement Status")

# CELL 14 – Internships vs Placement

table(data$Internships,
      data$PlacementStatus)

# CELL 15 – Internship Analysis Graph


ggplot(data,
       aes(x = factor(Internships),
           fill = PlacementStatus)) +
  geom_bar(position = "dodge") +
  ggtitle("Internships vs Placement") +
  xlab("Number of Internships")

# CELL 16 – Chances Distribution

barplot(table(data$Chances),
        col = c("red","yellow","green"),
        main = "Placement Chances Distribution")

# CELL 17 – Correlation Matrix

numeric_data <- data %>%
  select(CGPA,
         Internships,
         Projects,
         Certifications,
         Aptitude)

cor(numeric_data)

# CELL 18 – Average Values

colMeans(data[, c("CGPA",
                  "Internships",
                  "Projects",
                  "Certifications",
                  "Aptitude")])

# CELL 19 – Linear Regression Model

model <- lm(Aptitude ~ CGPA,
            data = data)

summary(model)

# CELL 20 – Regression Graph (Corrected)

ggplot(data,
       aes(x = CGPA,
           y = Aptitude)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm",
              color = "red",
              se = FALSE) +
  ggtitle("CGPA vs Aptitude Regression Analysis") +
  xlab("CGPA") +
  ylab("Aptitude")

# CELL 21 – Correlation Value

cor(data$CGPA,
    data$Aptitude)

# CELL 22 – Placement Predictor Function

predictPlacement <- function(cgpa,
                             internships,
                             projects,
                             certifications,
                             aptitude){

  score <- cgpa +
           internships +
           projects +
           certifications +
           aptitude

  if(score >= 18){
    return(c("Placed","High"))
  }

  else if(score >= 12){
    return(c("Placed","Medium"))
  }

  else{
    return(c("Unplaced","Low"))
  }
}

# CELL 23 – Run Placement Predictor

predictPlacement()
