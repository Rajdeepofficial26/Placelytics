cat("Placelytics: A Data-Driven Analysis of Student Placement Patterns")

data <- read.csv("dataset_aiplacement.csv")
head(data)

str(data)

summary(data)

hist(data$CGPA,
     col="skyblue",
     main="Distribution of Student CGPA",
     xlab="CGPA")

boxplot(data$CGPA,
        col="lightgreen",
        main="Boxplot of CGPA",
        ylab="CGPA")

table(data$PlacementStatus)

model <- lm(Aptitude ~ CGPA, data=data)
summary(model)

plot(data$CGPA,
     data$Aptitude,
     pch=16,
     col="blue",
     main="CGPA vs Aptitude",
     xlab="CGPA",
     ylab="Aptitude")

abline(model, col="red", lwd=2)

cor(data$CGPA, data$Aptitude)

colMeans(data[, c("CGPA","Internships","Projects","Certifications","Aptitude")])

barplot(table(data$PlacementStatus),
        col=c("orange","lightblue"),
        main="Placement Status Distribution",
        xlab="Placement Status",
        ylab="Count")

barplot(table(data$Chances),
        col=c("red","yellow","green"),
        main="Placement Chances Distribution")

predictPlacement <- function() {

  cgpa <- as.numeric(readline("Enter CGPA: "))
  internships <- as.numeric(readline("Enter Internships: "))
  projects <- as.numeric(readline("Enter Projects: "))
  certifications <- as.numeric(readline("Enter Certifications: "))
  aptitude <- as.numeric(readline("Enter Aptitude Score: "))

  score <- cgpa + internships + projects + certifications + aptitude

  if(score >= 18){
    print("Placement Status: Placed")
    print("Chance: High")
  }

  else if(score >= 12){
    print("Placement Status: Placed")
    print("Chance: Medium")
  }

  else{
    print("Placement Status: Unplaced")
    print("Chance: Low")
  }
}
predictPlacement()


plot(data$CGPA,
     data$Aptitude,
     pch=16,
     col="blue",
     main="CGPA vs Aptitude",
     xlab="CGPA",
     ylab="Aptitude")


