## Statistical Consulting Final Project
## Author: Kenny Mai

# The following project analyzes NYC high school senior attendance rates before and during the 
# Covid-19 pandemic. It uses two datasets from the Research Alliance for NYC Schools, and one dataset 
# from CityHealth's Dashboard. The Research Alliance data include student-level demographic, geographical,
# and attendance data for the 2018-2019 and 2019-2020 school years, the latter being affected by the lockdown. 
# The CityHealth data contains geographical data and the corresponding Covid Risk Index on the census tract level.

# Load libraries
library(readr)
library(readxl)

# Attendance is only through Sept 2019 - March 2020, due to data collection problems
# Load data
merged19 <- read_excel("C:/Users/Admin/Downloads/merged19.xlsx")
merged20 <- read_excel("C:/Users/Admin/Downloads/merged20.xlsx")
localriskindex <- read_csv("C:/Users/Admin/Downloads/localriskindex.csv")

# Recode and merge raw data
risk <- data.frame(RESTRACT=as.character(localriskindex$stcotr_fips), RISKINDEX=localriskindex$est)
stu19 <- merge(merged19, risk, by.x = "RESTRACT")
stu20 <- merge(merged20, risk, by.x = "RESTRACT")
stu19$year <- 0
stu20$year <- 1
rawdat <- rbind(stu19, stu20)
# Low attendance data is a percentage based on school days available in the year, 180
# Creating binary indicator for low attendance
rawdat$la <- ifelse(rawdat$ATTPCT180<mean(stu19$ATTPCT180),1,0)

# Recoding ethnicity categories from the Department of Education
rawdat$ETH <- ifelse(rawdat$ETHCAT==2,'Asian',rawdat$ETH)
rawdat$ETH <- ifelse(rawdat$ETHCAT==3,'Hispanic',rawdat$ETH)
rawdat$ETH <- ifelse(rawdat$ETHCAT==4,'Black',rawdat$ETH)
rawdat$ETH <- ifelse(rawdat$ETHCAT==5,'White',rawdat$ETH)
rawdat$ETH <- ifelse(rawdat$ETHCAT==1,'Other',rawdat$ETH)
rawdat$ETH <- ifelse(rawdat$ETHCAT>=6,'Other',rawdat$ETH)

# Use white students as the reference
rawdat$ETH <- factor(rawdat$ETH, ordered = F)
rawdat$ETH <- relevel(rawdat$ETH, ref = 'White')

# Construct final data frame
dat <- data.frame(
  id = rownames(rawdat),
  covid = as.factor(a$year),
  school = rawdat$ENRDBN,
  tract = rawdat$RESTRACT,
  district = as.factor(rawdat$RESDIS),
  boro = as.factor(rawdat$RESBORO),
  zipcode = as.factor(rawdat$RESZIP),
  # Keeping risk category as an ordinal category
  risk = as.numeric(rawdat$RISKINDEX),
  attend = rawdat$ATTPCT180,
  male = as.factor(ifelse(rawdat$GENCAT==2,1,0)),
  ethnicity = as.factor(rawdat$ETH),
  lowatt = as.factor(rawdat$la)
  )

dat <- dat[complete.cases(dat),]

mod1 <- lm(attend~covid*ethnicity+risk+male, data=dat)
summary(mod1)









