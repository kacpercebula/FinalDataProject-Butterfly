library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(knitr)
library(insight)


rm(list = ls())

setwd("~/PierisData")

dfPieris <- read_excel("CompletePierisData_2022-03-09.xlsx", .name_repair = "universal")

#Cleaning the data
#renaming
dfPieris[dfPieris == 'male'] <- 'M'
dfPieris[dfPieris == 'male?'] <- 'M'
dfPieris[dfPieris == 'Male?'] <- 'M'
dfPieris[dfPieris == 'Male'] <- 'M'
dfPieris[dfPieris == 'female'] <- 'F'
dfPieris[dfPieris == 'F?'] <- 'F'
dfPieris[dfPieris == 'Female'] <- 'F'
dfPieris[dfPieris == 'Female?'] <- 'F'

#excluding null values
!dfPieris==" "
!dfPieris=="unknown"
!dfPieris=="N/A"

#making numeric values
dfPieris$LWingLength <- as.numeric(dfPieris$LWingLength)
dfPieris$LWingWidth <- as.numeric(dfPieris$LWingWidth)
dfPieris$RWingLength <- as.numeric(dfPieris$RWingLength)
dfPieris$RWingWidth <- as.numeric(dfPieris$RWingWidth)
dfPieris$RBlackPatchApex <- as.numeric(dfPieris$RBlackPatchApex)
dfPieris$LBlackPatchApex <- as.numeric(dfPieris$LBlackPatchApex)
dfPieris$DecimalLatitudeUpdated <- as.numeric(dfPieris$DecimalLatitudeUpdated)
dfPieris$LAnteriorSpotM3 <- as.numeric(dfPieris$LAnteriorSpotM3)
dfPieris$RAnteriorSpotM3 <- as.numeric(dfPieris$RAnteriorSpotM3)


##1/ How does sex affect wing length/width? apex area? anterior spot area?
#Wings:
dfWing <- dfPieris %>%
  select(coreid, SexUpdated, LWingLength, LWingWidth, RWingLength, RWingWidth) %>%
  na.omit(dfWing)%>%
  distinct()

#data frame and calculations for males
dfWingM <- subset(dfWing, SexUpdated =='M')
  LWingLengthM <- mean(dfWingM$LWingLength)
  LWingWidthM <- mean(dfWingM$LWingWidth)
  RWingLengthM <- mean(dfWingM$RWingLength)
  RWingWidthM <- mean(dfWingM$RWingWidth)

#data frame and calculations for females
dfWingF <- subset(dfWing, SexUpdated =='F')
  LWingLengthF <- mean(dfWingF$LWingLength)
  LWingWidthF <- mean(dfWingF$LWingWidth)
  RWingLengthF <- mean(dfWingF$RWingLength)
  RWingWidthF <- mean(dfWingF$RWingWidth)
  
#Find average for left and right
#males
WingLengthM <- (LWingLengthM + RWingLengthM)/2
WingWidthM <- (LWingWidthM + RWingWidthM)/2

#females
WingLengthF <- (LWingLengthF + RWingLengthF)/2
WingWidthF <- (LWingWidthF + RWingWidthF)/2


#Apex area: 
dfApex <- dfPieris %>%
  select(coreid, SexUpdated, RBlackPatchApex, LBlackPatchApex) %>%
  na.omit(dfApex)%>%
  distinct()

#data frame and calculations for males
dfApexM <- subset(dfApex, SexUpdated =='M')
  RBlackPatchApexM <- mean(dfApexM$RBlackPatchApex)
  LBlackPatchApexM <- mean(dfApexM$LBlackPatchApex)

#data frame and calculations for females
dfApexF <- subset(dfApex, SexUpdated =='F')
  RBlackPatchApexF <- mean(dfApexF$RBlackPatchApex)
  LBlackPatchApexF <- mean(dfApexF$LBlackPatchApex)

#Averages for both sex's
BlackPatchApexM <- (RBlackPatchApexM + LBlackPatchApexM)/2
BlackPatchApexF <- (RBlackPatchApexF + LBlackPatchApexF)/2


#Anterior Spot Area: 
dfAnteriorSpot <- dfPieris %>%
  select(coreid, SexUpdated, LAnteriorSpotM3, RAnteriorSpotM3) %>%
  na.omit(dfAnteriorSpot)%>%
  distinct()

#data frame and calculations for males
dfAnteriorSpotM <- subset(dfAnteriorSpot, SexUpdated =='M')
  LAnteriorSpotM <- mean(dfAnteriorSpot$LAnteriorSpotM3)
  RAnteriorSpotM <- mean(dfAnteriorSpot$RAnteriorSpotM3)

#data frame and calculations for females
dfAnteriorSpotF <- subset(dfAnteriorSpot, SexUpdated =='F')
  LAnteriorSpotF <- mean(dfAnteriorSpot$LAnteriorSpotM3)
  RAnteriorSpotF <- mean(dfAnteriorSpot$RAnteriorSpotM3)

#Averages for both sex's
AnteriorSpotM <- (LAnteriorSpotM + RAnteriorSpotM)/2
AnteriorSpotF <- (LAnteriorSpotF + RAnteriorSpotF)/2


#Merging all data into a table  
dfTempSexComparison <- data.frame(Wing.Length = c(WingLengthM, WingLengthF), 
                    Wing.Width = c(WingWidthM, WingWidthF), 
                    BlackPatchApex = c(BlackPatchApexM, BlackPatchApexF), 
                    AnteriorSpotM3 = c(AnteriorSpotM, AnteriorSpotF), 
                    row.names = c("M", "F"))
knitr::kable(dfTempSexComparison, format = "markdown")
export_table(dfTempSexComparison, format = "md")

#Making a graphable chart using dfTempSexComparison
gender <- c("M", "M", "M", "M", "F", "F", "F", "F")
characteristics <- c("WL", "WW", "Apex", "Spot", "WL", "WW", "Apex", "Spot")
values <- c("23.51381","13.04842", "8.615322", "1.987908", "23.18983", "13.14500", "10.438207", "1.987908")
dfFinalSexComparison <- data.frame(gender, characteristics, values)

# Convert the values column to numeric
dfFinalSexComparison$values <- as.numeric(dfFinalSexComparison$values)

# Create the side-by-side bar chart using ggplot2
plGenderComparison <- ggplot(dfFinalSexComparison, aes(x = characteristics, y = values, fill = gender)) +
  geom_col(position = "dodge", color = "black")
plGenderComparison + labs(x = "Characteristics", y = "Values (mm)", fill = "Gender") + ggtitle("Gender Comparison")

###Question 1 done


##2 How does location (latitude, longitude, or territory) affect: Wing length/width? Apex area? Anterior spot area?
#Wing length/width
dfLocationWings <- dfPieris %>%
  select(coreid, DecimalLatitudeUpdated, LWingLength, LWingWidth, RWingLength, RWingWidth)%>%
  na.omit(dfLocationWings)%>%
  distinct()

#Find the mean of each row 
dfLocationWings <- dfLocationWings %>%
  mutate(WingLength = (LWingLength + RWingLength) / 2)
dfLocationWings <- dfLocationWings %>%
  mutate(WingWidth = (LWingWidth + RWingWidth) / 2)

##Apex Area
dfLocationApex <- dfPieris %>%
  select(coreid, DecimalLatitudeUpdated, LBlackPatchApex, RBlackPatchApex)%>%
  na.omit(dfLocationApex)%>%
  distinct()

#Find the mean of each row 
dfLocationApex <- dfLocationApex %>%
  mutate(BlackPatchApex = (LBlackPatchApex + RBlackPatchApex) / 2)

##Anterior Spot Area
dfLocationAnt <- dfPieris %>%
  select(coreid,SexUpdated, DecimalLatitudeUpdated, RAnteriorSpotM3, LAnteriorSpotM3)%>%
  na.omit(dfLocalisationAnterior)%>%
  distinct()

#Find the mean of each row 
dfLocationAnt <- dfLocationAnt %>%
  mutate(AnteriorSpot = (RAnteriorSpotM3 + LAnteriorSpotM3) / 2)

##Plots
# Length plot
plLatitudeWingLength <- ggplot(dfLocationWings, aes(x = DecimalLatitudeUpdated, y = WingLength)) +
   geom_point(color = "blue") + geom_smooth()
plLatitudeWingLength + labs(y = "Wing Length (mm)", x = "Decimal Latitude") + ggtitle("Decimal Latitude vs. Wing Length")

# Width plot
plLatitudeWingWidth <- ggplot(dfLocationWings, aes(x = DecimalLatitudeUpdated, y = WingWidth)) +
  geom_point(color = "red") + geom_smooth()
plLatitudeWingWidth + labs(y = "Wing Width (mm)", x = "Decimal Latitude") + ggtitle("Decimal Latitude vs. Wing Width")

# Apex plot 
plLatitudeApex <- ggplot(dfLocationApex, aes(x = DecimalLatitudeUpdated, y = BlackPatchApex)) +
  geom_point(color = "black") + geom_smooth()
plLatitudeApex + labs(y = "Apex Location", x = "Decimal Latitude") + ggtitle("Decimal Latitude vs. Apex Location")

# Spot plot
plLatitudeAnt <- ggplot(dfLocationAnt, aes(x = DecimalLatitudeUpdated, y = AnteriorSpot)) +
  geom_point(color = "purple") + geom_smooth()
plLatitudeAnt + labs(y = "Anteior Spot Location", x = "Decimal Latitude") + ggtitle("Decimal Latitude vs. Anterior Spot Location")

### Question 2 done


#3 What is the relationship between apex area and spot area
#Apex and Spot data frame
dfApexSpotCompare <- dfPieris %>%
  dplyr::select(LBlackPatchApex,RBlackPatchApex, LAnteriorSpotM3, RAnteriorSpotM3) %>%
  na.omit(dfapex_spot) %>%
  distinct()

#Find the mean of each row 
dfApexSpotCompare <- dfApexSpotCompare %>%
  mutate(Apex = (LBlackPatchApex + RBlackPatchApex) / 2)
dfApexSpotCompare <- dfApexSpotCompare %>%
  mutate(Spot = (LAnteriorSpotM3 + RAnteriorSpotM3) / 2)

#Simplify data frame
dfApexSpotCompare <- dfApexSpotCompare %>%
  select(Apex, Spot)

# Apex Spot plot 
plApexSpot <- ggplot(dfApexSpotCompare, aes(x = Apex, y = Spot)) +
  geom_point(color = "darkred") + geom_smooth(method=lm, se=FALSE)
plApexSpot + labs(y = "Spot (mm)", x = "Apex (mm)") + ggtitle("Apex area vs. Anterior Spot")

###Questions 3 done


#4 How does collection month or year affect:
##Wing length/width
dfYearWing <- dfPieris %>%
  select(coreid,YearUpdated,LWingLength, LWingWidth, RWingLength, RWingWidth)%>%
  na.omit(dfYearWing)%>%
  distinct()
dfYearWing <- subset(dfYearWing, YearUpdated != '200')

#Find the mean of each row 
dfYearWing <- dfYearWing %>%
  mutate(WingLength = (LWingLength + RWingLength) / 2)
dfYearWing <- dfYearWing %>%
  mutate(WingWidth = (LWingWidth + RWingWidth) / 2)

#Simplify table 
dfYearWing <- dfYearWing %>%
  select(YearUpdated, WingLength, WingWidth)

##Apex Area
dfYearApex <- dfPieris %>%
  select(coreid,YearUpdated,LBlackPatchApex, RBlackPatchApex)%>%
  na.omit(dfYearApex)%>%
  distinct()
dfYearApex <- subset(dfYearApex, YearUpdated != '200')

#Find the mean of each row 
dfYearApex <- dfYearApex %>%
  mutate(BlackPatchApex = (LBlackPatchApex + RBlackPatchApex) / 2)

#Simplify table 
dfYearApex <- dfYearApex %>%
  select(YearUpdated, BlackPatchApex)

##Anterior Spot Area
dfYearAnterior <- dfPieris %>%
  select(coreid,YearUpdated,LAnteriorSpotM3, RAnteriorSpotM3)%>%
  na.omit(dfYearAnterior)%>%
  distinct()
dfYearAnterior <- subset(dfYearAnterior, YearUpdated != '200')

#Find the mean of each row 
dfYearAnterior <- dfYearAnterior %>%
  mutate(AnteriorSpotM3 = (LAnteriorSpotM3 + RAnteriorSpotM3) / 2)

#Simplify table 
dfYearAnterior <- dfYearAnterior %>%
  select(YearUpdated, AnteriorSpotM3)

##Plots
# Length plot 
plYearWingLength <- ggplot(dfYearWing, aes(x = YearUpdated, y = WingLength)) + 
  geom_point(color = "blue") + geom_smooth()
plYearWingLength + labs(y = "Wing Length (mm)", x = "Year") + ggtitle("How does Year affect Wing Length?")

# Width plot
plYearWingWidth <- ggplot(dfYearWing, aes(x = YearUpdated, y = WingWidth)) + 
  geom_point(color = "red") + geom_smooth()
plYearWingWidth + labs(y = "Wing Width (mm)", x = "Year") + ggtitle("How does Year affect Wing Width?")

#Apex plot
plYearApex <- ggplot(dfYearApex, aes(x = YearUpdated, y = BlackPatchApex)) + 
  geom_point(color = "black") + geom_smooth()
plYearApex + labs(y = "Apex (mm)", x = "Year") + ggtitle("How does Year affect Apex?")

# Spot plot 
plYearSpot <- ggplot(dfYearAnterior, aes(x = YearUpdated, y = AnteriorSpotM3)) + 
  geom_point(color = "purple") + geom_smooth()
plYearSpot + labs(y = "Anterior Spot (mm)", x = "Year") + ggtitle("How does Year affect Anterior Spot?")

###Question 4 done

#T-test: find the mean male LWingLength 
t.test(dfWingM$LWingLength, mu= mean(dfWingM$LWingLength), alternative = "less")
dim(dfWingM)
