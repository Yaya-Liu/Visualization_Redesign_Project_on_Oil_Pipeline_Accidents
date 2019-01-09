###############################################################

# Visulization Redesign on Oil Pipeline Accidents

###############################################################

library(tidyverse)
library(ggrepel)
library(data.table)


## Data Clean 
# Read the original dataset, remove rows 2794 and 2795(records about 2017) 
df1 <- fread(file = "C:\\Rdata\\Redesign_Project\\database.csv",
            select = c("Accident Year", "Accident Date/Time", "Operator ID",
                       "Operator Name", "Accident State", "Cause Category", "Net Loss (Barrels)",
                       "All Costs"),
            na.strings=c(""))[-c(2794, 2795)]

# Delete rows including missing values.
OilAccidents <- na.omit(df1)

# Rename selected columns
names(OilAccidents) <- c("Accident_Year", "Accident_Date_Time", "Operator_ID","Operator_Name",
                         "Accident_State", "Cause_Category","Net_Loss", "All_Costs")

# Check data
OilAccidents
head(OilAccidents)

# Chage data type for OilAccidents$Operator_ID
OilAccidents$Operator_ID <- as.character(OilAccidents$Operator_ID)
summary(OilAccidents)

# Graph1 : Line Chart number of accidents happened in each year(2006-2010). Author: Yaya Liu
AccidentTable <- table(OilAccidents$Accident_Year)
AccidentFrame = as.data.frame(AccidentTable)
summary(AccidentFrame)

names(AccidentFrame)[1] = 'XYear'
names(AccidentFrame)[2] = 'YFreq'

ggplot(AccidentFrame, aes(x = AccidentFrame$XYear, y = AccidentFrame$YFreq)) +
  geom_point(shape = 16, color = c( "orange", "khaki4", "deepskyblue1", "steelblue4",
                                    "blue", "Red", "purple"), size = 3) + 
  geom_line(group = 1, size = 0.8, linetype = "solid",
            color = c("khaki4", "deepskyblue1","steelblue4","blue", "Red", "purple", "coral1")) +
  geom_label(label = AccidentFrame$YFreq, 
             label.padding = unit(0.15, "lines"), 
             color = c( "orange", "khaki4", "deepskyblue1","steelblue4","blue", "Red", "purple")) +

  ylim(320, 480) +
  labs(
    x = "Year",
    y = "Count",
    title = "Number of Accidents per year in US"
  ) + 
  theme(plot.title = element_text(hjust = 0.5)) 


# Graph2: Bar chart shows occurance of different causes
OilAccidents <- within(OilAccidents,Cause_Category <- factor(Cause_Category,
                                                             levels=names(sort(table(Cause_Category), 
                                                                               decreasing=FALSE))))

windows(width=8,height=4) 
ggplot(OilAccidents, aes(x = Cause_Category, fill = as.character(OilAccidents$Accident_Year))) + 
  geom_bar(color = gray(.25), stat = "count", width = 0.5, na.rm = FALSE) +
  scale_fill_manual( values = c( "orange", "khaki1", "deepskyblue2", "steelblue3",
                                 "blue", "Red", "purple")) +
  
  labs(
    x = "Cause",
    y = "Count",
    title = "Cause of Accidents"
  ) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom', legend.direction = 'horizontal') +
  guides(fill=guide_legend(title="Year")) +
  ylim(0, 1450) +
  coord_flip() 


# Graph3: Line chart shows the number of accidents happened in each month
AccidentDates <- as.Date(OilAccidents$Accident_Date_Time, format = "%m/%d/%Y")
month(AccidentDates)
MonthFrame = as.data.frame(table(month(AccidentDates)))
MonthFrame
names(MonthFrame)[1] = 'XMonth'
names(MonthFrame)[2] = 'YFreqMonth'

ggplot(MonthFrame, aes(x = MonthFrame$XMonth, y = MonthFrame$YFreqMonth)) +
  #geom_point(shape = 16, size = 3, color = ifelse(MonthFrame$YFreqMonth >= 250, "red", "blue")) + 
  geom_line(group = 1, size = 0.8, linetype = "solid", color = "darkgrey") +
  geom_label(label = MonthFrame$YFreq, label.size = 0.05, 
             color = ifelse(MonthFrame$YFreqMonth >= 240, "red", "blue"),
             label.padding = unit(0.15, "lines")) +
  labs(
    x = "Month",
    y = "Count",
    title = "Number of Accidents per Month"
  )


#Graph 4 Redesign: Create coningnecy table(Operator IDs, Accident of numbers), total 211 operaters.
ot = table(OilAccidents$Operator_ID)
ot
otsort_y <- sort(as.numeric(ot))
ot_x <- c(1:211)

otdata <- tibble(ot_x, otsort_y)

#Get the 10 worst operator IDs which caused the most accidents
OperatorIDs <- names(ot)[order(ot)]
WorstOperatorIDs <- OperatorIDs[length(OperatorIDs)-0:9]
WorstOperatorIDs

#Get the lable for points
IDlabels <- c(rep(NA,length(otsort_y)-10),rev(WorstOperatorIDs))

windows(width=8,height=8)
ggplot(data = otdata, aes(x=ot_x, y=otsort_y)) +
  geom_point(shape = 21, color = gray(.25), fill = ifelse(otsort_y > 80, "red", "blue"), size = 3) +
  labs(
    x = "Ranking of Operator IDs",
    y = "Number of Accidents",
    title = "Operator IDs and the Number of Accidents"
  ) + 
  geom_label_repel(aes(label =  IDlabels),
                   box.padding   = 1, 
                   point.padding = 0.2,
                   segment.color = 'grey50') +
theme(plot.title = element_text(hjust = 0.5)) 

#Graph5 Bar graph shows the operaters grouped by the number of Accidents.
act = table(ot)
actdata = as.data.frame(act)
names(actdata)[1] = 'Accident_Numbers'
names(actdata)[2] = 'Operator_Numbers'
actdata

windows(width=8,height=4) 
ggplot(data = actdata, aes(x = Accident_Numbers, y = Operator_Numbers, fill = Operator_Numbers)) +
  geom_bar(stat = "identity", width = 0.3) +
  labs(
    x = "Number of Accidents",
    y = "Number of Operator IDs",
    title = "Operator IDs Grouped by the Number of Accidents"
  ) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5),legend.position = 'none')


