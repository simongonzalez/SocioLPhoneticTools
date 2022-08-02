# R scripting workshop

# Step one: Create R script file and save this 
#           in the same folder your data is located in

# Step two: Set working directory to where your data and script are:
#          Session > Set Working Directory > To Source File Location

setwd("C:/Users/u3487939/Dropbox/workshops/r") # Thhis is the script way to set a working direction

#######################
# Here's where you list the packages you'll need to load
library(tidyr)
library(ggplot2)

# Load in the .csv files you want to analyse

ssds <- read.csv("df_SSDS_Anglonormed.csv",T) # Loads the SSDS Anglo .csv 
syds <- read.csv("df_SydS_Anglonormed.csv",T) # Loads the SydS Anglo .csv

# Now that the .csv files are in R, we refer to them as "dataframes" (or "dfs")
summary(ssds$speaker) # Returns a summary of all items in the ssds df
names(ssds) # Returns the titles of the columns of the ssds df

# You'll probably want to merge dfs at some point. Here's how!
df <- rbind(ssds, syds)
merged <- rbind(ssds, syds)
vowels = df

summary(df) # Check that this has worked with a summary
unique(df$speaker) # Returns the unique examples of the specified column 
sort(unique(df$speaker)) # Does the same but alphabetically sorts the output

unique(ssds$speaker)
unique(syds$speaker)

# Here's where we'll talk about merging different datasets 
demo <- read.csv("speakerDemographics.csv", T)
summary(demo)
df_demo <- merge(df, demo, by="speaker")
names(df_demo)
summary(df_demo$filename.x)
summary(df_demo$filename.y)

names(df_demo)[40] = "filename" # Renaming columns
names(df_demo)[45] = "filename_demo" # Renaming columns
View(head(df_demo)) # Just shows the first 6 items in the df
View(df_demo)

# Here's where we'll talk about subsetting
df_demo_50 <- df_demo[df_demo$percentage=="50",]
View(head(df_demo_50))
summary(df_demo_50$percentage)

# write.csv(df_demo_50, "df_demo_50_jg.csv", row.names = FALSE)  # Write a csv

summary(df_demo$age)

adult <- df_demo[df_demo$age=="Adult",]
adult_f_20 <- df_demo[df_demo$age=="Adult" & df_demo$gender=="Female" & df_demo$percentag %in% c("20", "80"),]
not_adults <- df_demo[df_demo$age!="Adult",]

# Here's where we'll talk about assigning a new column based on other columns
df$NEW_COLUMN <- "value" # TEMPLATE
df[df$OLD_COLUMN ,'NEW_COLUMN'] <- 'value'# TEMPLATE

df_demo$AgeGender <- "2010sYoung_Female"
df_demo[df_demo$age=="Adult" & df_demo$gender=="Female", 'AgeGender'] <- "1970sAdult_Female"
df_demo[df_demo$age=="Adult" & df_demo$gender=="Male", 'AgeGender'] <- "1970sAdult_Male"
df_demo[df_demo$age=="Teenager" & df_demo$gender=="Female", 'AgeGender'] <- "1970sTeen_Female"
df_demo[df_demo$age=="Teenager" & df_demo$gender=="Male", 'AgeGender'] <- "1970sTeen_Male"
df_demo[df_demo$age=="Old" & df_demo$gender=="Female", 'AgeGender'] <- "2010sOlder_Female"
df_demo[df_demo$age=="Old" & df_demo$gender=="Male", 'AgeGender'] <- "2010sOlder_Male"
df_demo[df_demo$age=="Young" & df_demo$gender=="Male", 'AgeGender'] <- "2010sYoung_Male"

table(df_demo$age, df_demo$AgeGender)
# Here's where we'll talk about plotting using ggplot

ggplot(data = df_demo[df_demo$percentage=="20" & df_demo$vowel %in% c("FLEECE", "FACE"),], 
       aes(x = age, y = F1_norm, color = gender)) +
  geom_boxplot()+
  scale_y_reverse() +
  theme_bw()+
  scale_color_manual(values = c("midnightblue", "goldenrod"))+
  facet_wrap(~ vowel) 

tokens_fleece_face <- df_demo[df_demo$percentage=="20" & df_demo$vowel %in% c("FLEECE", "FACE"),]
table(tokens_fleece_face$vowel, tokens_fleece_face$age)

# Create a df from a subset of columns


transform(df, NEWCOLUMN = (FORMULA)) # Apply formula and create new column

# Create summary dataset

newDF <- df %>%
  group_by(VARIABLE(S)) %>% 
  summarise(NEWCOLUMN = mean(OLDCOLUMN)) 

