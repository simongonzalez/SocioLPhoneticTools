library(tidyverse)

#joining columns across files

# read in the two csvs
dfmodals <- read.csv("Modals-220714_ALL.csv", stringsAsFactors = TRUE)
dfVerb <- read.csv("Verb1Plus.csv", stringsAsFactors = TRUE)

dfmetadata_merge <- dfmetadata %>% 
  select(Filename,YearOfBirth,Occupation_AUSEI06_Score,SESComposite,Composite.score.continuous)
summary(dfmetadata_merge)

dfmodals_Verb<- left_join(dfmodals, dfVerb, by = "Verb")
write.csv(dfmodals_Verb, "dfmodals_Verb.csv") #write a csv, in the working directory



summary(dfAngChinALL)
summary(dfAngChinCoded)

#select the relevant columns to copy across; include the unique identifier that will match and columns of interest
dfAngChinCoded_merge <- dfAngChinCoded %>% 
  select(URL,Target.PNT,
         SocialNetwork_narrow,SocialNetwork_broad,PercentageAnglo,EO_content,EO_overall,CantoLevel,SuburbComunityLang,AgeCommunity,
         Realisation,IUPosition,Bigram,FrequencyFollowingWord,TransProbBW,TransProb,
         FollowingWordOriginal,FollowingWord,WordClassOrig,WordClass,
         SyllableStress,FollowingVowelOrig,FollowingVowel,FollV_HighLow_BackFront,FollV_MonoDipth,FollowingVowel_fourway,
         GQ,CT,BP,Glottal)
summary(dfAngChinCoded_merge)

#join the two csvs together by a unique identifier for each token; check that N of obs is the same as first sheet
dfAngChinMod <- left_join(dfAngChinALL, dfAngChinCoded_merge, by = "URL")

summary(dfAngChinMod)
view(dfAngChinMod) #view spreadsheet in another tab in R
write.csv(dfAngChinMod, "dfAngChinMod8.csv") #write a csv, in the working directory


