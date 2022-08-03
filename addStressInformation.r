library(tidyverse)
library(data.table)

dic <- read.csv("word_dataframe.csv")

names(dic) <- unlist(strsplit('word vowel phones disc', ' '))

dic$phones <- gsub('-', '', dic$phones)

dic <- dic[complete.cases(dic),]

dic <- dic %>% distinct(word, .keep_all = TRUE)

#count vowel per word
vowelsList <- unlist(strsplit('1 2 3 4 5 6 7 8 9 i I E { u @ # V U $ Q', ' '))
vowelsListMap <- setNames(unlist(strsplit('FACE PRICE NURSE CHOICE GOAT MOUTH NEAR SQUARE CURE FLEECE KIT DRESS TRAP GOOSE SCHWA BATH STRUT FOOT THOUGHT LOT', ' ')), unlist(strsplit('1 2 3 4 5 6 7 8 9 i I E { u @ # V U $ Q', ' ')))

vowelCount <- NULL

for(i in 1:nrow(dic)){
  tmpword <- as.character(dic[i,'word'])
  tmpphones <- as.character(dic[i,'phones'])
  tmpphonesSplit <- unlist(strsplit(tmpphones, ''))
  
  vowelCount <- append(vowelCount, length(which(tmpphonesSplit %in% vowelsList)))
}

dic$n <-vowelCount

dic <- dic[dic$n != 0,]

dicMultiple <- dic[dic$n != 1,]

#length(grep("'", dic$phones))

noStressInfo <- unique(dicMultiple[!grepl("'", dicMultiple$phones),'word'])

dic <- dic[-which(dic$word %in% noStressInfo),]

stressVowelV <- NULL
wordLengthV <- NULL
tmpPrevious <- NULL
tmpFollowing <- NULL

#get location of vowel
for(i in 1:nrow(dic)){
  tmpword <- as.character(dic[i,'word'])
  tmpphones <- as.character(dic[i,'phones'])
  
  tmpphonesSplit <- unlist(strsplit(tmpphones, ''))
  
  if(grepl("'", tmpphones)){
    vowelLocations <- which(tmpphonesSplit %in% vowelsList)
    stressLocation <- which(tmpphonesSplit == "'")
    stressVowelLocation <- vowelLocations[which(vowelLocations > stressLocation)[1]]
    stressVowel <- tmpphonesSplit[stressVowelLocation]
    
    stressVowelV <- append(stressVowelV, stressVowel)
    
    tmpphonesSplitNoStress <- tmpphonesSplit[tmpphonesSplit != "'"]
    stressVowelLocationNew <- stressVowelLocation - 1
    wordLength <- length(tmpphonesSplitNoStress)
    wordLengthV <- append(wordLengthV, wordLength)
    
    if(stressVowelLocationNew == 1){
      tmpPrevious <- append(tmpPrevious, '')
    }else{
      tmpPrevious <- append(tmpPrevious, tmpphonesSplitNoStress[stressVowelLocationNew - 1])
    }
    
    if(stressVowelLocationNew == wordLength){
      tmpFollowing <- append(tmpFollowing, '')
    }else{
      tmpFollowing <- append(tmpFollowing, tmpphonesSplitNoStress[stressVowelLocationNew + 1])
    }
  }else{
    vowelLocations <- which(tmpphonesSplit %in% vowelsList)
    stressVowel <- tmpphonesSplit[vowelLocations]
    wordLength <- length(tmpphonesSplit)
    
    stressVowelV <- append(stressVowelV, stressVowel)
    wordLengthV <- append(wordLengthV, wordLength)
    
    if(vowelLocations == 1){
      tmpPrevious <- append(tmpPrevious, '')
    }else{
      tmpPrevious <- append(tmpPrevious, tmpphonesSplit[vowelLocations - 1])
    }
    
    if(vowelLocations == wordLength){
      tmpFollowing <- append(tmpFollowing, '')
    }else{
      tmpFollowing <- append(tmpFollowing, tmpphonesSplit[vowelLocations + 1])
    }
  }
  
}

dic$stressVowel <- stressVowelV
dic$wordLength <- wordLengthV
dic$previous <- tmpPrevious
dic$following <- tmpFollowing

dic[dic$previous == "",'previous'] <- 'initial'
dic[dic$following == "",'following'] <- 'final'

dic$vowel <-vowelsListMap[as.character(dic$stressVowel)]

write.csv(dic, 'dic.csv', row.names = F)

#add stress
df <- read.csv('HE-vowel-data.csv', stringsAsFactors = F)





df$stress <- 'unstressed'

dic2 <- dic
dic2$previous <- gsub('initial', '', dic2$previous)
dic2$following <- gsub('final', '', dic2$following)

for(i in 1:nrow(dic2)){
  df[df$vowel == dic2$vowel[i] & df$word == dic2$word[i] & 
       df$previous == dic2$previous[i] & 
       df$following == dic2$following[i],'stress'] <- 'stressed'  
}

df_stressed <- df[df$stress == 'stressed',]
df_unstressed <- df[df$stress == 'unstressed',]

for(i in 1:nrow(df_unstressed)){
  tmpLine <- df_unstressed[i,]
  
  tmpVowel <- as.character(tmpLine$vowel)
  tmpWord <- as.character(tmpLine$word)
  tmp_prev <- as.character(tmpLine$previous)
  tmp_foll <- as.character(tmpLine$following)
  
  tmpDic <- dic2[dic2$word == tmpWord & dic2$vowel == tmpVowel,]
  
  if(nrow(tmpDic) != 0){
    # if(tmpVowel == 'KIT' & tmpWord == 'abbreviate'){
    #   xvafv
    # }
    tmpVowelDic <- as.character(tmpDic$vowel)
    tmpWordDic <- as.character(tmpDic$word)
    tmp_prevDic <- as.character(tmpDic$previous)
    tmp_follDic <- as.character(tmpDic$following)
    
    #all conditions are met
    if(tmp_prev == tmp_prevDic & tmp_foll == tmp_follDic){
      df_unstressed$stress[i] <- 'stressed'
    }else{
      #test for previous

      #afdgasfaf
      prevContext <- paste0(tmp_prev, tmpVowel)
      prevContextDic <- paste0(tmp_prevDic, tmpVowelDic)

      follContext <- paste0(tmpVowel, tmp_foll)
      follContextDic <- paste0(tmpVowelDic, tmp_follDic)
      if(prevContext == prevContextDic){
        df_unstressed$stress[i] <- 'stressed'
      }else if(follContext == follContextDic){
        df_unstressed$stress[i] <- 'stressed'
      }
      a = 1
    }
  }
  

  
  #asdfafd
}

dfFinal <- rbind(df_stressed, df_unstressed)

write.csv(dfFinal, 'HE-vowel-data.csv', row.names = F)
for(i in 1:nrow(dic)){
  df[df$vowel == dic$vowel[i] & df$word == dic$word[i] & 
       df$boundary == dic$previous[i] & 
       df$following == dic$following[i],'stress'] <- 'stressed'  
}

for(i in 1:nrow(dic)){
  df[df$vowel == dic$vowel[i] & df$word == dic$word[i] & 
       df$previous == dic$previous[i] & 
       df$boundary == dic$following[i],'stress'] <- 'stressed'  
}

write.csv(df, 'df_stressInformation.csv', row.names = F)
