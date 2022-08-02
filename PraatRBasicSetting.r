library(PraatR)

audioLoc = '/Users/u1037706/Documents/griffith/NWAVAP/audio/'

audioPath = function(FileName){ return( paste( audioLoc, FileName, sep="") ) }
OutPath = function(FileName){ return( paste(getwd(), '/temporalFiles/', FileName, sep="") ) }

df = read.csv('df.csv')
df$au_name = paste0(df$fileName, '_', df$gender, '_', df$ses, '.wav')
df$formant_name = paste0(df$fileName, '_', df$gender, '_', df$ses, '.Formant')
df$newMid = df$onsetnew + ((df$offsetnew - df$onsetnew) / 2)
df$rawMid = df$onsetraw + ((df$offsetraw - df$onsetraw) / 2)
df$newF1 = NULL
df$newF2 = NULL
df$rawF1 = NULL
df$rawF2 = NULL

namesdf = names(df)
newDf = data.frame(matrix(nrow = 0, ncol = length(namesdf)))
names(newDf) = namesdf 

for(filei in unique(df$fileName)){
  tmpdf = df[df$fileName == filei,]
  
  formant_name = unique(df$formant_name)
  
  praat('To Formant (burg)...', list(0.0, 5, 5500, 0.025, 50.0), 
        input=audioPath(unique(tmpdf$au_name)), output=OutPath(unique(tmpdf$formant_name)), overwrite=T)
  
  for(i in 1:nrow(tmpdf)){
    tmpdf[i,'newF1'] = as.numeric(praat("Get value at time...",                                                          
                                        arguments = list(1, tmpdf$newMid[i], "Hertz", "Linear"),                          
                                        input=OutPath(unique(tmpdf$formant_name)), simplify=TRUE) )
  }
}