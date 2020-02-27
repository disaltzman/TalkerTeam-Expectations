rm(list=ls()) #clear all variables
setwd("/Volumes/netapp/MyersLab/Sahil/TalkerTeam/Expectations/randomizations")

files <- as.data.frame(list.files())
colnames(files) <- "files"
#

for(n in 1:length(files$files)){
  randomization <- read.csv(as.character(files[n,]))
  subject <- unlist(strsplit(as.character(files[n,]),split = ".", fixed = TRUE))[1]
  mixed <- randomization[1:48,]
  blockedm150 <- randomization[49:72,]
  blockedm160 <- randomization[73:96,]
  write.csv(mixed, paste0(subject,"mixed.csv"), row.names = FALSE)
  write.csv(blockedm150, paste0(subject,"blockedm150.csv"), row.names = FALSE)
  write.csv(blockedm160, paste0(subject,"blockedm160.csv"), row.names = FALSE)
}

