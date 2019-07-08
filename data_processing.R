library(caret)
library(tidyverse)
library(reshape2)

load("../Bacteria_Data/Data from Backdive-2.Rdata")
df <- merge
df <- df[0:10000,]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df <- df %>% distinct(bacdive_id, section, subsection, field, species, .keep_all = F)

# q <- dcast(df, bacdive_id~section+subsection+field, fun.aggregate = Mode, drop = F)
q <- dcast(df, bacdive_id~section+subsection+field+species)
# save(q, file = "../Bacteria_Data/Data from Backdive-2 Processed.Rdata")
write.csv(q, file = "data.csv", row.names = F)

# remove columns with near zero variance Global
nzv <- nearZeroVar(q,saveMetrics=TRUE,freqCut = 95/5)
nzv <- row.names(nzv[which(nzv$nzv==TRUE),])
dropnzv<-names(q[ , which(names(q) %in% nzv)])
q<-q[ , -which(names(q) %in% nzv)]
write.csv(q, file = "dropnzvdata.csv", row.names = F)

dmy <- dummyVars(" ~ .", data = q)
trsf <- data.frame(predict(dmy, newdata = q))