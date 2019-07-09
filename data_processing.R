library(caret)
library(tidyverse)
library(reshape2)


load("Data from Backdive-2.Rdata")

threshold = 0.01
D = merge
dim(D)
D = unique(D)
dim(D)
#rm(merge)
#remove NA values
inds.na = which(!is.na(D$value))
D = D[inds.na,]
id_len=length(unique(D$bacdive_id))
D$new_field = paste(D$section, D$subsection, D$field)
save(D, file = "../DATA/PROCESSED/D.Rdata")
count=as.data.frame(table(D$new_field))#combination of subsection and field
var1=count$Var1
freq=count$Freq
var<-list()
freqs<-list()
for (i in 1:length(freq)) {
  if (freq[i]>=threshold*id_len) {#this will get fields greater than 1%
    var[[i]]=var1[i]
    freqs[[i]]=freq[i]
  }
}

df=do.call(rbind, Map(data.frame, Feature_name=var, Frequency=freqs))

df$Fraction = df$Frequency/id_len
###get those seen at least 20% of time
df20 = subset(df, Fraction >= 0.2)
# write.csv(df20, file ="variables_20_percent_coverage.csv")
# 
# ###get those seen at least 10% of time
# df10 = subset(df, Fraction >= 0.1)
# write.csv(df10, file ="variables_10_percent_coverage.csv")
# 
# ###get those seen at least 5% of time
# df05 = subset(df, Fraction >= 0.05)
# write.csv(df05, file ="variables_5_percent_coverage.csv")
# 
# ###get those seen at least 1% of time
# df01 = subset(df, Fraction >= 0.01)
# save(df01, file = "../DATA/PROCESSED/df01.Rdata")

df <- merge

df <- unique(df)

df$feature_name <- paste(df$section, df$subsection, df$field)

a <- df[is.element(df$feature_name, df20$Feature_name),]
# df <- df %>% distinct(bacdive_id, section, subsection, field, species, .keep_all = T)

q <- dcast(a, bacdive_id~feature_name)
q <- q[order(q$bacdive_id),]

write.csv(q, file = "df.csv", row.names = F)

# remove columns with near zero variance Global
nzv <- nearZeroVar(q,saveMetrics=TRUE,freqCut = 95/5)
nzv <- row.names(nzv[which(nzv$nzv==TRUE),])
dropnzv<-names(q[ , which(names(q) %in% nzv)])
q<-q[ , -which(names(q) %in% nzv)]
write.csv(q, file = "dropnzvdata.csv", row.names = F)

dmy <- dummyVars(" ~ .", data = q,fullRank = T, sep=".")
df_transformed <- data.frame(predict(dmy, newdata = q))


write.csv(df_transformed, file = "onehotdata.csv", row.names = F)



