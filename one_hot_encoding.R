library(caret)

q <- read.csv(file = "dropnzvdata.csv")

dmy <- dummyVars(" ~ .", data = q,fullRank = T, sep=".")
df_transformed <- data.frame(predict(dmy, newdata = q))


write.csv(df_transformed, file = "onehotdata.csv", row.names = F)