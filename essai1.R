
library(plotly)
df <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")

col1 <- df$Smokes
print(col1)
naCol1 <- is.na(col1)
print(naCol1)
sum(naCol1, na.rm = TRUE)
length(col1)

round(sum(is.na(df$Smokes), na.rm = TRUE) / length(df$Smokes) * 100,digits = 2)

res <- 0
for (i in names(df)) {
  col <- df[,i]
  res[i] = round(sum(is.na(col), na.rm = TRUE) / length(col) * 100,digits = 2)
}
res <- res[-1]
plot_ly(x = names(res), y = res, name = "Pourcentage of NAs in each column", type = "bar")

resColo <- 0
for (i in names(res)){
  if (res[i] < 15) resColo[i] = i
}
resColo <- resColo[-1]
print(resColo)
df[,resColo]

















