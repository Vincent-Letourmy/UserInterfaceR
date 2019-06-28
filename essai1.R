
library(plotly)
df <- read.csv("risk_factors_cervical_cancer_Copie.csv", header = TRUE, sep = ";")

col1 <- df$Color
print(col1[1])
is.character(col1)
mode(col1)
class(col1[1])
naCol1 <- is.na(col1)
print(naCol1)
sum(naCol1, na.rm = TRUE)
length(col1)

col1 <- df$Age
print(col1[1])
is.numeric(col1)
class(col1[1])

round(sum(is.na(df$Smokes), na.rm = TRUE) / length(df$Smokes) * 100,digits = 2)

res <- 0
for (i in names(df)) {
  col <- df[,i]
  if (is.numeric(col) || is.logical(col)){
    res[i] = round(sum(is.na(col), na.rm = TRUE) / length(col) * 100,digits = 2)
  }
  else {
    a <- 0
    for (j in col) {
      if(j == "") a = a + 1
    }
    
    res[i] = round(a / length(col) * 100,digits = 2)
    
    
  }
}
res <- res[-1]
plot_ly(x = names(res), y = res, name = "Pourcentage of NAs in each column", type = "bar")


TF <- df$Smokes
is.logical(TF)
print(TF)

vect <- 0
bool <- 0
for (i in row.names(df)){
  
  
  a <- 0
  for (j in df[i,]) {
    if(j == "" || is.na(j)) a = a + 1
  }
  bool = a > 0
  
  if(isFALSE(bool)){
    vect[i] = i
  }
}
df <- df[vect,]
df <- df[-1,]
df













