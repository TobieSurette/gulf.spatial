## Maritimes LFAs, checking the CSV files obtained from Adam Cook

df2 <- read.csv("./build/Maritimes-LFAPolys2.csv")

dim(df1)
dim(df2)

df1 <- read.csv("./build/Maritimes-LFAPolys.csv")
library(PBSmapping)
plotMap(df1)
plotMap(df2)

table(df1$SID)
