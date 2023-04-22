# Import data
df <- read.table("data.csv",sep=';',header = TRUE)
dim(df)
colnames(df)

# Select columns
df <- df[,c('Life.expectancy','Alcohol','BMI','GDP','Total.expenditure','Schooling')]
dim(df)
