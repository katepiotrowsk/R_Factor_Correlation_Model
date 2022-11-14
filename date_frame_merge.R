df1 = read.table(text = 'Date  ID
1980-12-01  2
1982-11-01  1
1990-01-01  6
1993-07-01  9', header = TRUE)

df2 = read.table(text = 'Date  Var
1980-12-17  8
1982-11-07  9
1990-01-19  10
1993-07-20  22', header = TRUE)

df1$my=format(as.Date(df1$Date), "%Y-%m")
df2$my=format(as.Date(df2$Date), "%Y-%m")
merge(df1,df2, by='my')