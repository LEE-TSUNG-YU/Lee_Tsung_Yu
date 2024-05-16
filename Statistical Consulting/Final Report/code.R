rawdata <- read.csv("C:/github_LTY/Lee_Tsung_Yu/Statistical Consulting/Final Report/data/laptop.csv")
rawdata[rawdata == ""] <- NA

# drop columns
# X, Name
# c(1, 2, )
table(rawdata$Brand)
unique(rawdata$Processor_brand)



