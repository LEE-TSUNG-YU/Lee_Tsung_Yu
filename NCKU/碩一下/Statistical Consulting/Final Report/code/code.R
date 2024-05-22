rawdata <- read.csv("C:/github_LTY/Lee_Tsung_Yu/碩一下/Statistical Consulting/Final Report/data/laptop.csv")
rawdata[rawdata == ""] <- NA

# drop columns
# X, Name
# c(1, 2, )
table(rawdata$Brand)
unique(rawdata$Brand)
unique(rawdata$Processor_brand)
unique(rawdata$Processor_name)
unique(rawdata$Processor_variant)
unique(rawdata$RAM_type)
unique(rawdata$Storage_capacity_GB)
unique(rawdata$Storage_type)
unique(rawdata$Graphics_name)
unique(rawdata$Graphics_brand)
unique(rawdata$Graphics_integreted)
unique(rawdata$Horizontal_pixel)
unique(rawdata$Vertical_pixel)
unique(rawdata$Touch_screen)
unique(rawdata$Operating_system)


