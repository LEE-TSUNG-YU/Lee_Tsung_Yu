library(ggplot2)
diamonds
ggplot(diamonds, aes(clarity, group = cut)) +
  geom_line(aes(color = cut), position="identity", stat = "count")
df <- read.csv("C:/github_LTY/Lee_Tsung_Yu/MyPaper/vessel data/CFD8600/CFD8600.csv")
df["DraftTrim"] <- paste0(df[,"Draft"], ", ", df[,"Trim"])
df["DraftV"] <- paste0(df[,"Draft"], ", ", df[,"V"])
df["TrimV"] <- paste0(df[,"Trim"], ", ", df[,"V"])

ggplot(data = df, mapping = aes(x = V, y = PE, group = DraftTrim)) + 
  geom_line(aes(color = DraftTrim))

ggplot(data = df, mapping = aes(x = Trim, y = PE, group = DraftV)) + 
  geom_line(aes(color = DraftV))

ggplot(data = df, mapping = aes(x = Draft, y = PE, group = TrimV)) + 
  geom_line(aes(color = TrimV))

library(tidyverse)
super_nba_stars <- c("Steve Nash", "Michael Jordan", "LeBron James", "Dirk Nowitzski", "Hakeem Olajuwon")
lbj <- super_nba_stars %>% 
  strsplit(split = " ") %>% 
  `[[` (3) %>% 
  `[` (2) %>% 
  toupper()
lbj

df %>% group_by(Draft) %>% summarise(mean(PE))
df %>% group_by(Trim) %>% summarise(mean(PE))
df %>% group_by(V) %>% summarise(mean(PE))

df %>% group_by(Draft) %>% summarise(meanPE = mean(PE)) %>% 
  ggplot(mapping = aes(x = Draft, y = meanPE)) + geom_line()

df %>% group_by(Trim) %>% summarise(meanPE = mean(PE)) %>% 
  ggplot(mapping = aes(x = Trim, y = meanPE)) + geom_line()

df %>% group_by(V) %>% summarise(meanPE = mean(PE)) %>% 
  ggplot(mapping = aes(x = V, y = meanPE)) + geom_line()
df %>% arrange(DraftV)
