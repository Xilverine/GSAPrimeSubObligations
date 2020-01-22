library(dplyr)
library(magrittr)
library(readr)
library(naniar)
library(tibble)
library(ggplot2)
library(pastecs)

getwd()
###"C:/Users/sf110866/Desktop/R_Projects/SubcontractObligations"


# does tibble default blank cells in data frame to NA? 
psdir_data <- read_csv(file = "primesub_directory2019.csv")
psdir_data %>% as_tibble(psdir_data)
view(psdir_data)


# select specific rows from data frame, rename, and converted dollars to numeric
psdir_table <- psdir_data %>% 
    select(2:9, 11, 16, 18:19) %>% 
    rename(PSC = 'Product or Service Code', Complete_Date = 'Est. Ultimate Completion Date',
           PSC_Desc = 'Product or Service Description', 
           POP_City = 'Principal Place of Performance City Name', 
           POP_State = 'Principal Place of Performance State Code', NAICS_Code = 'NAICS Code', 
           Total_Dollars = 'Action Obligation') %>% 
    mutate(Total_Dollars = as.numeric(gsub("[^0-9.]", "", Total_Dollars))) %>% 
    mutate(NAICS_Code = as.factor(as.numeric(NAICS_Code))) %>% 
    mutate(NAICS_short = substr(NAICS_Code, 1, 2)) %>% 
    mutate(NAICS_short = as.factor(as.character(NAICS_short))) %>% 
    group_by(NAICS_Code)

count(psdir_table$NAICS_short)

view(psdir_table)
class(psdir_table$NAICS_Code)
class(psdir_table$Total_Dollars)

# return all rows with "NA", very small sample
NA_rows <- view(psdir_data[rowSums(is.na(psdir_data)) > 0,])
    
head(psdir_table)
mean(psdir_table$Total_Dollars)
summary(psdir_table$NAICS_short)



bppsdir <- boxplot(Total_Dollars ~ NAICS_short, data = psdir_table,
        xlab = "NAICS_short", ylab = "Total_Dollars",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))





res.psdir <- aov(Total_Dollars ~ NAICS_short, data = psdir_table)
summary(res.psdir)
TukeyHSD(res.psdir)





psdir_lm <- ggplot2(psdir_table, aes_(x='NAICS_Code', y= 'Total_Dollars') + geom_point() + geom_smooth(method="lm"))

#experimenting with pastecs, but not particular useful for this case
stat.desc(psdir_table)
#stat.desc(psdir_table[,c("Total_Dollars","NAICS_Code","POP_State")])
#stat.desc(psdir_table[,c("Total_Dollars","NAICS_Code","POP_State")], basic=TRUE, desc=TRUE, norm=TRUE, p=0.95)






