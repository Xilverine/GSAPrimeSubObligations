library(dplyr)
library(magrittr)
library(readr)
library(naniar)
library(tibble)
getwd()
###"/Users/shannafrierson/Desktop/R Projects/Projects/BBPresents"


# does tibble default blank cells in data frame to NA? 
psdir_data <- read_csv(file = "primesub_directory2019.csv")
psdir_data %>% as_tibble(psdir_data)
view(psdir_data)

# return all rows with "NA", very small sample
NA_rows <- view(psdir_data[rowSums(is.na(psdir_data)) > 0,])

# select specific rows from data frame, rename
psdir_table <- psdir_data %>% 
    select(2:9, 11, 16, 18:19) %>% 
    mutate(Dollars = as.numeric(Dollars)) %>% 
    rename(PSC = 'Product or Service Code', Complete_Date = 'Est. Ultimate Completion Date',
           PSC_Desc = 'Product or Service Description', 
           POP_City = 'Principal Place of Performance City Name', 
           POP_State = 'Principal Place of Performance State Code', NAICS_Code = 'NAICS Code', Dollars = 'Action Obligation')

# psdir_table$Dollars <- as.numeric(as.character('Dollars'))     
as.numeric(gsub("[^0-9.]", "", Dollars))
view(psdir_table)



head(psdir_table)






#save
savehistory(file = "primesub_directory2019.csv")
                    
    


