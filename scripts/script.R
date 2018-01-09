library(tidyverse)
library(stringr)
library(readxl)




# READ FILES --------------------------------------------------------------
clx <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Prada") %>% 
        bind_rows(read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Miu Miu"))

anagrafica <- read_csv2("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/FLUIID4_ANAGRAFICA_TOT.CSV",quote = "", col_types = cols(.default = col_character()))



# REMOVE QUOTES -----------------------------------------------------------
anagrafica <- anagrafica %>% 
        mutate_all(~ gsub("\"$","",.)) %>% 
        mutate_all(~ gsub("^\"","",.))



# FILTER OUT INVALID PLACEHOLDERS  ----------------------------------------
clx <- clx %>% 
        separate(col = "file_name",into = c("tmp","placeholder"), extra = "drop", sep = "_") %>% 
        select(-tmp) %>% 
        filter(nchar(placeholder) == 3)




# WRITE FILES -------------------------------------------------------------
output_file <- "k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/clx_reshape.csv"

out <- anagrafica %>% 
        select(ID_ARTICOLO,SKUs) %>% 
        mutate(sku = str_split(string = SKUs, pattern = ",")) %>% 
        unnest(sku) %>% 
        select(-SKUs) %>% 
        left_join(clx %>% select(-sku), by = c("ID_ARTICOLO" = "uid")) %>% 
        distinct() 


out %>% 
        write.csv(na = "", row.names = F, file = output_file)



