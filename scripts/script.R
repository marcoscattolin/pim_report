library(tidyverse)
library(stringr)
library(readxl)




clx <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Prada") %>% 
        bind_rows(read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Miu Miu"))

anagrafica <- read_csv2("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/FLUIID4_ANAGRAFICA_TOT.CSV", col_types = cols(.default = col_character()))


output_file <- "k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/clx_reshape.csv"

anagrafica %>% 
        select(ID_ARTICOLO,SKUs) %>% 
        mutate(sku = str_split(string = SKUs, pattern = ",")) %>% 
        unnest(sku) %>% 
        select(-SKUs) %>% 
        left_join(clx %>% select(-sku), by = c("ID_ARTICOLO" = "uid")) %>% 
        distinct() %>% 
        write.csv(na = "", row.names = F, file = output_file)



