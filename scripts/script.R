library(tidyverse)
library(stringr)
library(readxl)




clx <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Prada") %>% 
        bind_rows(read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Miu Miu"))

anagrafica <- read_csv2("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/FLUIID4_ANAGRAFICA_TOT.CSV", col_types = cols(.default = col_character()))


anagrafica %>% 
        select(ID_ARTICOLO,SKUs) %>% 
        mutate(rev_sku = str_split(string = SKUs, pattern = ",")) %>% 
        unnest(rev_sku) %>% 
        select(-SKUs) %>% 
        left_join(clx,., by = c("uid" = "ID_ARTICOLO")) %>% 
        View()



# test commit
