library(tidyverse)
library(stringr)
library(readxl)




# READ FILES --------------------------------------------------------------
clx <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Prada") %>% 
        bind_rows(read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Miu Miu"))

anagrafica <- read_csv2("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/FLUIID4_ANAGRAFICA_TOT.CSV",quote = "", col_types = cols(.default = col_character()))
descrizioni <- read_csv2("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/ProductTextEnrichment.csv", col_types = cols(.default = col_character()))
pim <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Published PIM - master.xlsx", sheet = "Sheet1")


############## PIM MANIPULATE ####################


# GENERATE PRODUCT ID ----------------------------------------------------
pim <- pim %>% 
        separate(col = `Variant no.`, into = c("col1","col2","col3"), remove = F, sep = "_", extra = "drop") %>% 
        select(-col1,-col2) %>% 
        mutate(product_id = str_replace(`Variant no.`,paste0(col3),"") %>% str_replace(.,"__","_") %>% str_replace(.,"_$",""))
        


############## CLX MANIPULATE ####################


# REMOVE QUOTES -----------------------------------------------------------
anagrafica <- anagrafica %>% 
        mutate_all(~ gsub("\"$","",.)) %>% 
        mutate_all(~ gsub("^\"","",.))



# FILTER OUT INVALID PLACEHOLDERS  ----------------------------------------
clx <- clx %>% 
        separate(col = "file_name",into = c("tmp","placeholder"), extra = "drop", sep = "_", remove = F) %>% 
        select(-tmp) %>% 
        filter(nchar(placeholder) == 3)





# RESHAPE OUTFILE ---------------------------------------------------------
clx <- anagrafica %>% 
        select(ID_ARTICOLO,SKUs) %>% 
        mutate(sku = str_split(string = SKUs, pattern = ",")) %>% 
        unnest(sku) %>% 
        select(-SKUs) %>% 
        left_join(clx %>% select(-sku), by = c("ID_ARTICOLO" = "uid")) %>% 
        distinct() 



# GROUP BY PRODUCT --------------------------------------------------------
clx <- clx %>% 
        separate(col = sku, into = c("col1","col2","col3"), remove = F, sep = "-", extra = "drop") %>% 
        select(-col1,-col2) %>% 
        mutate(product_id = str_replace(sku,paste0(col3),"") %>% str_replace(.,"--","-") %>% str_replace(.,"-$","")) %>% 
        mutate(product_id = gsub("-","_",product_id))


clx <- clx %>% 
        select(product_id,Shooting) %>% 
        mutate(shooting = case_when(grepl("iniziare",Shooting, ignore.case = T) ~ "No Foto", TRUE ~ "Foto Presenti")) %>% 
        group_by(product_id) %>% 
        arrange(product_id,shooting) %>% 
        summarise(shooting = first(shooting)) 

############## DESCRIPTION MANIPULATE ####################

descrizioni <- descrizioni %>% 
        mutate_at(vars(-`Product ID`,-Brand), ~ case_when(!is.na(.) ~ "Available", TRUE ~ .))


############## MERGE FILES 

out <- pim %>% 
        left_join(clx, by = "product_id") %>% 
        left_join(descrizioni, by = c("product_id" = "Product ID"))



# WRITE FILES -------------------------------------------------------------
output_file <- "k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/output/pim_report.csv"



out %>% 
        mutate(script_execution_time = Sys.time()) %>% 
        write.csv(na = "", row.names = F, file = output_file, fileEncoding = "UTF-8")

