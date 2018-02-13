library(tidyverse)
library(stringr)
library(readxl)




# READ FILES --------------------------------------------------------------
clx <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Prada") %>% 
        bind_rows(read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Prada_report.xlsx", sheet = "Miu Miu"))

anagrafica <- read_csv2("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/FLUIID4_ANAGRAFICA_TOT.CSV",quote = "", col_types = cols(.default = col_character()))
descrizioni <- read_csv2("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/ProductTextEnrichment.csv", col_types = cols(.default = col_character()))
pim <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/Published PIM - master.xlsx", sheet = "Sheet1", guess_max = 1000000)
giacenze <- read_excel("k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/raw data/estrazione magazzino.XLS",sheet = 1, guess_max = 1000000)



############## PIM MANIPULATE ####################


# GENERATE PRODUCT ID ----------------------------------------------------
pim <- pim %>% 
        separate(col = `Variant no.`, into = c("col1","col2","col3"), remove = F, sep = "_", extra = "drop") %>% 
        select(-col1,-col2) %>% 
        mutate(product_id = str_replace(`Variant no.`,paste0(col3),"") %>% str_replace(.,"__","_") %>% str_replace(.,"_$",""))
        


############## ANAGRAFICA MANIPULATE ####################


# REMOVE QUOTES -----------------------------------------------------------
anagrafica <- anagrafica %>% 
        mutate_all(~ gsub("\"$","",.)) %>% 
        mutate_all(~ gsub("^\"","",.))


############## CLX MANIPULATE ####################

# FILTER OUT INVALID PLACEHOLDERS  ----------------------------------------
clx <- clx %>% 
        separate(col = "file_name",into = c("tmp","placeholder"), extra = "drop", sep = "_", remove = F) %>% 
        select(-tmp) %>% 
        filter(nchar(placeholder) == 3)





# RESHAPE ANAGRAFICA AND REPLACE CLX SKU WITH RESHAPED SKU PRESENT IN ANAGRAFICA --------------------------------------------
clx <- anagrafica %>% 
        select(ID_ARTICOLO,SKUs,CodiceCollezione,DescrizioneReparto,DES_ABBINAMENTO,DescrizioneColore,LABEL_WAVE) %>% 
        mutate(sku = str_split(string = SKUs, pattern = ",")) %>% 
        unnest(sku) %>% 
        select(-SKUs) %>% 
        left_join(clx %>% select(-sku), by = c("ID_ARTICOLO" = "uid")) %>% 
        distinct() 



# GROUP BY PRODUCT --------------------------------------------------------
clx_agg <- clx %>% 
        separate(col = sku, into = c("col1","col2","col3"), remove = F, sep = "-", extra = "drop") %>% 
        select(-col1,-col2) %>% 
        mutate(product_id = str_replace(sku,paste0(col3),"") %>% str_replace(.,"--","-") %>% str_replace(.,"-$","")) %>% 
        mutate(product_id = gsub("-","_",product_id))


clx_agg <- clx_agg %>% 
        select(product_id,Shooting) %>% 
        mutate(shooting = case_when(grepl("iniziare",Shooting, ignore.case = T) ~ "No Foto", TRUE ~ "Foto Presenti")) %>% 
        group_by(product_id) %>% 
        arrange(product_id,shooting) %>% 
        summarise(shooting = first(shooting)) 

############## DESCRIPTION MANIPULATE ####################
descrizioni <- descrizioni %>% 
        mutate_at(vars(-`Product ID`,-Brand), ~ case_when(!is.na(.) ~ "Available", TRUE ~ .))




############## GIACENZE MANIPULATE ####################
giacenze <- giacenze %>% 
        mutate_all(str_trim) %>% 
        mutate_at(vars(MDPRB6,CPARB6,COL5B6,TPVAB6,VARIB6), ~ ifelse(is.na(.),"",.)) %>% 
        mutate(sku = paste0(MDPRB6,"_",CPARB6,"_",COL5B6,"_",TPVAB6,"_",VARIB6)) %>% 
        mutate(sku = gsub("__","_",sku) %>% gsub("_$","",.)) %>% 
        mutate(tipo_giacenza = MAGAB6) %>% 
        select(sku,tipo_giacenza) %>% 
        distinct() %>% 
        group_by(sku) %>% 
        summarise(tipo_giacenza = paste0(tipo_giacenza, collapse = ", "))
        

############## MERGE FILES FOR PIM DASHBOARD 
out <- pim %>% 
        left_join(clx_agg, by = "product_id") %>% 
        left_join(descrizioni, by = c("product_id" = "Product ID"))


############## MERGE FILES FOR MARTA REPORT 
marta_report <- clx %>% 
        mutate(sku = gsub("-","_",sku)) %>% 
        left_join(giacenze, by = "sku") 


#recap shooting
recap <- marta_report %>% 
        filter(!is.na(file_name)) %>% 
        mutate(photo_present = case_when(grepl("\\.jpg$|\\.png$|\\.psd\\.jpeg$",file_name, ignore.case = T) ~ "scattato", TRUE ~ "non_scattato")) %>% 
        group_by(sku,photo_present) %>% 
        summarise(n = n()) %>% 
        spread(photo_present,n,fill = 0) %>% 
        ungroup() %>% 
        mutate(recap = case_when(non_scattato == 0 & scattato > 0 ~ "shooting completo",
                                 non_scattato > 0 & scattato > 0 ~ "mancano alcune",
                                 non_scattato > 0 & scattato == 0 ~ "mancano tutte",
                                 TRUE ~ "invalid"))

#recap approvazione
recap_approvazione <- marta_report %>% 
        filter(!is.na(file_name)) %>% 
        mutate(approvazione = case_when(grepl("approvati",asset_status, ignore.case = T) ~ "approvati", 
                                        grepl("alternativ.|cancellat.|contribut.|(contributi lavorati)",asset_status, ignore.case = T) ~ "altri_stati", 
                                        TRUE ~ "non_approvati")) %>% 
        group_by(sku,approvazione) %>% 
        summarise(n = n()) %>% 
        spread(approvazione,n,fill = 0) %>% 
        ungroup() %>% 
        mutate(recap_approvazione = case_when(non_approvati == 0 & approvati > 0 ~ "approvazione completa",
                                 non_approvati >  0 & approvati > 0 ~ "approvazione parziale",
                                 non_approvati >  0 & approvati == 0 ~ "nessuna approvazione",
                                 TRUE ~ "other"))


marta_report <- marta_report %>% 
        group_by(sku) %>% 
        summarise_at(vars(marchio,CodiceCollezione,DescrizioneReparto,DES_ABBINAMENTO,DescrizioneColore,LABEL_WAVE),first) %>% 
        inner_join(recap, by = "sku") %>% 
        inner_join(recap_approvazione, by = "sku") %>% 
        left_join(giacenze, by = "sku")


marta_report <- marta_report %>% 
        separate(col = sku, into = c("Modelcode","Materialcode","Colorcode","Typevariantcode","Variantcode"), remove = F, sep = "_", fill = "right") %>% 
        mutate(sku_editoriale = str_replace(sku,paste0(Colorcode),"") %>% str_replace(.,"__","_") %>% str_replace(.,"_$","") %>% gsub("_","",.)) %>% 
        mutate_at(vars(Modelcode,Materialcode,Typevariantcode,Variantcode,Colorcode), ~ ifelse(is.na(.),"",.)) %>% 
        mutate(sku_catalogo = paste0(Modelcode,Typevariantcode,Variantcode,Materialcode,Colorcode)) %>% 
        mutate(recap_inclusa_giacenza = case_when(!is.na(tipo_giacenza) ~ paste0(recap," - ",tipo_giacenza),
                                                  TRUE ~ recap)) %>% 
        select(sku,sku_editoriale,sku_catalogo,marchio,Modelcode,Materialcode,Typevariantcode,Variantcode,Colorcode,recap,recap_inclusa_giacenza,recap_approvazione,LABEL_WAVE, CodiceCollezione,DescrizioneReparto,DES_ABBINAMENTO,DescrizioneColore)



nicola_report <- marta_report %>% 
        filter(marchio == "Miu Miu")

marta_report <- marta_report %>%
        select(-marchio) %>% 
        right_join(pim %>% select(`Variant no.`,Brand), by = c("sku" = "Variant no.")) %>% 
        select(sku,sku_editoriale,sku_catalogo,Brand,Modelcode,Materialcode,Typevariantcode,Variantcode,Colorcode,recap,recap_inclusa_giacenza,recap_approvazione,LABEL_WAVE, CodiceCollezione,DescrizioneReparto,DES_ABBINAMENTO,DescrizioneColore)





# WRITE FILES -------------------------------------------------------------
output_file <- "k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/output_reports/pim_report_descrizioni.csv"

out %>% 
        mutate(script_execution_time = Sys.time()) %>% 
        write.csv(na = "", row.names = F, file = output_file, fileEncoding = "UTF-8")


marta_output_file <- "k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/output_reports/pim_report_scatti.csv"

marta_report %>% 
        mutate(script_execution_time = Sys.time()) %>% 
        write.csv(na = "", row.names = F, file = marta_output_file, fileEncoding = "UTF-8")



nicola_output_file <- "k:/dept/DIGITAL E-COMMERCE/E-COMMERCE/Report E-Commerce/pim_report/output_reports/pim_report_miumiu.csv"

nicola_report %>% 
        mutate(script_execution_time = Sys.time()) %>% 
        write.csv(na = "", row.names = F, file = nicola_output_file, fileEncoding = "UTF-8")
