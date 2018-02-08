quadratura <- read_excel("../../../../Desktop/SCATTO_status_scatti_as_of_20180207.xlsx", col_types = "text")

quadratura <- quadratura %>% 
        mutate(sku = gsub("-","_",sku))


# quadratura <- quadratura %>% 
#         select(intersect(colnames(marta_report),colnames(quadratura))) %>% 
#         mutate_at(vars(Modelcode,Materialcode,Typevariantcode,Variantcode,Colorcode), ~ ifelse(is.na(.),"",.))
# 
# marta_report_quadratura <- marta_report %>% 
#         select(intersect(colnames(marta_report),colnames(quadratura)))
# 
# quadratura <- quadratura %>% 
#         semi_join(marta_report_quadratura, by = c("sku"))
# 
# marta_report_quadratura <- marta_report_quadratura %>% 
#         semi_join(quadratura, by = c("sku"))
# 
# 
# 
# checks <- all.equal(quadratura %>% select(sku,recap,recap_approvazione),marta_report_quadratura %>% select(sku,recap,recap_approvazione))
# 
# 
# marta_report_quadratura %>% 
#         slice(5470)


quadratura %>% 
        filter(!grepl("MM",LABEL_WAVE)) %>% 
        filter(LABEL_WAVE != "ELIMINARE" & LABEL_WAVE != "NOWAVE") %>% 
        anti_join(marta_report, by = c("sku")) %>% count(LABEL_WAVE)
