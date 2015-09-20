library("readxl")
library("dplyr")
datapath <- c("~/OPADA/paquetesr/opadar/data-raw/data/")
siiu_codigos_erasmus <-
    read_excel(opadar::datafile("siiu_Codigos_70609.xls"),
               sheet = "Códigos_ERASMUS")
siiu_codigos_ccaa <-
    read_excel(opadar::datafile("siiu_Codigos_70609.xls"),
               sheet = "CCAA")
names(siiu_codigos_ccaa) <- gsub(" ", "", names(siiu_codigos_ccaa))
siiu_codigos_paises <-
    read_excel(opadar::datafile("siiu_Codigos_22910.xls"),
               sheet = "País")
names(siiu_codigos_paises) <- gsub(" ", "", names(siiu_codigos_paises))
siiu_codigos_areas_conocimiento <-
    read_excel(opadar::datafile("siiu_Codigos_97112.xls"),
               sheet = "Área_de_conocimiento")
siiu_codigos_ambito_estudio <-
    read_excel(opadar::datafile("siiu_Codigos_97112.xls"),
               sheet = "Ambito_Estudio")
names(siiu_codigos_ambito_estudio) <-
    gsub(" ", "", names(siiu_codigos_ambito_estudio))
