library("readxl")
library("dplyr")
library("XML")
library("opadar")
datapath <- c("~/OPADA/paquetesr/opadar/data-raw/data/",
              "~/OPADA/data/SIIU/ficheros/",
              "~/OPADA/data/SIIU/ficheros-codigos/")

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
siiu_codigos_categoria_pdi <-
    read_excel(opadar::datafile("siiu_Codigos_86047.xls"),
               sheet = 1)
siiu_codigos_categoria_pdi <-
    siiu_codigos_categoria_pdi %>%
      mutate(TipoCentro = "Centros propios")
siiu_codigos_categoria_pdi <-
    siiu_codigos_categoria_pdi %>% 
      rbind(
          read_excel(opadar::datafile("siiu_Codigos_86047.xls"),
                     sheet = 2) %>%
            mutate(TipoCentro = "Centros privados"))
siiu_codigos_categoria_pdi <-
    siiu_codigos_categoria_pdi %>% 
      rbind(
          read_excel(opadar::datafile("siiu_Codigos_86047.xls"),
                     sheet = 3) %>%
            mutate(TipoCentro = "Centros privados sin ánimo de lucro"))
## los códigos de los centros UPCT
centrosUPCT <- xmlToDataFrame(datafile("U06414AX0101_02.XML")) %>%
  rename(NombreCentro = NombreUnidad,
         CodigoSIIU = Unidad) %>%
  select(NombreCentro, CodigoSIIU) %>%
  mutate(Acronimo = c("CUD",
             "FCE",
             "ETSII",
             "ETSIA",
             "EICM",
             "ETSINO",
             "EUT",
             "ETSIT",
             "ARQEDI"),
         CodigoUXXI = c("222",
             "6401",
             "6402",
             "6403",
             "6404",
             "6405",
             "6407",
             "6408",
             "6410"))

devtools::use_data(centrosUPCT, overwrite = TRUE)

## -----------------------------------------------------------------------------
##
## dwd_estudios, planes centros
##
## -----------------------------------------------------------------------------

planes_centros <- read.table(datafile("planes_centros.txt"), skip = 6,
                                     sep = "|", fileEncoding = "latin1",
                            header = TRUE)
devtools::use_data(planes_centros, overwrite = TRUE)
