#' Códigos ERASMUS de las universidades europeas.
#'
#' Códigos ERASMUS de las universidades europeas, contiene el código de la
#' universidad (COD_UNIV_DESTINO), el nombre del páis ("PAIS"), la ciudad
#' ("CIUDAD"), el nombre de la institución ("NOMBRE"), las siglas del páis
#' ("SIGLAS_PAIS") así como la referencia de la solicitud de adhesión
#' ("APP_REFER_NUMBER").
#' Última modificación según portal SIIU: 02/02/2015.
#'
#' @docType data
#'
#' @usage data(siiu_codigos_erasmus)
#'
#' @keywords datasets
#'
#' @source Apartado "Descargas" en SIIU.
#'
#' @examples
#' dplyr::filter(siiu_codigos_erasmus, CIUDAD == "Cartagena")
"siiu_codigos_erasmus"

#' Códigos SIIU de las comunidades autónomas
#'
#' Códigos de las comunidades autónomas según SIIU.
#' Última modificación según portal SIIU: 05/10/2010.
#'
#' @docType data
#'
#' @usage data(siiu_codigos_ccaa)
#'
#' @keywords datasets
#'
#' @source Apartado "Descargas" en SIIU.
#' @format Un dataframe con 18 filas y 3 columnas
#' \itemize{
#'  \item CA_ID_CCAA. El código usado en los ficheros SIIU
#'  \item CA_DENOM_CCAA. La denominación de la comunidad autónoma.
#'  \item CA_SIGLAS_CCAA La sigla de la comunidad autónoma (2 letras)'
#' }
"siiu_codigos_ccaa"

#' Códigos SIIU de áreas de los paises
#'
#' Códigos de los paises según SIIU. Corresponden con la codificación ISO
#' 31666-1 numérico. 
#' Última modificación según portal SIIU: 14/03/2014
#'
#' @docType data
#'
#' @usage data(siiu_codigos_paises)
#'
#' @keywords datasets
#'
#' @source Apartado "Descargas" en SIIU.
#' @format Un dataframe con 220 filas y 2 columnas
#' \itemize{
#'  \item ID El código del páis.
#'  \item NOMBRE'
#' }
"siiu_codigos_paises"

#' Códigos SIIU de las áreas de conocimiento.
#'
#' Códigos de las áreas de conocimiento según SIIU.
#' Última modificación según portal SIIU: 19/05/2014.
#'
#' @docType data
#'
#' @usage data(siiu_codigos_areas_conocimiento)
#'
#' @keywords datasets
#'
#' @source Apartado "Descargas" en SIIU.
#' @format Un dataframe con 202 filas y 2 columnas
#' \itemize{
#'  \item ID. El código usado en los ficheros SIIU
#'  \item NOMBRE'
#' }
"siiu_codigos_areas_conocimiento"

#' Códigos SIIU de los ámbitos de estudio.
#'
#' Códigos ISCED de las ámbitos de estudio.
#' Última modificación según portal SIIU: 29/09/2014.
#'
#' @docType data
#'
#' @usage data(siiu_codigos_ambito_estudio)
#'
#' @keywords datasets
#'
#' @source Apartado "Descargas" en SIIU.
#' @format Un dataframe con 83 filas y 2 columnas
#' \itemize{
#'  \item ID. El código ISCED de tres dígitos.
#'  \item NOMBRE'
#' }
#' @examples
#' ## Para obtener los códigos de 2 dígitos, corresponden a los múltiplos de 10
#' dplyr::filter(siiu_codigos_ambito_estudio, substr(ID, 3, 3) == "0") %>%
#' dplyr::mutate(ID2 = substr(ID, 1, 2))
"siiu_codigos_ambito_estudio"
