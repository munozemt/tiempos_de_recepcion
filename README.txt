DATOS Y CÓDIGO DEL ARTÍCULO
Cuello de botella hospitalario en la atención prehospitalaria de la Ciudad de México

Autor. José Manuel Muñoz-Carrillo, Escuela de Gobierno y Transformación Pública, Tecnologico de Monterrey. ORCID 0009-0009-7055-4926.
Versión del depósito. 1.1, noviembre de 2026.
Depósito. https://github.com/munozemt/tiempos_de_recepcion
Licencia. Creative Commons Atribución 4.0 Internacional (CC BY 4.0). Ver la sección 8.


1. ARCHIVOS DEL DEPÓSITO

README.txt
LICENSE
CITATION.cff
catalogo_motivos_erum.csv
01_preparacion.R
02_analisis.R
03_figuras.R
erum_ambulancias_2022_2026_anonimizado.csv.gz
samu_crum_2022_2026_anonimizado.csv.gz


2. DATOS

erum_ambulancias_2022_2026_anonimizado.csv.gz
Bitácoras de servicio del Escuadrón de Rescate y Urgencias Médicas (ERUM) de la Secretaría de Seguridad Ciudadana de la Ciudad de México, del 1 de enero de 2022 al 30 de septiembre de 2026. Se obtuvieron con las solicitudes de información 090163424002631 (2022 y 2023) y 090163426002256 (2024 a 2026). Tiene 671,113 filas. Cada fila es una unidad asignada a un servicio. La columna grupo_motivo clasifica el motivo del servicio en Trauma, Clinico, Obstetrico y Otros (ver catalogo_motivos_erum.csv).

samu_crum_2022_2026_anonimizado.csv.gz
Registros de despacho del Centro Regulador de Urgencias Médicas de la Secretaría de Salud de la Ciudad de México (SAMU-CRUM), obtenidos con las solicitudes 090163324000705 y 090163326001654. Tiene 274,465 filas. Cada fila es un servicio.

catalogo_motivos_erum.csv
Catálogo de los 177 valores del campo motivo de la bitácora del ERUM, con el grupo detallado, el grupo de cuatro categorías, la bandera de exclusión y el número de registros de cada valor. Un valor de texto libre con datos personales aparece como [TEXTO LIBRE CON DATOS PERSONALES, OMITIDO]. El archivo de datos contiene solo el grupo de cada registro y no el motivo exacto, para no publicar motivos poco frecuentes junto con la fecha, la hora y la alcaldía. Las reglas de clasificación se describen en el material suplementario F del artículo.

Los archivos son la versión limpia de las bitácoras. Las reglas de limpieza se describen en el material suplementario B del artículo.

Anonimización
Los archivos originales contienen nombres de pacientes y de personal y no se comparten. Los archivos depositados conservan solo las columnas que usa el análisis. Se eliminaron el texto libre, las coordenadas, los folios del C5 y las claves de radiooperadores y tripulaciones. La clave del servicio del ERUM (id_servicio) es una clave secuencial que no corresponde al folio original. En el SAMU-CRUM, los destinos y resultados con menos de 50 apariciones y los números económicos con menos de 30 apariciones se agruparon como OTRO. Cada celda de texto se revisó contra listas de nombres, correos, teléfonos y CURP antes del depósito.

Formato
Archivos CSV separados por comas, codificación UTF-8, comprimidos con gzip.
Las columnas ts_ son horas de reloj local de la bitácora en formato AAAA-MM-DD HH:MM:SS, sin zona horaria.
Las columnas int_ son intervalos en minutos.
Las columnas flag_ son marcas lógicas de la limpieza.
Una celda vacía es un dato faltante.


3. DATOS QUE HAY QUE DESCARGAR APARTE

Los recursos de los hospitales provienen de las bases Recursos en Salud Sectorial 2021 a 2025 de la Dirección General de Información en Salud (DGIS), que son públicas y no se incluyen en este depósito.

Página de descarga
http://www.dgis.salud.gob.mx/contenidos/basesdedatos/da_recursos_gobmx.html

Descarga los cinco archivos .zip, uno por año de 2021 a 2025, y guárdalos en una carpeta llamada dgis dentro de la carpeta de trabajo. El nombre de cada archivo debe contener el año, por ejemplo Recursos_Salud_Sectorial_2021.zip. Cada .zip debe contener un solo archivo .csv.

La DGIS puede actualizar estos archivos. El análisis del artículo usó los archivos con estas huellas SHA-256.
2021  1c86ccbc6192876491f7f3303ba0b9370f061550a306e4ae784446334e7232bf
2022  b1cfea0dba79555c1440952865c19df512c3cc3974afbf4f00e7681d2e7470e1
2023  cb04ff2e4efe93d52ea03c26dac5abdaa2fb76212c89d11db65cc831d18f8086
2024  7f29be89a81ee51627177ae745b681750a0e97630a3fb34394a2e5eba836ec46
2025  cea7529a75253d6212a6a79c5c0082048a6f104e361a9b5137345af29e3fe747
Si los archivos cambiaron, solo pueden cambiar las cifras de recursos de la Tabla IV.


4. REQUISITOS

R 4.3 o posterior. Las cifras de la versión 1.1 del artículo se obtuvieron con R 4.3.3.
Paquetes data.table, R.utils, quantreg y ggplot2.

install.packages(c("data.table", "R.utils", "quantreg", "ggplot2"))


5. CÓMO CORRERLO

La carpeta de trabajo debe quedar así.

carpeta_de_trabajo/
    01_preparacion.R
    02_analisis.R
    03_figuras.R
    erum_ambulancias_2022_2026_anonimizado.csv.gz
    samu_crum_2022_2026_anonimizado.csv.gz
    dgis/
        cinco archivos .zip de la DGIS

En R o RStudio.

setwd("ruta/a/carpeta_de_trabajo")
source("01_preparacion.R")
source("02_analisis.R")
source("03_figuras.R")

01_preparacion.R reconstruye la muestra de 59,639 traslados y se detiene si no la obtiene. Tarda menos de un minuto.
02_analisis.R estima todos los modelos. Los intervalos de confianza de la mediana usan 200 remuestreos de días con semillas fijas, así que reproducen los del artículo. Tarda de 10 a 25 minutos según el equipo.
03_figuras.R produce las dos figuras. Tarda menos de un minuto.


6. RESULTADOS

Los scripts crean las carpetas resultados y figuras.

resultados/base_analisis.rds                                  base intermedia que usan 02 y 03
resultados/tabla_I_tiempo_en_hospital_por_anio_y_sector.csv   Tabla I
resultados/tabla_II_cambio_anual.csv                          Tabla II, cambio anual
resultados/tabla_II_descomposicion_2022_2026.csv              Tabla II, descomposición
resultados/tabla_III_institucion_por_anio.csv                 Tabla III, medianas y porcentajes por año
resultados/tabla_III_cambio_anual_por_institucion.csv         Tabla III, cambio anual
resultados/tabla_IV_recursos_dgis.csv                         Tabla IV, recursos de la DGIS
resultados/tabla_IV_carga_erum_y_suplemento_C3.csv            Tabla IV, carga del ERUM, y Tabla C3 del suplemento
resultados/tabla_V_robustez.csv                               Tabla V
resultados/texto_descomposicion_imss_bienestar_2022_2026.csv  descomposición en hospitales de IMSS-Bienestar, en Resultados
resultados/texto_carga_del_mismo_dia.csv                      modelo de carga del mismo día, en Resultados
resultados/suplemento_A1_samu_registro_salida.csv             Tabla A1 del suplemento
resultados/suplemento_A2_samu_con_y_sin_salida.csv            Tabla A2 del suplemento
resultados/suplemento_A3_samu_tiempo_en_hospital.csv          Tabla A3 del suplemento
resultados/suplemento_A4_samu_destino_y_disponible.csv     Tabla A4 del suplemento
resultados/suplemento_A5_samu_registro_por_hora_y_turno_2024_2026.csv  Tabla A5 del suplemento
resultados/suplemento_C1_registro_disponibilidad_erum.csv     Tabla C1 del suplemento
resultados/suplemento_C2_por_destino.csv                      Tabla C2 del suplemento
resultados/suplemento_C4_cuartiles_de_carga.csv               Tabla C4 del suplemento
resultados/suplemento_C5_carga_mismo_dia_y_dia_previo.csv     Tabla C5 del suplemento
resultados/suplemento_D1_limite_480_minutos.csv               Tabla D1 del suplemento
resultados/suplemento_D2_truncados.csv                        Tabla D2 del suplemento
resultados/suplemento_E1_turno_y_hora_de_llegada.csv          Tabla E1 del suplemento
resultados/suplemento_E2_efecto_por_anio.csv                  Tabla E2 del suplemento
resultados/suplemento_E3_distribucion_y_digitos.csv           Tabla E3 del suplemento
resultados/suplemento_F1_motivo_por_grupo.csv              Tabla F1 del suplemento
resultados/suplemento_F2_tendencia_con_ajuste_por_motivo.csv  Tabla F2 del suplemento
resultados/suplemento_F3_motivo_por_sector.csv             Tabla F3 del suplemento
resultados/suplemento_F4_diferencia_por_motivo.csv         Tabla F1 del suplemento, diferencia ajustada
resultados/figura2_serie_mensual.csv                          datos de la Figura 2
figuras/Figura1_inclusion.tiff y .png                         Figura 1
figuras/Figura2_serie_mensual.tiff y .png                     Figura 2

La Tabla B1 del suplemento (reglas de limpieza) no se reproduce, porque se calculó sobre los archivos originales.


7. DEFINICIONES

Tiempo en hospital. Minutos desde la llegada al hospital (ts_llegada) hasta que la unidad queda disponible (ts_salida), en la columna int_llegada_salida.
Horas de exceso. Suma de los minutos de cada intervalo por encima de 30, divididos entre 60.
Ambulancia activa. Unidad de tipo AMBULANCIA ERUM con al menos un registro en el día.
Sector. IMSS-Bienestar si el destino empieza con GDF, y IMSS, ISSSTE, Privado u Otro público según la institución registrada. La bitácora registra con el prefijo GDF a los hospitales de la Secretaría de Salud de la Ciudad de México, que pasaron a IMSS-Bienestar en 2024.
Grupo de motivo. Trauma, Clinico u Obstetrico según catalogo_motivos_erum.csv. Los traslados del grupo Otros (motivo administrativo, de búsqueda o sin registro interpretable) se excluyen de la muestra.


8. LICENCIA Y CITA

Los datos, el catálogo y el código de este depósito se distribuyen con la licencia Creative Commons Atribución 4.0 Internacional (CC BY 4.0), cuyo texto completo está en el archivo LICENSE y en https://creativecommons.org/licenses/by/4.0/legalcode.es

Cualquier persona puede copiar, redistribuir, adaptar y usar estos archivos con cualquier fin, incluso comercial, siempre que cite al autor, indique la licencia y señale si hizo cambios.

Cita sugerida.
Muñoz-Carrillo JM. Datos y código del artículo Cuello de botella hospitalario en la atención prehospitalaria de la Ciudad de México. Versión 1.0. 2026. Disponible en https://github.com/munozemt/tiempos_de_recepcion

Cuando el artículo se publique, se agregará aquí su referencia, que deberá citarse junto con el depósito.


9. CAMBIOS RESPECTO DE LA VERSIÓN 1.0

El intento de suicidio por medicamentos pasó del grupo Clinico al grupo Trauma, con lo que todos los intentos de suicidio quedan en Trauma. Cambia la columna grupo_motivo de 4,495 registros y la fila correspondiente de catalogo_motivos_erum.csv.
Se eliminó el límite superior de 600 minutos al tiempo en hospital, en el ERUM y en el SAMU-CRUM, porque una ambulancia puede quedar retenida durante horas. La muestra pasó de 59,628 a 59,639 traslados.
El sector de los hospitales con prefijo GDF se llama ahora IMSS-Bienestar.
