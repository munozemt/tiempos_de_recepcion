faltan <- setdiff(c("data.table", "R.utils", "quantreg", "ggplot2"), rownames(installed.packages()))
if (length(faltan)) stop("Instala antes los paquetes  ", paste(faltan, collapse = ", "))
suppressPackageStartupMessages({ library(data.table); library(R.utils) })
Sys.setenv(TZ = "America/Mexico_City")
dir.create("resultados", showWarnings = FALSE)

f_erum <- "erum_ambulancias_2022_2026_anonimizado.csv.gz"
f_samu <- "samu_crum_2022_2026_anonimizado.csv.gz"
zips <- sort(list.files("dgis", pattern = "[.]zip$", full.names = TRUE))
if (!file.exists(f_erum) || !file.exists(f_samu)) stop("Faltan los dos archivos .csv.gz en la carpeta de trabajo")
if (length(zips) != 5) stop("Faltan los cinco archivos de la DGIS en la carpeta dgis")

UMBRAL <- 30
TOPE <- 600
FIN <- as.IDate("2026-09-30")
sin_acento <- function(x) toupper(chartr("\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", "AEIOUUNaeiouun", x))
nrm <- function(x) trimws(gsub("[[:space:]]+", " ", sin_acento(enc2utf8(as.character(x)))))
hora_txt <- function(x, f) format(x, f, tz = "UTC")
a_hora <- function(x) as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
leer <- function(f) {
  cab <- names(fread(f, nrows = 0))
  ch <- intersect(cab, c("id_registro", "id_servicio", "fecha", "turno", "unidad", grep("^ts_", cab, value = TRUE)))
  D <- fread(f, colClasses = list(character = ch), na.strings = c("", "NA"), encoding = "UTF-8", showProgress = FALSE)
  for (v in grep("^ts_", names(D), value = TRUE)) set(D, j = v, value = a_hora(D[[v]]))
  D
}

E <- leer(f_erum)
E[, `:=`(fecha_d = as.IDate(substr(fecha, 1, 10)), alc = sin_acento(delegacion))]
norm_h <- function(h) { h <- trimws(h); fcase(h == "BALBUENA", "GDF BALBUENA", h == "GENERAL LA VILLA", "GDF VILLA",
                                              h == "47 IMSS", "IMSS", h %in% c("R", ""), NA_character_, rep(TRUE, length(h)), h) }
sector_de <- function(h) fcase(grepl("^GDF ", h), "SEDESA", h == "IMSS", "IMSS", h == "ISSSTE", "ISSSTE", h == "PRIVADO", "Privado",
                               h %in% c("GOB FEDERAL", "MILITAR", "PEMEX", "CAD PASTORA"), "Otro publico", default = "Otro")

TH <- E[tipo_unidad == "AMBULANCIA ERUM" & !is.na(ts_llegada) & !is.na(hospital),
        .(id_servicio, unidad, alc, hosp = norm_h(hospital), R = int_llegada_salida, L = as.numeric(ts_llegada), sol = as.numeric(ts_solicitud), grupo_motivo)]
TH <- TH[order(is.na(R))][!duplicated(paste(unidad, id_servicio))]
FLUJO <- list(data.table(orden = 0L, quedan = nrow(TH), excluidas = NA_integer_, mediana_excluidas = NA_real_))
paso <- function(keep) {
  keep[is.na(keep)] <- FALSE
  ex <- TH[!keep]
  FLUJO[[length(FLUJO) + 1]] <<- data.table(orden = length(FLUJO), quedan = sum(keep), excluidas = nrow(ex),
                                            mediana_excluidas = if (any(!is.na(ex$R))) as.numeric(median(ex$R, na.rm = TRUE)) else NA_real_)
  TH[keep]
}
TH <- paso(!is.na(TH$hosp))
TH <- paso(!is.na(TH$R))
TH <- paso(TH$R >= 0 & TH$R <= TOPE)
TH[, d := (L - sol) / 60]
TH[!is.na(d) & d < 0 & d + 1440 <= 600, L := L + 86400]
TH[!is.na(d) & d > 600 & d - 1440 >= 0, L := L - 86400]
TH[, d := (L - sol) / 60]
TH <- paso(!is.na(TH$d) & TH$d >= 0 & TH$d <= 600)
TH <- paso(!is.na(TH$alc))
SV <- E[tipo_unidad == "AMBULANCIA ERUM", .(unidad, id_servicio, ini = as.numeric(fcoalesce(ts_asignacion, ts_solicitud)))]
SV <- SV[!is.na(ini) & !is.na(unidad)][!duplicated(paste(unidad, id_servicio))]
SV[, sig := { s <- sort(unique(ini)); s[findInterval(ini, s) + 1L] }, by = unidad]
TH <- merge(TH, SV[, .(unidad, id_servicio, sig)], by = c("unidad", "id_servicio"), all.x = TRUE)
TH <- paso(!(!is.na(TH$sig) & TH$sig < TH$L - 60))
TH <- paso(TH$grupo_motivo %in% c("Trauma", "Clinico", "Obstetrico"))
FLUJO <- rbindlist(FLUJO)
TH[, R_tr := fifelse(!is.na(sig) & sig < L + R * 60, pmax((sig - L) / 60, 0), R)]
TH[, `:=`(exceso_h = pmax(R - UMBRAL, 0) / 60, ts_L = as.POSIXct(L, origin = "1970-01-01", tz = "UTC"))]
TH[, `:=`(fecha_L = as.IDate(hora_txt(ts_L, "%Y-%m-%d")), hora_L = as.integer(hora_txt(ts_L, "%H")), sector = sector_de(hosp))]
TH[, anio := as.integer(substr(as.character(fecha_L), 1, 4))]
TUR <- E[tipo_unidad == "AMBULANCIA ERUM" & !is.na(turno) & !is.na(unidad), .(turno = as.character(turno)[1]), by = .(unidad, id_servicio)]
TH <- merge(TH, TUR, by = c("unidad", "id_servicio"), all.x = TRUE)
if (nrow(TH) != 59628L || round(sum(TH$exceso_h)) != 33307) stop("La muestra no coincide con la del articulo (59,628 traslados y 33,307 horas de exceso)")

CV <- E[tipo_unidad == "AMBULANCIA ERUM" & !is.na(ts_llegada) & !is.na(hospital)][!duplicated(paste(unidad, id_servicio))]
CV[, anio := as.integer(format(ts_llegada, "%Y", tz = "UTC"))]
COB <- CV[, .(traslados = .N, con_salida = sum(!is.na(ts_salida)), con_estancia_valida = sum(!is.na(int_llegada_salida))), by = anio][order(anio)]
COB <- rbind(COB[, anio := as.character(anio)], COB[, .(anio = "total", traslados = sum(traslados), con_salida = sum(con_salida), con_estancia_valida = sum(con_estancia_valida))])

A <- E[tipo_unidad == "AMBULANCIA ERUM" & !is.na(unidad) & !is.na(fecha_d) & fecha_d <= FIN]
A[, traslado := !is.na(ts_llegada) & !is.na(hospital)]
DIA <- A[, .(activas = uniqueN(unidad), traslados = uniqueN(paste(unidad, id_servicio)[traslado])), by = .(dia = fecha_d)][order(dia)]
DIA[, `:=`(anio = year(dia), tras_por_amb = traslados / activas)]
rm(E, A, CV, SV, TUR); invisible(gc(verbose = FALSE))

S <- leer(f_samu)
S5 <- S[!is.na(int_estancia_hosp1) & !is.na(ts_arribo_hosp1) & int_estancia_hosp1 >= 0 & int_estancia_hosp1 <= TOPE,
        .(tipo_dependencia, hosp2 = !is.na(ts_arribo_hosp2), R = int_estancia_hosp1, ts_L = ts_arribo_hosp1)]
S5[, fecha_L := as.IDate(hora_txt(ts_L, "%Y-%m-%d"))]
S5[, anio := as.integer(substr(as.character(fecha_L), 1, 4))]
S10 <- S[tipo_dependencia == "SAMU-CRUM" & !is.na(ts_arribo_hosp1) & !is.na(fecha)]
S10 <- S10[, .(reg = as.integer(!is.na(int_estancia_hosp1) & int_estancia_hosp1 >= 0 & int_estancia_hosp1 <= 600),
               anio = year(as.IDate(fecha)), hora = as.integer(hora_txt(ts_arribo_hosp1, "%H")), hosp2 = as.integer(!is.na(ts_arribo_hosp2)),
               turno = as.character(turno), destino_reg = as.integer(!is.na(destino) & destino != "OTRO"),
               ambas = as.integer(!is.na(ts_salida_hosp1) & !is.na(ts_disponible)), igual = as.integer(!is.na(ts_salida_hosp1) & !is.na(ts_disponible) & ts_salida_hosp1 == ts_disponible),
               t_respuesta = fifelse(!is.na(int_despacho_llegada) & int_despacho_llegada > 0 & int_despacho_llegada < 240, int_despacho_llegada, NA_real_),
               t_escena = fifelse(!is.na(int_llegada_traslado) & int_llegada_traslado >= 0 & int_llegada_traslado < 240, int_llegada_traslado, NA_real_),
               t_traslado = fifelse(!is.na(int_traslado_hosp1) & int_traslado_hosp1 >= 0 & int_traslado_hosp1 < 240, int_traslado_hosp1, NA_real_))]
rm(S); invisible(gc(verbose = FALSE))

VARS <- c(camas_hosp = "TOTAL CAMAS AREA HOSPITALIZACION", camas_urg = "CAMAS EN AREA DE URGENCIAS",
          urgenciologos = "MEDICOS URGENCIOLOGOS", medicos = "TOTAL MEDICOS GENERALES ESPECIALISTAS Y ODONOLOGOS",
          enfermeras_contacto = "TOTAL ENFERMERAS EN CONTACTO CON EL PACIENTE")
col_busca <- function(nombres, patron) { k <- which(nrm(nombres) == patron); if (length(k) != 1) stop("Columna no encontrada en la DGIS  ", patron); k }
inst_de <- function(x) { x <- nrm(x)
  fcase(grepl("BIENESTAR", x), "IMSS-BIENESTAR", x %in% c("IMSS", "INSTITUTO MEXICANO DEL SEGURO SOCIAL"), "IMSS",
        grepl("ISSSTE|TRABAJADORES DEL ESTADO", x), "ISSSTE", x %in% c("SSA", "SECRETARIA DE SALUD"), "SSA", default = "OTRO") }
HOSP <- rbindlist(lapply(zips, function(z) {
  tmp <- file.path(tempdir(), sub("[.]zip$", "", basename(z))); dir.create(tmp, showWarnings = FALSE)
  arch <- unzip(z, exdir = tmp); csvs <- arch[grepl("[.]csv$", arch, ignore.case = TRUE)]
  if (length(csvs) != 1) stop("El archivo ", z, " no tiene exactamente un CSV")
  D <- fread(csvs[1], encoding = "Latin-1", colClasses = "character", showProgress = FALSE)
  nn <- names(D)
  X <- data.table(anio = as.integer(regmatches(basename(z), regexpr("20[0-9]{2}", basename(z)))),
                  institucion = inst_de(D[[col_busca(nn, "INSTITUCION")]]), edo = sub("^0+", "", trimws(D[[col_busca(nn, "CLAVE ESTADO")]])),
                  tipo = nrm(D[[col_busca(nn, "TIPO DE ESTABLECIMIENTO")]]), nombre = nrm(D[[col_busca(nn, "NOMBRE DE LA UNIDAD")]]))
  for (v in names(VARS)) set(X, j = v, value = suppressWarnings(as.numeric(D[[col_busca(nn, VARS[[v]])]])))
  X[edo == "9" & grepl("HOSPITALIZ", tipo)]
}))
MAPA <- data.table(hosp = c("GDF XOCO", "GDF BALBUENA", "GDF VILLA", "GDF RUBEN LENERO", "GDF AJUSCO MEDIO", "GDF ENRIQUE CABRERA", "GDF GREGORIO SALAS",
                            "GDF TOPILEJO", "GDF TICOMAN", "GDF IZTAPALAPA", "GDF TLAHUAC", "GDF BELISARIO DOMINGUEZ", "GDF MILPA ALTA", "GDF LEGARIA",
                            "GDF TOXICOLOGICO VENUSTIANO CARRANZA", "GDF CUAJIMALPA", "GDF COYOACAN", "GDF INGUARAN", "GDF PEDIATRICO VILLA", "GDF MAGDALENA CONTRERAS",
                            "GDF XOCHIMILCO", "GDF ARAGON", "GDF MOCTEZUMA", "GDF TACUBAYA", "GDF CUAUTEPEC", "GDF PERALVILLO", "GDF IZTACALCO", "GDF CEDILLO",
                            "GDF AZCAPOTZALCO", "GDF TOXICOLOGICO XOCHIMILCO", "GDF PEDIATRICO IZTAPALAPA", "GDF EMILIANO ZAPATA"),
                   clave = c("GENERAL XOCO", "GENERAL BALBUENA", "HOSPITAL GENERAL VILLA", "RUBEN LENERO", "HOSPITAL GENERAL AJUSCO MEDIO", "ENRIQUE CABRERA", "GREGORIO SALAS",
                             "GENERAL TOPILEJO", "TICOMAN", "HOSPITAL GENERAL IZTAPALAPA", "HOSPITAL GENERAL TLAHUAC", "BELISARIO DOMING", "GENERAL MILPA ALTA", "PEDIATRICO LEGARIA",
                             "TOXICOLOGICAS VENUSTIANO", "GENERAL DE CUAJIMALPA", "PEDIATRICO COYOACAN", "MATERNO INFANTIL INGUARAN", "PEDIATRICO VILLA", "MATERNO INFANTIL MAGDALENA CONTRERAS",
                             "MATERNO PEDIATRICO XOCHIMILCO", "SAN JUAN DE ARAGON", "PEDIATRICO MOCTEZUMA", "PEDIATRICO TACUBAYA", "MATERNO INFANTIL CUAUTEPEC", "PEDIATRICO PERALVILLO", "PEDIATRICO IZTACALCO", "NICOLAS M. CEDILLO",
                             "PEDIATRICO AZCAPOTZALCO", "TOXICOLOGICAS XOCHIMILCO", "PEDIATRICO IZTAPALAPA", "EMILIANO ZAPATA"))
SED <- rbindlist(lapply(seq_len(nrow(MAPA)), function(i) {
  x <- HOSP[institucion %in% c("SSA", "IMSS-BIENESTAR") & grepl(MAPA$clave[i], nombre, fixed = TRUE)]
  if (nrow(x)) x[, `:=`(hosp = MAPA$hosp[i], coincidencias = .N), by = anio] else NULL
}))

saveRDS(list(TH = TH, FLUJO = FLUJO, COB = COB, DIA = DIA, S5 = S5, S10 = S10, HOSP = HOSP, SED = SED), file.path("resultados", "base_analisis.rds"))
message("Preparacion terminada  ", nrow(TH), " traslados  ", round(sum(TH$exceso_h)), " horas de exceso")
