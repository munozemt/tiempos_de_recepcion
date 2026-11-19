suppressPackageStartupMessages({ library(data.table); library(quantreg) })
f_base <- file.path("resultados", "base_analisis.rds")
if (!file.exists(f_base)) stop("Corre primero 01_preparacion.R")
B <- readRDS(f_base)
TH <- B$TH; COB <- B$COB; DIA <- B$DIA; S5 <- B$S5; S10 <- B$S10; HOSP <- B$HOSP; SED <- B$SED
N_BOOT <- 200
UMBRAL <- 30
FIN <- as.IDate("2026-09-30")
guardar <- function(x, f) fwrite(x, file.path("resultados", f), bom = TRUE)
pct <- function(x) round(100 * x, 1)
q <- function(x, p) as.numeric(quantile(x, p))

resumen <- function(d, por) {
  r <- d[, .(traslados = .N, mediana = as.numeric(median(R)), p90 = q(R, .9), pct_mas_15 = pct(mean(R > 15)), pct_mas_30 = pct(mean(R > 30)),
             pct_mas_60 = pct(mean(R > 60)), horas_totales = round(sum(R) / 60), horas_sobre_30 = round(sum(pmax(R - 30, 0)) / 60),
             dias = uniqueN(fecha_L)), by = por]
  r[, turnos_10h_por_dia_sobre_30 := round(horas_sobre_30 / (dias * 10), 2)]
  r
}
por_anio <- function(d) rbind(resumen(d, "anio")[order(anio)][, anio := as.character(anio)], resumen(d[, .(R, fecha_L, anio = "total")], "anio"))
lm_cl <- function(f, d, termino) {
  mod <- lm(f, data = d); X <- model.matrix(mod); u <- residuals(mod); cl <- as.integer(factor(d$fecha_L))
  XtX_inv <- solve(crossprod(X)); G <- rowsum(X * u, cl); nG <- nrow(G); n <- nrow(X); k <- ncol(X)
  V <- (nG / (nG - 1)) * ((n - 1) / (n - k)) * XtX_inv %*% crossprod(G) %*% XtX_inv
  c(coef = unname(coef(mod)[termino]), ee = sqrt(V[termino, termino]))
}
rq_c <- function(f, d, termino) suppressWarnings(coef(rq(f, tau = 0.5, data = d, method = "fn"))[[termino]])
remuestreos <- function(d, semilla = NULL) {
  if (!is.null(semilla)) set.seed(semilla)
  idx <- split(seq_len(nrow(d)), match(d$fecha_L, sort(unique(d$fecha_L))))
  lapply(seq_len(N_BOOT), function(b) unlist(idx[sample.int(length(idx), replace = TRUE)], use.names = FALSE))
}
ic_rq <- function(est, bb) list(est = round(est, 2), inf = round(q(bb, .025), 2), sup = round(q(bb, .975), 2))
ic_lm <- function(r) list(est = round(100 * r[["coef"]], 2), inf = round(100 * (r[["coef"]] - 1.96 * r[["ee"]]), 2), sup = round(100 * (r[["coef"]] + 1.96 * r[["ee"]]), 2))
descomp <- function(d) {
  a <- d[anio %in% c(2022L, 2026L), .(n = .N, p = mean(mas30)), by = .(hosp, anio)]
  w <- dcast(a, hosp ~ anio, value.var = c("n", "p"), fill = NA)
  for (cc in c("n_2022", "n_2026")) set(w, which(is.na(w[[cc]])), cc, 0)
  w[, `:=`(s22 = n_2022 / sum(n_2022), s26 = n_2026 / sum(n_2026))]
  ambos <- w$n_2022 > 0 & w$n_2026 > 0
  mezcla <- sum(((w$s26 - w$s22) * (w$p_2022 + w$p_2026) / 2)[ambos]) + sum((w$s26 * w$p_2026)[!ambos & w$n_2026 > 0]) - sum((w$s22 * w$p_2022)[!ambos & w$n_2022 > 0])
  dentro <- sum(((w$p_2026 - w$p_2022) * (w$s22 + w$s26) / 2)[ambos])
  total <- sum(w$s26 * w$p_2026, na.rm = TRUE) - sum(w$s22 * w$p_2022, na.rm = TRUE)
  c(total = total, mezcla = mezcla, dentro = dentro)
}
tabla_descomp <- function(d_obs, dd) data.table(componente = names(d_obs), puntos = round(100 * d_obs, 2),
                                                inf = round(100 * apply(dd, 1, quantile, .025), 2), sup = round(100 * apply(dd, 1, quantile, .975), 2),
                                                porcentaje_del_cambio = round(100 * d_obs / d_obs[["total"]], 1))

TT <- TH[, .(R, R_tr, mas30 = as.numeric(R > UMBRAL), fecha_L, hosp, sector, turno, hora_L, anio, grupo_motivo, mes = month(fecha_L),
             t = as.numeric(fecha_L - as.IDate("2022-01-01")) / 365.25, mes_anio = factor(month(fecha_L)))]

T1 <- rbind(por_anio(TH)[, `:=`(grupo = "Anio", nivel = anio)][, anio := NULL],
            resumen(TH, "sector")[sector != "Otro"][, `:=`(grupo = "Sector", nivel = sector, turnos_10h_por_dia_sobre_30 = NA_real_)][, sector := NULL])
setcolorder(T1, c("grupo", "nivel"))
guardar(T1, "tabla_I_tiempo_en_hospital_por_anio_y_sector.csv")

TT[, hosp_f := factor(fifelse(hosp %in% TT[, .N, by = hosp][N >= 50, hosp], hosp, "OTROS"))]
boots <- remuestreos(TT, 20261031)
T2 <- list()
for (aj in c(FALSE, TRUE)) {
  f <- if (aj) R ~ t + mes_anio + hosp_f else R ~ t + mes_anio
  bb <- vapply(boots, function(ii) { d <- TT[ii]; d[, hosp_f := droplevels(hosp_f)]; rq_c(f, d, "t") }, 0)
  T2[[length(T2) + 1]] <- c(list(medida = "Mediana, minutos por anio", ajuste_destino = aj), ic_rq(rq_c(f, TT, "t"), bb))
}
for (aj in c(FALSE, TRUE)) {
  f <- if (aj) mas30 ~ t + mes_anio + hosp_f else mas30 ~ t + mes_anio
  T2[[length(T2) + 1]] <- c(list(medida = "Mas de 30 minutos, puntos por anio", ajuste_destino = aj), ic_lm(lm_cl(f, TT, "t")))
}
T2 <- rbindlist(T2)
dd <- vapply(boots, function(ii) descomp(TT[ii]), c(total = 0, mezcla = 0, dentro = 0))
T2d <- tabla_descomp(descomp(TT), dd)
guardar(T2, "tabla_II_cambio_anual.csv")
guardar(T2d, "tabla_II_descomposicion_2022_2026.csv")
rm(boots, dd)

TT[, motivo_f := relevel(factor(grupo_motivo), "Clinico")]
GM <- c("Clinico", "Trauma", "Obstetrico")
guardar(rbind(TT[, .(traslados = .N, pct = pct(.N / nrow(TT)), mediana = as.numeric(median(R)), p90 = q(R, .9), pct_mas_30 = pct(mean(mas30)),
                     pct_2022 = pct(sum(anio == 2022) / TT[anio == 2022, .N]), pct_2026 = pct(sum(anio == 2026) / TT[anio == 2026, .N])), by = grupo_motivo][match(GM, grupo_motivo)]),
        "suplemento_F1_motivo_por_grupo.csv")
guardar(dcast(TT[sector != "Otro", .N, by = .(sector, grupo_motivo)][, pct := pct(N / sum(N)), by = sector], sector ~ grupo_motivo, value.var = "pct"), "suplemento_F3_motivo_por_sector.csv")
boots <- remuestreos(TT, 20261116)
FM <- list(); CM <- NULL
for (aj in c(FALSE, TRUE)) {
  f <- if (aj) R ~ t + mes_anio + hosp_f + motivo_f else R ~ t + mes_anio + hosp_f
  k <- if (aj) c("t", "motivo_fTrauma", "motivo_fObstetrico") else "t"
  est <- suppressWarnings(coef(rq(f, tau = 0.5, data = TT, method = "fn")))[k]
  bb <- matrix(vapply(boots, function(ii) { d <- TT[ii]; d[, hosp_f := droplevels(hosp_f)]; suppressWarnings(coef(rq(f, tau = 0.5, data = d, method = "fn")))[k] }, numeric(length(k))), nrow = length(k))
  FM[[length(FM) + 1]] <- c(list(muestra = "Todos los destinos", medida = "Mediana, minutos por anio", ajuste_motivo = aj), ic_rq(est[1], bb[1, ]))
  FM[[length(FM) + 1]] <- c(list(muestra = "Todos los destinos", medida = "Mas de 30 minutos, puntos por anio", ajuste_motivo = aj), ic_lm(lm_cl(update(f, mas30 ~ .), TT, "t")))
  if (aj) CM <- data.table(grupo = c("Trauma", "Obstetrico"), diferencia_mediana_vs_clinico = round(est[2:3], 1),
                           inf = round(apply(bb[2:3, , drop = FALSE], 1, q, .025), 1), sup = round(apply(bb[2:3, , drop = FALSE], 1, q, .975), 1))
}
rm(boots)
TS <- TT[sector == "IMSS-Bienestar"][, hosp_f := droplevels(hosp_f)]
boots <- remuestreos(TS, 20261117)
for (aj in c(FALSE, TRUE)) {
  f <- if (aj) R ~ t + mes_anio + hosp_f + motivo_f else R ~ t + mes_anio + hosp_f
  bb <- vapply(boots, function(ii) { d <- TS[ii]; d[, hosp_f := droplevels(hosp_f)]; rq_c(f, d, "t") }, 0)
  FM[[length(FM) + 1]] <- c(list(muestra = "Hospitales de IMSS-Bienestar", medida = "Mediana, minutos por anio", ajuste_motivo = aj), ic_rq(rq_c(f, TS, "t"), bb))
}
guardar(rbindlist(FM), "suplemento_F2_tendencia_con_ajuste_por_motivo.csv")
guardar(CM, "suplemento_F4_diferencia_por_motivo.csv")
rm(boots, TS, bb)

T3 <- merge(TT, DIA[, .(fecha_L = dia, tras_por_amb)], by = "fecha_L", all.x = TRUE)
boots <- remuestreos(T3, 20261109)
F1 <- rbindlist(lapply(c("IMSS-Bienestar", "IMSS", "ISSSTE", "Privado"), function(s) {
  ds <- T3[sector == s]
  bb <- vapply(boots, function(ii) rq_c(R ~ t + mes_anio, T3[ii[T3$sector[ii] == s]], "t"), 0)
  m <- ic_rq(rq_c(R ~ t + mes_anio, ds, "t"), bb); p <- ic_lm(lm_cl(mas30 ~ t + mes_anio, ds, "t"))
  data.table(sector = s, traslados = nrow(ds), mediana_min_por_anio = m$est, mediana_inf = m$inf, mediana_sup = m$sup,
             mas30_puntos_por_anio = p$est, mas30_inf = p$inf, mas30_sup = p$sup)
}))
F2 <- T3[sector %in% c("IMSS-Bienestar", "IMSS", "ISSSTE", "Privado"), .(traslados = .N, mediana = as.numeric(median(R)), pct_mas_30 = pct(mean(mas30))), by = .(sector, anio)][order(sector, anio)]
guardar(F2, "tabla_III_institucion_por_anio.csv")
guardar(F1, "tabla_III_cambio_anual_por_institucion.csv")
dd <- vapply(boots, function(ii) descomp(T3[ii][sector == "IMSS-Bienestar"]), c(total = 0, mezcla = 0, dentro = 0))
guardar(tabla_descomp(descomp(T3[sector == "IMSS-Bienestar"]), dd), "texto_descomposicion_imss_bienestar_2022_2026.csv")
rm(boots, dd)
T4 <- T3[!is.na(tras_por_amb)]
T4[, sector_f := factor(sector)]
boots <- remuestreos(T4, 20261110)
F4 <- rbindlist(lapply(c(FALSE, TRUE), function(tend) {
  f_rq <- if (tend) R ~ tras_por_amb + t + mes_anio + sector_f else R ~ tras_por_amb + mes_anio + sector_f
  f_lm <- if (tend) mas30 ~ tras_por_amb + t + mes_anio + sector_f else mas30 ~ tras_por_amb + mes_anio + sector_f
  bb <- vapply(boots, function(ii) rq_c(f_rq, T4[ii], "tras_por_amb"), 0)
  m <- ic_rq(rq_c(f_rq, T4, "tras_por_amb"), bb); p <- ic_lm(lm_cl(f_lm, T4, "tras_por_amb"))
  data.table(ajuste_por_tendencia = tend, traslados = nrow(T4), mediana_min = m$est, mediana_inf = m$inf, mediana_sup = m$sup,
             mas30_puntos = p$est, mas30_inf = p$inf, mas30_sup = p$sup)
}))
guardar(F4, "texto_carga_del_mismo_dia.csv")
T4[, cuartil := cut(tras_por_amb, quantile(tras_por_amb, 0:4 / 4), include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))]
guardar(T4[, .(carga_min = round(min(tras_por_amb), 2), carga_max = round(max(tras_por_amb), 2), traslados = .N, mediana = as.numeric(median(R)), pct_mas_30 = pct(mean(mas30))), by = cuartil][order(cuartil)],
        "suplemento_C4_cuartiles_de_carga.csv")
rm(boots, T3, T4)

agg <- function(X, etiqueta) X[, c(list(grupo = etiqueta, unidades = .N), lapply(.SD, sum, na.rm = TRUE)), by = anio,
                               .SDcols = c("camas_hosp", "camas_urg", "urgenciologos", "medicos", "enfermeras_contacto")]
D4 <- rbind(agg(SED[coincidencias == 1L], "IMSS-Bienestar, hospitales de la bitacora enlazados con la DGIS"),
            agg(HOSP[institucion == "IMSS"], "IMSS, Ciudad de Mexico"), agg(HOSP[institucion == "ISSSTE"], "ISSSTE, Ciudad de Mexico"))[order(grupo, anio)]
guardar(D4, "tabla_IV_recursos_dgis.csv")
DX <- merge(DIA, TH[fecha_L <= FIN, .(exceso_h = sum(exceso_h)), by = .(dia = fecha_L)], by = "dia", all.x = TRUE)
DX[is.na(exceso_h), exceso_h := 0]
DX[, por_amb := exceso_h / activas]
carga <- function(X) X[, .(dias = .N, activas_mediana = as.numeric(median(activas)), activas_p10 = q(activas, .1), activas_p90 = q(activas, .9),
                           traslados_mediana = as.numeric(median(traslados)), tras_por_amb_mediana = round(as.numeric(median(tras_por_amb)), 2),
                           exceso_h_por_dia = round(mean(exceso_h), 2), exceso_h_por_ambulancia = round(sum(exceso_h) / sum(activas), 2),
                           mediana_diaria_por_ambulancia = round(median(por_amb), 2), p25_diaria = round(q(por_amb, .25), 2), p75_diaria = round(q(por_amb, .75), 2))]
guardar(rbind(DX[, carga(.SD), by = anio][, anio := as.character(anio)], cbind(anio = "total", carga(DX))), "tabla_IV_carga_erum_y_suplemento_C3.csv")

boots <- NULL
set.seed(20261110)
n_hosp <- TT[, .N, by = hosp]
TOP <- n_hosp[grepl("^GDF ", hosp) & N >= 1000, hosp]
q01 <- q(TT$R, 0.01); q99 <- q(TT$R, 0.99)
VAR <- list(
  "V0 Muestra principal" = rep(TRUE, nrow(TT)),
  "V1 Sin el 1% inferior y el 1% superior" = TT$R >= q01 & TT$R <= q99,
  "V2 Intervalos de hasta 240 minutos" = TT$R <= 240,
  "V3 Intervalos de 5 minutos o mas" = TT$R >= 5,
  "V4 Sin la llegada posterior al 30 de septiembre de 2026" = TT$fecha_L <= FIN,
  "V5 Sin 2022" = TT$anio >= 2023,
  "V6 Solo enero a septiembre de cada anio" = TT$mes <= 9 & TT$fecha_L <= FIN,
  "V7 Hospitales de IMSS-Bienestar con 1,000 traslados o mas" = TT$hosp %in% TOP,
  "V8 Desde junio de 2022" = TT$fecha_L >= as.IDate("2022-06-01"),
  "V9 Intervalos de 10 minutos o mas" = TT$R >= 10)
tendencias <- function(d) {
  bb <- vapply(remuestreos(d), function(ii) rq_c(R ~ t + mes_anio, d[ii], "t"), 0)
  m <- ic_rq(rq_c(R ~ t + mes_anio, d, "t"), bb); p <- ic_lm(lm_cl(mas30 ~ t + mes_anio, d, "t"))
  list(tend_mediana = m$est, tend_mediana_inf = m$inf, tend_mediana_sup = m$sup, tend_mas30 = p$est, tend_mas30_inf = p$inf, tend_mas30_sup = p$sup)
}
G1 <- rbindlist(lapply(names(VAR), function(v) { d <- TT[VAR[[v]]]
  c(list(variante = v, traslados = nrow(d), mediana = as.numeric(median(d$R)), p90 = q(d$R, .9), pct_mas_30 = pct(mean(d$mas30))), tendencias(d)) }))
G1 <- rbind(G1, data.table(variante = "Intervalos truncados en la siguiente asignacion de la unidad", traslados = nrow(TT),
                           mediana = as.numeric(median(TT$R_tr)), p90 = q(TT$R_tr, .9), pct_mas_30 = pct(mean(TT$R_tr > 30)), truncados = sum(TT$R_tr < TT$R)), fill = TRUE)
guardar(G1, "tabla_V_robustez.csv")
TT[, franja := fcase(hora_L < 6, "Llegada 00 a 05 h", hora_L < 12, "Llegada 06 a 11 h", hora_L < 18, "Llegada 12 a 17 h", default = "Llegada 18 a 23 h")]
estratos <- c(lapply(sort(unique(na.omit(TT$turno))), function(x) list(grupo = "Turno", nivel = x, f = !is.na(TT$turno) & TT$turno == x)),
              lapply(sort(unique(TT$franja)), function(x) list(grupo = "Hora de llegada", nivel = x, f = TT$franja == x)))
G4 <- rbindlist(lapply(estratos, function(e) { d <- TT[e$f]; if (nrow(d) < 500) return(NULL)
  med <- d[, .(m = as.numeric(median(R))), by = anio][order(anio)]
  c(list(grupo = e$grupo, nivel = e$nivel, traslados = nrow(d)), setNames(as.list(med$m), paste0("mediana_", med$anio)), tendencias(d)) }))
guardar(G4, "suplemento_E1_turno_y_hora_de_llegada.csv")
D6 <- copy(DIA)
D6[, `:=`(previo = shift(tras_por_amb), dia_previo = shift(dia))]
D6[as.integer(dia - dia_previo) != 1L, previo := NA_real_]
T6 <- merge(TT, D6[, .(fecha_L = dia, carga_dia = tras_por_amb, carga_previa = previo)], by = "fecha_L", all.x = TRUE)
T6[, sector_f := factor(sector)]
G6 <- rbindlist(lapply(c("carga_dia", "carga_previa"), function(x) {
  d <- T6[!is.na(get(x))]; d[, expo := get(x)]
  bb <- vapply(remuestreos(d), function(ii) rq_c(R ~ expo + t + mes_anio + sector_f, d[ii], "expo"), 0)
  m <- ic_rq(rq_c(R ~ expo + t + mes_anio + sector_f, d, "expo"), bb); p <- ic_lm(lm_cl(mas30 ~ expo + t + mes_anio + sector_f, d, "expo"))
  data.table(exposicion = x, traslados = nrow(d), mediana_min = m$est, mediana_inf = m$inf, mediana_sup = m$sup, mas30_puntos = p$est, mas30_inf = p$inf, mas30_sup = p$sup)
}))
guardar(G6, "suplemento_C5_carga_mismo_dia_y_dia_previo.csv")
rm(T6, D6)

TT[, anio_f := factor(anio)]
G5 <- rbindlist(lapply(levels(TT$anio_f)[-1], function(a) {
  term <- paste0("anio_f", a); p <- ic_lm(lm_cl(mas30 ~ anio_f + mes_anio, TT, term))
  data.table(anio = a, mediana_dif_vs_2022 = round(rq_c(R ~ anio_f + mes_anio, TT, term), 2), mas30_dif_vs_2022 = p$est, mas30_inf = p$inf, mas30_sup = p$sup)
}))
guardar(G5, "suplemento_E2_efecto_por_anio.csv")
G2 <- TT[, .(traslados = .N, p10 = q(R, .10), p25 = q(R, .25), p50 = as.numeric(median(R)), p75 = q(R, .75), p90 = q(R, .90), p99 = q(R, .99),
             pct_menos_5 = round(100 * mean(R < 5), 2), pct_mas_240 = round(100 * mean(R > 240), 2),
             pct_termina_0_o_5 = round(100 * mean((round(R) %% 5 == 0)[abs(R - round(R)) < 1e-9]), 1)), by = anio][order(anio)]
guardar(G2, "suplemento_E3_distribucion_y_digitos.csv")

guardar(COB[, `:=`(pct_con_salida = pct(con_salida / traslados), pct_estancia_valida = pct(con_estancia_valida / traslados))], "suplemento_C1_registro_disponibilidad_erum.csv")
guardar(resumen(TH, c("hosp", "sector"))[order(-horas_sobre_30), .(hosp, sector, traslados, mediana, p90, pct_mas_30, horas_sobre_30)], "suplemento_C2_por_destino.csv")
guardar(por_anio(TH[R <= 480])[, .(anio, traslados, mediana, p90, pct_mas_30, pct_mas_60, horas_sobre_30)], "suplemento_D1_limite_480_minutos.csv")
guardar(por_anio(TH[, .(R = R_tr, fecha_L, anio)])[, .(anio, traslados, mediana, p90, pct_mas_30, pct_mas_60, horas_sobre_30)], "suplemento_D2_truncados.csv")
guardar(S10[, .(traslados = .N, con_salida = sum(reg), pct_con_salida = pct(mean(reg))), by = anio][order(anio)], "suplemento_A1_samu_registro_salida.csv")
guardar(S10[, .(traslados = .N, pct_llegada_00_a_05 = pct(mean(hora < 6)), respuesta_mediana = median(t_respuesta, na.rm = TRUE),
                escena_mediana = median(t_escena, na.rm = TRUE), traslado_mediana = median(t_traslado, na.rm = TRUE), pct_segundo_hospital = pct(mean(hosp2))),
            by = .(anio, salida_registrada = reg)][order(anio, -salida_registrada)], "suplemento_A2_samu_con_y_sin_salida.csv")
a3 <- function(d, g) por_anio(d)[, .(grupo = g, anio, traslados, mediana, p90, pct_mas_30, pct_mas_60)]
guardar(rbind(a3(S5, "Todas las corporaciones"), a3(S5[tipo_dependencia %in% "SAMU-CRUM"], "Unidades del SAMU-CRUM"),
              a3(S5[tipo_dependencia %in% "SAMU-CRUM" & hosp2 == FALSE], "SAMU-CRUM sin segundo hospital")), "suplemento_A3_samu_tiempo_en_hospital.csv")
guardar(S10[, .(traslados = .N, pct_destino_registrado = pct(mean(destino_reg)), con_salida_y_disponible = sum(ambas),
                pct_salida_igual_disponible = pct(sum(igual) / sum(ambas))), by = anio][order(anio)], "suplemento_A4_samu_destino_y_disponible.csv")
S24 <- S10[anio >= 2024]
guardar(rbind(S24[, .(grupo = "Hora de llegada", nivel = fcase(hora < 6, "00 a 05", hora < 12, "06 a 11", hora < 18, "12 a 17", default = "18 a 23"), reg)][!is.na(nivel), .(traslados = .N, pct_con_salida = pct(mean(reg))), by = .(grupo, nivel)][order(nivel)],
              S24[!is.na(turno), .(traslados = .N, pct_con_salida = pct(mean(reg))), by = .(nivel = turno)][order(-traslados)][, .(grupo = "Turno", nivel, traslados, pct_con_salida)]),
        "suplemento_A5_samu_registro_por_hora_y_turno_2024_2026.csv")
message("Analisis terminado. Tablas en la carpeta resultados")
