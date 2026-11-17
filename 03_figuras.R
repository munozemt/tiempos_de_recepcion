suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
f_base <- file.path("resultados", "base_analisis.rds")
if (!file.exists(f_base)) stop("Corre primero 01_preparacion.R")
B <- readRDS(f_base)
dir.create("figuras", showWarnings = FALSE)
FIN <- as.IDate("2026-09-30")
fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
guardar <- function(g, nombre, alto) {
  ggsave(file.path("figuras", paste0(nombre, ".tiff")), g, width = 17.4, height = alto, units = "cm", dpi = 600, compression = "lzw", bg = "white")
  ggsave(file.path("figuras", paste0(nombre, ".png")), g, width = 17.4, height = alto, units = "cm", dpi = 600, bg = "white")
}

F <- B$FLUJO[order(orden)]
principal <- c("Traslados de ambulancias del ERUM con llegada\nal hospital y destino registrados", "Destino reconocido", "Hora de disponibilidad válida",
               "Intervalo de 0 a 600 minutos", "Llegada al hospital entre 0 y 600 minutos\ndespués de la llamada", "Alcaldía de origen registrada",
               "Sin nueva asignación antes de llegar", "Traslados incluidos en el análisis")
exclusion <- c(NA, "Destino registrado como R o vacío", "Sin disponibilidad, o disponibilidad\nanterior a la llegada", "Intervalo mayor de 600 minutos",
               "Sin hora de llamada, o llegada fuera\ndel rango de 0 a 600 minutos", "Sin alcaldía de origen", "Unidad asignada a un nuevo servicio\nantes de llegar al hospital",
               "Motivo administrativo, operativo,\nde búsqueda o sin registro")
pasos <- F[orden == 0 | excluidas > 0]
k <- nrow(pasos)
C <- data.table(i = seq_len(k), orden = pasos$orden)
C[, y := (k - i) * 1.25]
C[, txt := paste0(principal[orden + 1], "\nn = ", fmt(pasos$quedan))]
X <- C[orden > 0]
X[, txt := paste0(exclusion[orden + 1], "\nn = ", fmt(pasos[orden > 0]$excluidas))]
X[, y := y + 0.625]
xm <- 0; wm <- 2.0; hm <- 0.40; xe <- 4.75; we <- 1.95; he <- 0.30
g1 <- ggplot() +
  geom_segment(data = C[-.N], aes(x = xm, xend = xm, y = y - hm, yend = y - 1.25 + hm), arrow = arrow(length = unit(1.6, "mm"), type = "closed"), linewidth = 0.35) +
  geom_segment(data = X, aes(x = xm, xend = xe - we, y = y, yend = y), arrow = arrow(length = unit(1.6, "mm"), type = "closed"), linewidth = 0.35) +
  geom_rect(data = C, aes(xmin = xm - wm, xmax = xm + wm, ymin = y - hm, ymax = y + hm), fill = "white", colour = "black", linewidth = 0.35) +
  geom_rect(data = X, aes(xmin = xe - we, xmax = xe + we, ymin = y - he, ymax = y + he), fill = "white", colour = "black", linewidth = 0.35) +
  geom_text(data = C, aes(x = xm, y = y, label = txt), size = 2.7, lineheight = 0.95, family = "sans") +
  geom_text(data = X, aes(x = xe, y = y, label = txt), size = 2.5, lineheight = 0.95, family = "sans") +
  coord_cartesian(xlim = c(xm - wm - 0.1, xe + we + 0.1), expand = FALSE, clip = "off") +
  scale_y_continuous(limits = c(min(C$y) - hm - 0.1, max(C$y) + hm + 0.1)) +
  theme_void() + theme(plot.background = element_rect(fill = "white", colour = NA), plot.margin = margin(4, 4, 4, 4))
guardar(g1, "Figura1_inclusion", 0.9 + 2.25 * k)

TH <- B$TH[fecha_L <= FIN]
TH[, mes := format(fecha_L, "%Y-%m")]
M <- merge(TH[, .(mediana = as.numeric(median(R))), by = mes], TH[sector == "SEDESA", .(mediana_sedesa = as.numeric(median(R))), by = mes], by = "mes")
D <- B$DIA[, .(activas_mediana = as.numeric(median(activas)), tras_por_amb = round(sum(traslados) / sum(activas), 2)), by = .(mes = format(dia, "%Y-%m"))]
S <- merge(M, D, by = "mes")[order(mes)]
fwrite(S, file.path("resultados", "figura2_serie_mensual.csv"), bom = TRUE)
S[, fecha := as.Date(paste0(mes, "-15"))]
pA <- rbind(S[, .(fecha, valor = mediana, serie = "Todos los hospitales")], S[, .(fecha, valor = mediana_sedesa, serie = "Hospitales de SEDESA")])
pA[, panel := "A. Mediana del tiempo en hospital (minutos)"]
P <- rbind(pA, S[, .(fecha, valor = tras_por_amb, serie = "ERUM", panel = "B. Traslados por ambulancia activa en el día")],
           S[, .(fecha, valor = activas_mediana, serie = "ERUM", panel = "C. Ambulancias activas por día (mediana mensual)")])
P[, `:=`(panel = factor(panel, levels = unique(P$panel)), serie = factor(serie, levels = c("Todos los hospitales", "Hospitales de SEDESA", "ERUM")))]
g2 <- ggplot(P, aes(fecha, valor, linetype = serie, colour = serie)) +
  geom_line(linewidth = 0.45) +
  facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c("Todos los hospitales" = "black", "Hospitales de SEDESA" = "grey45", "ERUM" = "black"), breaks = c("Todos los hospitales", "Hospitales de SEDESA"), name = NULL) +
  scale_linetype_manual(values = c("Todos los hospitales" = "solid", "Hospitales de SEDESA" = "22", "ERUM" = "solid"), breaks = c("Todos los hospitales", "Hospitales de SEDESA"), name = NULL) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y", expand = expansion(mult = 0.01)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8, base_family = "sans") +
  theme(strip.background = element_blank(), strip.text = element_text(hjust = 0, face = "bold", size = 8), panel.grid.minor = element_blank(),
        legend.position = "top", legend.justification = "left", legend.key.width = unit(8, "mm"), legend.margin = margin(0, 0, 0, 0), axis.text.x = element_text(size = 6.5))
guardar(g2, "Figura2_serie_mensual", 16)
message("Figuras terminadas en la carpeta figuras")
