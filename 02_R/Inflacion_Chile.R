# Proyecto R - Inflación interanual en Chile
# Sofía Rojas
# Ejecutar desde la carpeta que contiene el Excel.


# Colab (R) ya trae tidyverse; solo instala lo que falte
paquetes <- c("readxl", "dplyr", "ggplot2", "scales", "tidyr")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(scales); library(tidyr)
})

archivo <- "EI_PREC_VAR (Chile).xlsx"
if (!file.exists(archivo)) stop("Ubica el Excel en el directorio de trabajo.")
cat("Leyendo:", archivo, "\n")

raw <- read_excel(archivo, sheet = "Cuadro", skip = 2)

df <- raw |>
  select(fecha = 1, tasa = 2) |>
  mutate(fecha = as.Date(fecha), tasa = as.numeric(tasa)) |>
  filter(!is.na(fecha), !is.na(tasa)) |>
  arrange(fecha) |>
  mutate(
    anio      = as.integer(format(fecha, "%Y")),
    mes       = as.integer(format(fecha, "%m")),
    media_12m = as.numeric(stats::filter(tasa, rep(1/12, 12), sides = 1)),  # promedio móvil 12 meses
    var_12m   = tasa - lag(tasa, 12)                                         # variación en pp vs. año anterior
  )

cat("Observaciones:", nrow(df), "\n")
summary(df$tasa)
tail(df)

# Crea un vector con las abreviaturas de los 12 meses en español
meses_es  <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
# Crea una función que convierte una fecha en una etiqueta tipo "Ago 2026"
etq_fecha <- function(x) paste(meses_es[as.integer(format(x, "%m"))], format(x, "%Y"))
# Crea una función que formatea números
num_es    <- function(x) number(x, accuracy = 0.1, decimal.mark = ",")

col_principal <- "#1F4E79"   # azul
col_acento    <- "#E07B39"   # naranjo

# estilo visual común
tema <- theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 20, colour = "#1A1A1A"),
    plot.subtitle      = element_text(colour = "#555555", margin = margin(b = 12)),
    plot.caption       = element_text(colour = "#888888", size = 9, hjust = 0),
    plot.title.position = "plot", plot.caption.position = "plot",
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#E5E5E5"),
    legend.position    = "top", legend.justification = "left",
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.margin        = margin(15, 20, 10, 15)
  )

fuente <- "Fuente: Banco Central de Chile, Base de Datos Estadísticos. Serie EI_PREC_VAR (Chile): variación del IPC en 12 meses (%), frecuencia mensual."

# define el tamaño y la resolución con que Colab muestra los gráficos en el notebook
options(repr.plot.width = 18, repr.plot.height = 10, repr.plot.res = 110)

# arma una pequeña tabla llamada hitos con los tres puntos que se destacan en el primer gráfico: el máximo, el mínimo y el último dato.
hitos <- bind_rows(
  df |> slice_max(tasa, n = 1, with_ties = FALSE) |> mutate(txt = paste0("Máximo: ", num_es(tasa), "%\n", etq_fecha(fecha))),
  df |> slice_min(tasa, n = 1, with_ties = FALSE) |> mutate(txt = paste0("Mínimo: ", num_es(tasa), "%\n", etq_fecha(fecha))),
  df |> slice_tail(n = 1)      |> mutate(txt = paste0("Último: ", num_es(tasa), "%\n", etq_fecha(fecha)))
)

# construye el gráfico (la serie de tiempo) y lo guarda como g1
g1 <- ggplot(df, aes(fecha)) +
  annotate("rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2021-06-01"),
           ymin = -Inf, ymax = Inf, fill = "#F3E3D8", alpha = .6) +
  annotate("text", x = as.Date("2020-10-15"), y = 1.2, label = "Pandemia\nCOVID-19",
           size = 3.4, colour = "#A0522D", lineheight = .9) +
  geom_area(aes(y = tasa), fill = col_principal, alpha = .07) +
  geom_line(aes(y = tasa, colour = "Inflación interanual"), linewidth = .9) +
  geom_line(aes(y = media_12m, colour = "Promedio móvil 12 meses"),
            linewidth = .9, linetype = "22", na.rm = TRUE) +
  geom_point(data = hitos, aes(y = tasa), size = 3.2, colour = col_acento,
             fill = "white", shape = 21, stroke = 1.6) +
  geom_label(data = hitos, aes(y = tasa, label = txt), size = 3.2, vjust = -0.35,
             linewidth = 0, fill = alpha("white", .85), colour = "#333333", lineheight = .95) +
  scale_colour_manual(NULL, values = c("Inflación interanual" = col_principal,
                                       "Promedio móvil 12 meses" = col_acento)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y",
               expand = expansion(mult = c(.01, .04))) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = "%"),
                     limits = c(0, max(df$tasa) + 2.2), expand = expansion(mult = c(0, .02))) +
  labs(title = "Inflación interanual en Chile",
       subtitle = paste0("Variación del IPC en 12 meses (%), ",
                         etq_fecha(min(df$fecha)), " – ", etq_fecha(max(df$fecha))),
       x = NULL, y = NULL, caption = fuente) +
  tema

print(g1)


# define el tamaño y la resolución con que Colab muestra los gráficos en el notebook
options(repr.plot.width = 18, repr.plot.height = 10)

# construye el mapa de calor y lo guarda como g2
g2 <- ggplot(df, aes(factor(mes, levels = 1:12, labels = meses_es),
                     factor(anio, levels = rev(sort(unique(anio)))),
                     fill = tasa)) +
  geom_tile(colour = "white", linewidth = .6) +
  geom_text(aes(label = num_es(tasa), colour = tasa > 7), size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1A1A1A")) +
  scale_fill_gradient(low = "#FDEDEC", high = "#B71C1C", name = "Inflación (%)") +
  labs(title = "Mapa de calor de la inflación interanual en Chile",
       subtitle = "Variación del IPC en 12 meses (%) por año y mes", x = NULL, y = NULL, caption = fuente) +
  tema + theme(panel.grid = element_blank(), legend.position = "right")

print(g2)


# define el tamaño y la resolución con que Colab muestra los gráficos en el notebook
options(repr.plot.width = 18, repr.plot.height = 10)

# construye el gráfico de barras de la variación en 12 meses y lo guarda como g3
g3 <- df |>
  filter(!is.na(var_12m)) |>
  ggplot(aes(fecha, var_12m, fill = var_12m > 0)) +
  geom_col(width = 25) +
  geom_hline(yintercept = 0, colour = "#333333") +
  scale_fill_manual(NULL, values = c(`TRUE` = col_acento, `FALSE` = col_principal),
                    labels = c(`TRUE` = "Sube", `FALSE` = "Baja")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(breaks = breaks_width(1), labels = label_number(accuracy = 1, suffix = " pp")) +
  labs(title = "Cambio de la inflación interanual respecto de 12 meses atrás",
       subtitle = "Diferencia entre la inflación interanual actual y la del mismo mes del año anterior (puntos porcentuales)",
       x = NULL, y = NULL, caption = fuente) +
  tema

print(g3)


ggsave("inflacion_serie.png",       g1, width = 12, height = 6.5, dpi = 300)
ggsave("inflacion_mapa_calor.png",  g2, width = 11, height = 8,   dpi = 300)
ggsave("inflacion_variacion12m.png", g3, width = 12, height = 5.5, dpi = 300)
list.files(pattern = "\\.png$")