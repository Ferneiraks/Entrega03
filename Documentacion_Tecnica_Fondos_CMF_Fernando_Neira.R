# =============================================================================
# GENERADOR DE DOCUMENTACIÓN TÉCNICA PDF - FONDOS MUTUOS CMF ()  
# =============================================================================

# 1. CARGAR LIBRERÍAS NECESARIAS
library(rmarkdown)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# 2. CONFIGURACIÓN GLOBAL
GENERAR_DOCUMENTACION_PDF <- TRUE
NOMBRE_ARCHIVO_PDF <- "Documentacion_Tecnica_Fondos_CMF_Fernando_Neira.pdf"

# 3. FUNCIÓN PARA GENERAR DOCUMENTACIÓN TÉCNICA EN PDF
generar_documentacion_tecnica <- function(generar_pdf = TRUE) {
  
  cat("[INFO] GENERANDO DOCUMENTACION TECNICA ...\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  # Crear contenido R Markdown
  contenido_rmd <- '
---
title: "DOCUMENTACIÓN TÉCNICA - Sistema de Análisis de Fondos Mutuos CMF"
subtitle: "Ciencia de datos para finanzas en R| Julio 2025"
author: "Fernando Neira"
date: "`r Sys.Date()`"
output:
  pdf_document:
    toc: true
    toc_depth: 3
    number_sections: true
    fig_width: 10
    fig_height: 6
    latex_engine: xelatex
    keep_tex: false
header-includes:
  - \\usepackage{float}
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{array}
  - \\usepackage{multirow}
  - \\usepackage{wrapfig}
  - \\usepackage{colortbl}
  - \\usepackage{pdflscape}
  - \\usepackage{tabu}
  - \\usepackage{threeparttable}
  - \\usepackage{geometry}
  - \\geometry{margin=2cm}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = FALSE, 
  warning = FALSE, 
  message = FALSE,
  fig.pos = "H"
)
```

\\newpage

# DESCRIPCIÓN DEL MODELO

## Propósito del Sistema

El sistema desarrollado automatiza el análisis cuantitativo de fondos mutuos utilizando datos oficiales de la CMF con los siguientes objetivos:

- Evaluar performance de fondos mediante métricas financieras estándar (Alpha, Sharpe, volatilidad)
- Implementar modelo CAPM para cálculo de retorno anormal ajustado por riesgo
- Generar reportes automatizados para toma de decisiones de inversión
- Proporcionar rankings y recomendaciones basadas en análisis cuantitativo

## Metodología de Análisis

### Fuente de Datos
**Origen:** Información oficial de la Comisión para el Mercado Financiero (CMF)
**Actualización:** Quincenal/Mensual según disponibilidad CMF
**Formato:** Archivos delimitados descargados directamente del portal CMF

### Modelo Financiero Implementado

```{r tabla-modelo-financiero}
modelo_financiero <- data.frame(
  Componente = c("Modelo CAPM", "Métricas de Riesgo", "Análisis Comparativo", "Benchmarking"),
  Descripcion = c("Cálculo de Alpha y Beta vs mercado",
                 "Volatilidad, Sharpe Ratio, VaR",
                 "Rankings relativos entre fondos",
                 "Comparación vs TPM y mercado"),
  Formula_Clave = c("Alpha = R_fondo - (TPM + Beta*(R_mercado - TPM))",
                   "Sharpe = (R_fondo - TPM) / Volatilidad",
                   "Ranking por Alpha ajustado",
                   "% períodos > TPM")
)

kable(modelo_financiero, 
      caption = "Componentes del Modelo Financiero", 
      booktabs = TRUE, 
      longtable = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

## Variables del Sistema

### Variables de Entrada (Fuente CMF)

```{r tabla-variables-entrada}
variables_entrada <- data.frame(
  Variable = c("FECHA_INF", "VALOR_CUOTA", "PATRIMONIO_NETO", "NUM_PARTICIPES", 
               "NOM_ADM", "SERIE"),
  Descripcion = c("Fecha de información", "Valor unitario de la cuota", 
                 "Patrimonio neto del fondo", "Número de partícipes", 
                 "Administradora del fondo", "Serie del fondo"),
  Uso_en_Modelo = c("Serie temporal", "Cálculo retornos", 
                   "Ponderación", "Base de inversores", 
                   "Agrupación", "Clasificación")
)

kable(variables_entrada, 
      caption = "Variables de Entrada desde CMF", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

\\newpage

### Variables Calculadas

```{r tabla-variables-calculadas}
variables_calculadas <- data.frame(
  Variable = c("Retorno Mensual", "Alpha CAPM", "Sharpe Ratio", "Volatilidad", "R²"),
  Formula = c("(Valor_t / Valor_t-1) - 1", "Intercepto regresión CAPM", 
              "(Retorno - TPM) / Desv_Std", "Desviación estándar retornos",
              "Coeficiente determinación"),
  Interpretacion = c("Performance mensual", "Valor agregado vs mercado", 
                    "Retorno ajustado por riesgo", "Medida de riesgo",
                    "Calidad del modelo")
)

kable(variables_calculadas, 
      caption = "Variables Calculadas por el Sistema", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

\\newpage

# LIMPIEZA Y TRANSFORMACIÓN DE DATOS

## Proceso de Validación

### Criterios de Calidad de Datos CMF

```{r tabla-criterios-calidad}
criterios_calidad <- data.frame(
  Criterio = c("Validación Temporal", "Validación Numérica", "Validación Estructural"),
  Inclusión = c("Fechas consecutivas sin gaps > 30 días",
               "Valores cuota > 0 y variaciones < 500%", 
               "Mínimo 3 observaciones por fondo"),
  Exclusión = c("Fondos con datos inconsistentes temporalmente",
               "Outliers extremos o valores nulos", 
               "Fondos sin información suficiente")
)

kable(criterios_calidad, 
      caption = "Criterios de Calidad para Datos CMF", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

### Transformaciones Aplicadas

1. **Normalización de Fechas:** Conversión a formato estándar Date
2. **Cálculo de Retornos:** Logarítmicos para análisis estadístico
3. **Identificación Única:** Combinación Administradora + Serie
4. **Filtrado de Outliers:** Retornos fuera de [-50%, +500%] mensual

\\newpage

# ARQUITECTURA DEL SISTEMA

## Flujo de Procesamiento Ampliado

```{r diagrama-arquitectura-ampliado, fig.cap="Arquitectura Completa del Sistema", fig.width=14, fig.height=10}
# Crear diagrama de arquitectura ampliado
library(ggplot2)

# Datos para el diagrama ampliado
arquitectura_data <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8),
  y = c(3, 3, 2, 2, 2, 1, 1, 1),
  componente = c("Portal CMF", "Descarga", "Validación", "Limpieza", 
                "CAPM", "Análisis", "Reportes", "Alertas"),
  tipo = c("source", "input", "process", "process", 
          "analysis", "analysis", "output", "monitoring"),
  descripcion = c("Datos oficiales\\nCMF", "Carga\\nautomática", 
                 "Validación\\nintegridad", "Filtros y\\ntransformación",
                 "Modelo\\nfinanciero", "Rankings y\\nmétricas", 
                 "PDF y\\nvisualización", "Sistema\\nmonitoreo")
)

# Crear el gráfico
ggplot(arquitectura_data, aes(x = x, y = y)) +
  # Rectángulos para cada componente
  geom_rect(aes(xmin = x - 0.4, xmax = x + 0.4, 
                ymin = y - 0.25, ymax = y + 0.25, 
                fill = tipo), alpha = 0.7, color = "black", size = 0.8) +
  
  # Texto principal
  geom_text(aes(label = componente), size = 3.5, fontface = "bold", vjust = 0.1) +
  
  # Descripción adicional
  geom_text(aes(label = descripcion), size = 2.5, vjust = -0.8, color = "darkblue") +
  
  # Colores para diferentes tipos
  scale_fill_manual(values = c("source" = "#FFE6E6", 
                              "input" = "#E6F2FF", 
                              "process" = "#E6FFE6", 
                              "analysis" = "#FFF2E6",
                              "output" = "#F2E6FF",
                              "monitoring" = "#FFFFE6")) +
  
  # Flechas de flujo principal
  annotate("segment", x = 1.4, y = 3, xend = 1.6, yend = 3, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1.2, color = "darkblue") +
  annotate("segment", x = 2.4, y = 3, xend = 2.6, yend = 2.25, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1.2, color = "darkblue") +
  annotate("segment", x = 3.4, y = 2, xend = 3.6, yend = 2, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1.2, color = "darkblue") +
  annotate("segment", x = 4.4, y = 2, xend = 4.6, yend = 2, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1.2, color = "darkblue") +
  annotate("segment", x = 5.4, y = 2, xend = 5.6, yend = 1.25, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1.2, color = "darkblue") +
  annotate("segment", x = 6.4, y = 1, xend = 6.6, yend = 1, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1.2, color = "darkblue") +
  
  # Flujo de monitoreo
  annotate("segment", x = 6, y = 1.25, xend = 8, yend = 1.25, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1, 
           color = "red", linetype = "dashed") +
  annotate("segment", x = 8, y = 1.25, xend = 5, yend = 2.25, 
           arrow = arrow(length = unit(0.3, "cm")), size = 1, 
           color = "red", linetype = "dashed") +
  
  # Etiquetas de flujo
  annotate("text", x = 2, y = 3.5, label = "Flujo Principal", 
           size = 4, fontface = "bold", color = "darkblue") +
  annotate("text", x = 7, y = 0.5, label = "Monitoreo y Feedback", 
           size = 3, fontface = "italic", color = "red") +
  
  # Configuración del gráfico
  xlim(0.3, 8.7) + ylim(0.2, 4) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Arquitectura Completa del Sistema de Análisis de Fondos CMF",
       subtitle = "Flujo de Procesamiento con Retroalimentación")
```

## Componentes Técnicos

### Módulos del Sistema

```{r tabla-modulos-sistema}
modulos_sistema <- data.frame(
  Módulo = c("Ingesta CMF", "Procesamiento", "Análisis CAPM", "Reportería"),
  Función = c("Lectura y validación datos CMF",
             "Limpieza y transformación", 
             "Cálculo métricas financieras",
             "Generación PDF y visualizaciones"),
  Tecnología = c("R readr + validación", "dplyr + filtros", 
                "Regresión lineal + broom", "rmarkdown + ggplot2"),
  Output = c("Dataset validado", "Series temporales limpias", 
            "Métricas por fondo", "Documentos ejecutivos")
)

kable(modulos_sistema, 
      caption = "Módulos del Sistema de Análisis", 
      booktabs = TRUE, 
      longtable = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

\\newpage

# DESPLIEGUE

## Estrategia de Implementación

### Proceso de Despliegue

**Fase 1: Preparación del Entorno**
- Instalación de R y dependencias
- Configuración de directorios de trabajo
- Verificar carga de datos de los últimos 30 días

**Fase 2: Automatización**
- Configuración de Task Scheduler (Windows)
- Scripts de ejecución quincenal/mensual
- Sistema de logging y alertas

**Fase 3: Validación**
- Pruebas con datos históricos
- Validación de outputs vs cálculos manuales
- Documentación de procedimientos

## Automatización Windows

### Configuración de Tareas Programadas

```r
# Programador de Tareas - Configuración sugerida:
# Nombre: Análisis Fondos CMF
# Frecuencia: Quincenal (cada 2 semanas)
# Horario: A definir.
# Programa: Rscript.exe
# Argumentos: Analisis_fondos_2025.R
```

\\newpage

# SEGUIMIENTO Y MONITOREO

## Estrategia de Monitoreo

### Objetivos del Sistema de Seguimiento

- **Calidad de Datos:** Validar consistencia de información CMF
- **Performance del Modelo:** Monitorear estabilidad de métricas calculadas  
- **Alertas Ejecutivas:** Detectar cambios significativos en rankings
- **Reporting Automático:** Generar informes quincenales/mensuales

## Métricas de Monitoreo

### Indicadores Clave de Performance (KPIs)

```{r tabla-kpis-monitoreo}
kpis_monitoreo <- data.frame(
  KPI = c("Completitud de Datos", "Alpha Promedio Sector", "Fondos Top Performers", 
          "Calidad Modelo CAPM"),
  Descripción = c("% fondos con información completa", 
                 "Alpha promedio ponderado por patrimonio",
                 "Cantidad fondos con Alpha > 2% mensual",
                 "R² promedio de regresiones CAPM"),
  Umbral_Normal = c("> 95%", "0% a 2%", "10-30 fondos", "> 0.4"),
  Frecuencia = c("Quincenal", "Quincenal", "Quincenal", "Mensual")
)

kable(kpis_monitoreo, 
      caption = "KPIs de Monitoreo del Sistema", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

## Sistema de Alertas

### Alertas Automatizadas

```{r tabla-alertas-reducida}
alertas_sistema <- data.frame(
  Tipo = c("CRÍTICO", "ADVERTENCIA", "OPORTUNIDAD"),
  Condición = c("Falla carga datos CMF > 48h", 
               "Alpha sector fuera rango ±3σ histórico",
               "Nuevo fondo top 10 performance"),
  Acción = c("Verificar fuente datos inmediatamente",
            "Revisar modelo y recalibrar parámetros", 
            "Incluir en análisis especial próximo reporte")
)

kable(alertas_sistema, 
      caption = "Sistema de Alertas", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  row_spec(1, background = "#ffcccc") %>%
  row_spec(2, background = "#fff2cc") %>%
  row_spec(3, background = "#ccffcc")
```

## Reportes Automatizados

### Cronograma de Reportes

**Reporte Quincenal (cada 2 semanas):**
- Actualización rankings y métricas clave
- Detección de cambios significativos en top performers
- Análisis de nuevos fondos o administradoras

**Reporte Mensual:**
- Análisis de tendencias del sector
- Evaluación de calidad del modelo CAPM
- Recomendaciones estratégicas actualizadas

\\newpage

### Contenido de Reportes Ejecutivos

```{r tabla-contenido-reportes}
contenido_reportes <- data.frame(
  Sección = c("Top Performers", "Alertas y Cambios", "Recomendaciones", "Resumen Ejecutivo"),
  Contenido = c("Top 10 fondos por Alpha, patrimonio y Sharpe",
               "Fondos que entran/salen de rankings principales", 
               "Sugerencias por perfil de riesgo del inversionista",
               "3 métricas clave + variación vs período anterior"),
  Páginas = c("2 páginas", "1 página", "1 página", "1 página")
)

kable(contenido_reportes, 
      caption = "Estructura de Reportes Automatizados", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

## Mantenimiento del Sistema

### Tareas de Mantenimiento

**Mensual:**
- Backup de datos históricos
- Verificación integridad archivos
- Revisión logs de errores

**Trimestral:**
- Evaluación deriva del modelo
- Actualización umbrales de alerta
- Revisión de metodología vs benchmarks

**Anual:**
- Migración a nuevas versiones R
- Auditoría externa de metodología
- Actualización documentación técnica

\\newpage

# RESUMEN EJECUTIVO EJEMPLO Y CONTENIDO

## Panorama General del Sistema

**Período de Análisis:** Mayo 2025 | **Fondos Analizados:** 298 fondos válidos

### Métricas Clave del Sector

```{r tabla-resumen-ejecutivo}
resumen_ejecutivo <- data.frame(
  Indicador = c("Fondos Válidos Procesados", "Administradoras Activas", 
                "Patrimonio Total Sector", "Partícipes Totales",
                "Rendimiento Promedio Mensual", "Fondos que Superan TPM"),
  Valor = c("298 fondos", "22 administradoras", 
           "$6,457.8 mil millones", "296,513 partícipes",
           "+12.01% mensual", "197/298 (66.1%)"),
  Observacion = c("Universo completo CMF", "Concentración alta en top 5", 
                 "Crecimiento vs período anterior", "Base diversificada",
                 "Superior a expectativas", "Mayoría supera benchmark")
)

kable(resumen_ejecutivo, 
      caption = "Indicadores Clave del Sector - Mayo 2025", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

### Top Performers del Período

**Top 5 Administradoras por Patrimonio:**

```{r tabla-top-administradoras}
top_admin <- data.frame(
  Administradora = c("BCI ASSET MANAGEMENT", "SANTANDER ASSET MANAGEMENT", 
                    "ITAU ADMINISTRADORA", "SCOTIA ADMINISTRADORA", "FINTUAL"),
  Fondos = c("22 fondos", "23 fondos", "16 fondos", "18 fondos", "2 fondos"),
  Patrimonio = c("$1,600.2B", "$1,342.2B", "$680.4B", "$496.3B", "$405.8B"),
  Rentabilidad_Promedio = c("12.97%", "8.02%", "27.12%", "13.27%", "14.04%")
)

kable(top_admin, 
      caption = "Top 5 Administradoras por Patrimonio", 
      booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"))
```

**Hallazgos Principales:**

- **Performance Superior:** El 66.1% de fondos superan la TPM mensual
- **Concentración:** Top 5 administradoras concentran 75% del patrimonio total
- **Volatilidad Controlada:** Rentabilidad mediana de +2.40% mensual indica estabilidad
- **Diversificación:** 298 fondos ofrecen amplio espectro de opciones de inversión


'

# Escribir archivo R Markdown temporal
archivo_temp_rmd <- "documentacion__temp.Rmd"
writeLines(contenido_rmd, archivo_temp_rmd)

# Generar PDF si está habilitado
if(generar_pdf) {
  cat("[INFO] RENDERIZANDO PDF ...\n")
  
  tryCatch({
    # Renderizar a PDF
    rmarkdown::render(
      input = archivo_temp_rmd,
      output_file = NOMBRE_ARCHIVO_PDF,
      output_dir = getwd(),
      clean = TRUE,
      quiet = FALSE
    )
    
    cat("[EXITO] DOCUMENTACION PDF  GENERADA EXITOSAMENTE\n")
    cat("[INFO] Archivo:", file.path(getwd(), NOMBRE_ARCHIVO_PDF), "\n")
    cat("[INFO] Tamaño del archivo:", 
        round(file.size(NOMBRE_ARCHIVO_PDF) / 1024 / 1024, 2), "MB\n")
    
    # Limpiar archivo temporal
    if(file.exists(archivo_temp_rmd)) {
      file.remove(archivo_temp_rmd)
    }
    
    return(TRUE)
    
  }, error = function(e) {
    cat("[ERROR] ERROR AL GENERAR PDF :\n")
    cat("[ERROR]   ", as.character(e), "\n")
    cat("[INFO] SOLUCIONES POSIBLES:\n")
    cat("[INFO]   1. Instalar MiKTeX o tinytex: install.packages('tinytex'); tinytex::install_tinytex()\n")
    cat("[INFO]   2. Verificar que todas las librerías estén instaladas\n")
    cat("[INFO]   3. Verificar permisos de escritura en el directorio\n")
    
    # Limpiar archivo temporal
    if(file.exists(archivo_temp_rmd)) {
      file.remove(archivo_temp_rmd)
    }
    
    return(FALSE)
  })
}
}

# 4. FUNCIÓN PARA VERIFICAR DEPENDENCIAS
verificar_dependencias <- function() {
  cat("[INFO] VERIFICANDO DEPENDENCIAS...\n")
  
  # Lista de paquetes requeridos
  paquetes_requeridos <- c("rmarkdown", "knitr", "kableExtra", "dplyr", 
                           "ggplot2", "gridExtra", "grid", "tinytex")
  
  # Verificar paquetes instalados
  paquetes_faltantes <- c()
  
  for(paquete in paquetes_requeridos) {
    if(!require(paquete, character.only = TRUE, quietly = TRUE)) {
      paquetes_faltantes <- c(paquetes_faltantes, paquete)
    }
  }
  
  if(length(paquetes_faltantes) > 0) {
    cat("[ERROR] PAQUETES FALTANTES:", paste(paquetes_faltantes, collapse = ", "), "\n")
    cat("[INFO] EJECUTAR: install.packages(c(", 
        paste(paste0("'", paquetes_faltantes, "'"), collapse = ", "), "))\n")
    return(FALSE)
  }
  
  # Verificar LaTeX
  if(!tinytex::is_tinytex()) {
    cat("[ADVERTENCIA] LaTeX no detectado. Para generar PDF, ejecutar:\n")
    cat("[INFO]   install.packages('tinytex')\n")
    cat("[INFO]   tinytex::install_tinytex()\n")
    return(FALSE)
  }
  
  cat("[EXITO] Todas las dependencias están instaladas\n")
  return(TRUE)
}

# 5. FUNCIÓN PRINCIPAL PARA EJECUTAR TODO
ejecutar_documentacion_ <- function() {
  
  cat("\n[INICIO] INICIANDO GENERACION DE DOCUMENTACION \n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  # Verificar dependencias primero
  if(!verificar_dependencias()) {
    cat("[INFO] FALTAN DEPENDENCIAS. INSTALANDO...\n")
    
    # Intentar instalar automáticamente
    tryCatch({
      install.packages(c("rmarkdown", "knitr", "kableExtra", "tinytex"), 
                       dependencies = TRUE, quiet = TRUE)
      
      # Instalar tinytex si no está disponible
      if(!require("tinytex", quietly = TRUE)) {
        install.packages("tinytex")
        library(tinytex)
        install_tinytex()
      }
      
      cat("[EXITO] Dependencias instaladas exitosamente\n")
      
    }, error = function(e) {
      cat("[ERROR] ERROR EN INSTALACION AUTOMATICA:\n")
      cat("[ERROR] Por favor instale manualmente los paquetes faltantes\n")
      return(FALSE)
    })
  }
  
  # Generar la documentación
  resultado <- generar_documentacion_tecnica(generar_pdf = GENERAR_DOCUMENTACION_PDF)
  
  if(resultado) {
    cat("\n[EXITO] DOCUMENTACION  GENERADA EXITOSAMENTE\n")
    cat("[INFO] Ubicación:", file.path(getwd(), NOMBRE_ARCHIVO_PDF), "\n")
    cat("[INFO] La documentación  incluye:\n")
    cat("[INFO]   • Descripción del modelo (inicio)\n")
    cat("[INFO]   • Variables calculadas en nueva página\n")
    cat("[INFO]   • Diagrama de flujo ampliado\n")
    cat("[INFO]   • Infraestructura removida\n")
    cat("[INFO]   • Sistema de seguimiento y monitoreo\n")
    cat("[INFO]   • Resumen ejecutivo ejemplo y contenido al final\n")
    
    # Abrir el PDF automáticamente (opcional)
    if(interactive()) {
      respuesta <- readline("¿Desea abrir el PDF automáticamente? (s/n): ")
      if(tolower(respuesta) %in% c("s", "si", "yes", "y")) {
        try(shell.exec(NOMBRE_ARCHIVO_PDF), silent = TRUE)
      }
    }
    
  } else {
    cat("\n[ERROR] ERROR AL GENERAR DOCUMENTACION \n")
    cat("[ERROR] Revise los mensajes de error anteriores\n")
  }
  
  return(resultado)
}

# 6. EJECUTAR LA DOCUMENTACIÓN 
if(GENERAR_DOCUMENTACION_PDF) {
  cat("\n[INICIO] GENERANDO DOCUMENTACION \n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  resultado <- ejecutar_documentacion_()
  
  if(resultado) {
    cat("\n[EXITO] DOCUMENTACION  GENERADA\n")
    cat("[INFO] Cambios implementados:\n")
    cat("  ✓ RESUMEN EJECUTIVO EJEMPLO Y CONTENIDO movido al final\n")
    cat("  ✓ Variables Calculadas en nueva página\n")
    cat("  ✓ Diagrama de flujo ampliado con 8 componentes\n")
    cat("  ✓ Verificación CMF cambiada por verificar datos últimos 30 días\n")
    cat("  ✓ Contenido Reportes Ejecutivos en nueva página\n")
    cat("  ✓ Mejor organización visual\n")
  }
}

# 7. FUNCIÓN DE AYUDA
mostrar_ayuda_ <- function() {
  cat("\n[AYUDA] DOCUMENTACION TECNICA FONDOS CMF - \n")
  cat(paste(rep("-", 55), collapse = ""), "\n")
  cat("FUNCIONES PRINCIPALES:\n")
  cat("• ejecutar_documentacion_() - Generar PDF \n")
  cat("• verificar_dependencias() - Verificar librerías R\n") 
  cat("• mostrar_ayuda_() - Mostrar esta ayuda\n\n")
  cat("CONFIGURACION:\n")
  cat("• GENERAR_DOCUMENTACION_PDF =", GENERAR_DOCUMENTACION_PDF, "\n")
  cat("• NOMBRE_ARCHIVO_PDF =", NOMBRE_ARCHIVO_PDF, "\n\n")
  cat("CAMBIOS S:\n")
  cat("✓ Resumen ejecutivo ejemplo y contenido movido al final\n")
  cat("✓ Variables calculadas en nueva página\n")
  cat("✓ Diagrama de flujo ampliado (8 componentes)\n")
  cat("✓ Verificación CMF cambiada por verificar datos últimos 30 días\n")
  cat("✓ Contenido Reportes Ejecutivos en nueva página\n")
  cat("✓ Sin 'Fin del documento'\n")
  cat("✓ Mejor flujo de lectura técnica\n")
}


#renv::init()
#renv::snapshot()