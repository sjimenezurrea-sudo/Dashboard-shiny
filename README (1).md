# Pantallas, sueño y salud mental en adolescentes

Dashboard interactivo desarrollado con **R Shiny** para explorar la relación entre el uso de pantallas, las características del sueño y los síntomas depresivos en adolescentes.

El proyecto presenta análisis exploratorios, visualizaciones interactivas y una segmentación no supervisada de perfiles de pantalla y sueño. Su propósito es facilitar la comprensión de patrones estadísticos en la muestra, **sin interpretar las asociaciones como relaciones causales ni como diagnósticos clínicos**.

## Acceso al proyecto

- **R Book:** [Documentación, marco teórico, metodología y análisis](https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/)
- **Repositorio:** [Dashboard-shiny](https://github.com/sjimenezurrea-sudo/Dashboard-shiny)
- **Dataset:** [Screen Time vs Mental Health (ML-ready) — Kaggle](https://www.kaggle.com/datasets/kylefengkfeng209/screen-time-vs-mental-health-ml-ready)

## Datos y alcance

El dashboard trabaja con **4.810 registros** y **10 variables**, entre ellas indicadores de tiempo de pantalla, calidad y duración del sueño, cronotipo, jet lag social, puntaje BDI-II y clasificación del estado depresivo.

Se emplea el punto de corte **BDI-II ≥ 14** para la clasificación exploratoria de síntomas depresivos; esta clasificación **no sustituye una evaluación o diagnóstico clínico**.

## Funcionalidades

| Módulo | Contenido |
|---|---|
| **Proyecto** | Contexto, marco teórico, objetivos y metodología. |
| **Calidad de datos** | Estructura del dataset, tipos de variables, estadísticos descriptivos y revisión de integridad. |
| **Variable objetivo** | Distribución del BDI-II y proporción de adolescentes según estado depresivo. |
| **Análisis por sexo** | Comparaciones descriptivas, tablas de contingencia y pruebas estadísticas. |
| **Variables numéricas** | Histogramas, diagramas de caja e indicadores interactivos. |
| **Correlaciones** | Matriz de Spearman y asociaciones entre pantalla, sueño y BDI-II. |
| **Valores atípicos** | Identificación mediante la regla del rango intercuartílico (IQR). |
| **Segmentación de adolescentes** | Agrupamiento K-means, visualización PCA, perfiles estandarizados y prevalencia de síntomas depresivos por grupo. |
| **Cierre** | Hallazgos exploratorios y limitaciones. |
| **Estudios y recursos** | Literatura revisada, información del equipo y vínculos del proyecto. |

La interfaz incluye navegación por secciones, visualizaciones con **Plotly** y selector de **tema claro/oscuro**.

## Tecnologías

- **R** y **Shiny** para la aplicación interactiva.
- **tidyverse** y **ggplot2** para transformación de datos y gráficos.
- **plotly** para visualizaciones interactivas.
- **DT** para tablas.
- **bslib** y **CSS** para la interfaz.
- **K-means** y **PCA** para la segmentación y su representación visual.

## Estructura del proyecto

```text
Dashboard-shiny/
├── global.R
├── ui.R
├── server.R
├── screen_time_mental_health.csv
├── www/
│   ├── styles.css
│   ├── theme-plotly.js
│   └── logo.png
└── README.md
```

> La estructura corresponde a los archivos utilizados por la aplicación. El CSV y los recursos gráficos deben estar presentes localmente para ejecutarla; comprueba cuáles de estos archivos están publicados en el repositorio.

## Instalación y ejecución

**1.** Clona el repositorio:

```bash
git clone https://github.com/sjimenezurrea-sudo/Dashboard-shiny.git
cd Dashboard-shiny
```

**2.** Instala las dependencias principales desde R o RStudio:

```r
install.packages(c(
  "shiny", "tidyverse", "ggplot2", "DT", "plotly",
  "bslib", "caret", "pROC", "randomForest", "shinyWidgets"
))
```

**3.** Verifica que `screen_time_mental_health.csv` esté en la raíz del proyecto y que `www/` contenga los archivos de estilos, JavaScript y logo.

**4.** Inicia la aplicación:

```r
shiny::runApp()
```

La URL del libro está disponible en la propia aplicación y también puede configurarse mediante la variable de entorno `JUPYTER_BOOK_URL`, según la versión de `global.R` utilizada.

## Consideraciones metodológicas

- Las correlaciones y las diferencias entre grupos describen **asociaciones**, no efectos causales.
- Los perfiles de K-means dependen de las variables, su estandarización y el número de grupos seleccionado.
- El gráfico PCA es una **proyección bidimensional** y no reproduce toda la información usada para agrupar.
- La comparación de depresión entre perfiles es exploratoria. Los perfiles no son diagnósticos ni categorías de riesgo validadas.
- La interpretación debe considerar las limitaciones de medición, selección de muestra y generalización.

## Estudios de referencia

- [Hökby et al. (2023) — *International Journal of Environmental Research and Public Health*](https://www.mdpi.com/1660-4601/20/4/3771)
- [Lemke et al. (2023) — *PLOS ONE*](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0293580)
- [Hökby et al. (2025) — *PLOS Global Public Health*](https://journals.plos.org/globalpublichealth/article?id=10.1371/journal.pgph.0004262)
- [Chen et al. (2026) — DOI](https://doi.org/10.1186/s12911-026-03482-5)

Para la discusión y referencias completas, consulta el [R Book del proyecto](https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/).

## Equipo

Proyecto académico desarrollado en el contexto de la **Universidad del Norte**. La aplicación incluye una sección de equipo y recursos donde se presenta a sus integrantes.

---

**Nota:** Este dashboard es una herramienta académica de análisis de datos. Sus resultados no deben utilizarse como diagnóstico individual ni para tomar decisiones clínicas.
