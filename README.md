
# Pantallas, sueño y salud mental en adolescentes

**Dashboard interactivo desarrollado con R Shiny** para explorar la relación entre el uso de pantallas, los dominios del sueño y los síntomas depresivos en adolescentes.

Este proyecto combina análisis exploratorio de datos (EDA), visualizaciones interactivas, pruebas estadísticas y segmentación no supervisada mediante *K-means*. Su propósito es describir patrones y generar hipótesis, no establecer relaciones causales ni diagnósticos clínicos.

## Enlaces del proyecto

- **[R Book: documentación del proyecto](https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/)**
- **[Repositorio de GitHub](https://github.com/sjimenezurrea-sudo/Dashboard-shiny)**
- **[Dataset en Kaggle: Screen Time vs Mental Health (ML-ready)](https://www.kaggle.com/datasets/kylefengkfeng209/screen-time-vs-mental-health-ml-ready)**

## Datos y alcance

| Característica | Descripción |
|---|---|
| Observaciones | 4.810 adolescentes |
| Variables | 10 |
| Predictores de pantalla | `screen_time_index`, `est_leisure_screen_hours` |
| Dominios del sueño | `sleep_quality_index`, `avg_sleep_hours`, `midsleep_weekend_hours`, `social_jetlag_hours` |
| Resultados de salud mental | `bdi_total`, `depressed` |
| Identificador | `subject_id` |

La clasificación de síntomas depresivos se analiza utilizando el punto de corte **BDI-II ≥ 14**, conforme a la definición empleada en el proyecto. Este punto de corte no equivale a un diagnóstico clínico.

## Módulos del dashboard

### Proyecto

- **Inicio:** contexto, tamaño de muestra e indicadores generales.
- **Marco teórico:** antecedentes y fundamentos estadísticos.
- **Objetivos:** objetivo general y objetivos específicos.
- **Metodología:** etapas de exploración, preprocesamiento, modelado y evaluación propuestas.

### Análisis exploratorio de datos (EDA)

- **Calidad de datos:** estructura, valores faltantes, duplicados, tipos y estadísticos descriptivos.
- **Variable objetivo:** distribución de `bdi_total` y clasificación `depressed`.
- **Análisis por sexo:** comparaciones descriptivas, chi-cuadrado y Mann–Whitney.
- **Variables numéricas:** histogramas, diagramas de caja y medidas descriptivas.
- **Correlaciones:** matriz de Spearman y asociaciones entre pantalla, sueño y BDI-II.
- **Outliers:** identificación de valores atípicos mediante la regla de Tukey (1,5 × RIQ).
- **Segmentación:** perfiles obtenidos con K-means, representación PCA, prevalencia de síntomas depresivos por perfil y asociación mediante chi-cuadrado y V de Cramér.

### Otras secciones

- **Cierre:** hallazgos exploratorios y limitaciones.
- **Estudios:** antecedentes científicos consultados.
- **Acerca de:** equipo de trabajo y recursos del proyecto.

## Metodología de segmentación

Se agrupan adolescentes a partir de seis variables de pantalla y sueño, previamente estandarizadas. La variable `depressed` y el puntaje `bdi_total` **no se utilizan para construir los grupos**; se examinan después para describir sus diferencias.

La estandarización de cada variable se expresa como:

$$
z_{ij} = \frac{x_{ij}-\bar{x}_j}{s_j}
$$

La prevalencia de síntomas depresivos dentro de un perfil se calcula mediante:

$$
\text{Prevalencia}_k = \frac{n_{\text{deprimidos},k}}{n_k}\times 100
$$

La asociación entre la pertenencia a un perfil y el estado depresivo se explora mediante la prueba de independencia chi-cuadrado y el tamaño del efecto V de Cramér. La segmentación es exploratoria: los perfiles no constituyen diagnósticos ni categorías clínicas validadas.

## Tecnologías

- **R** y **Shiny** para la aplicación interactiva.
- **tidyverse** y **ggplot2** para procesamiento y gráficos.
- **Plotly** para visualizaciones interactivas.
- **DT** para tablas dinámicas.
- **bslib** y **CSS** para la interfaz y los temas claro/oscuro.
- **shinyWidgets** para controles de usuario.

El proyecto también contempla herramientas como `caret`, `pROC` y `randomForest` para fases de modelado.

## Ejecución local

1. Clona el repositorio:

   ```bash
   git clone https://github.com/sjimenezurrea-sudo/Dashboard-shiny.git
   ```

2. Abre el proyecto en RStudio e instala las dependencias, si todavía no las tienes:

   ```r
   install.packages(c(
     "shiny", "tidyverse", "DT", "plotly", "bslib",
     "caret", "pROC", "randomForest", "shinyWidgets"
   ))
   ```

3. Asegúrate de tener el archivo `screen_time_mental_health.csv` en la carpeta raíz del proyecto. Puedes obtener el conjunto de datos en el enlace de Kaggle indicado arriba.

4. Ejecuta desde esa carpeta:

   ```r
   shiny::runApp()
   ```

La estructura esperada es:

```text
Dashboard-shiny/
├── global.R
├── ui.R
├── server.R
├── screen_time_mental_health.csv
├── README.md
└── www/
    ├── styles.css
    ├── theme-plotly.js
    └── logo.png
```

## Estudios de referencia

- [Hökby et al. (2023) — *International Journal of Environmental Research and Public Health*](https://www.mdpi.com/1660-4601/20/4/3771)
- [Lemke et al. (2023) — *PLOS ONE*](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0293580)
- [Hökby et al. (2025) — *PLOS Global Public Health*](https://journals.plos.org/globalpublichealth/article?id=10.1371/journal.pgph.0004262)
- [Chen et al. (2026) — artículo identificado por DOI](https://doi.org/10.1186/s12911-026-03482-5)

## Consideraciones de interpretación

Los resultados son **exploratorios** y no permiten inferir causalidad. Las asociaciones entre pantalla, sueño y síntomas depresivos deben interpretarse en el contexto del diseño y las limitaciones de los datos. Las diferencias entre perfiles no implican que pertenecer a un grupo cause depresión.

---

**Proyecto académico · Universidad del Norte**  
Documentación ampliada: [R Book](https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/)
