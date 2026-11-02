\documentclass[11pt,a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[spanish]{babel}
\usepackage{amsmath,amssymb,booktabs,longtable,geometry,hyperref,listings,xcolor}
\geometry{margin=2.5cm}
\hypersetup{colorlinks=true,urlcolor=blue,linkcolor=blue}
\lstset{basicstyle=\ttfamily\small,breaklines=true,columns=fullflexible,frame=single}
\title{Pantallas, sueño y salud mental en adolescentes\\\large Documentación del dashboard R Shiny}
\author{Proyecto académico --- Universidad del Norte}
\date{}
\begin{document}
\maketitle

\section*{Descripción}
Dashboard interactivo desarrollado en \textbf{R Shiny} para explorar los patrones de uso de pantallas, los dominios del sueño y los síntomas depresivos en adolescentes. Reúne análisis exploratorio de datos (EDA), visualizaciones interactivas, pruebas de asociación y segmentación no supervisada.

\textbf{Alcance:} los resultados son exploratorios, no demuestran causalidad ni constituyen diagnósticos clínicos.

\section*{Enlaces del proyecto}
\begin{itemize}
\item \textbf{R Book:} \url{https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/}
\item \textbf{Repositorio:} \url{https://github.com/sjimenezurrea-sudo/Dashboard-shiny}
\item \textbf{Datos:} \url{https://www.kaggle.com/datasets/kylefengkfeng209/screen-time-vs-mental-health-ml-ready}
\end{itemize}

\section*{Datos}
La aplicación trabaja con \textbf{4.810 adolescentes} y \textbf{10 variables}. Para la segmentación se consideran seis variables explicativas: \texttt{screen\_time\_index}, \texttt{est\_leisure\_screen\_hours}, \texttt{sleep\_quality\_index}, \texttt{avg\_sleep\_hours}, \texttt{midsleep\_weekend\_hours} y \texttt{social\_jetlag\_hours}. El análisis de salud mental utiliza \texttt{bdi\_total} y \texttt{depressed}.

El umbral \(\mathrm{BDI-II}\geq14\) se utiliza como criterio de tamizaje para las comparaciones estadísticas. No equivale a un diagnóstico clínico.

\section*{Módulos del dashboard}
\begin{longtable}{p{0.31\textwidth}p{0.6\textwidth}}
\toprule
\textbf{Módulo} & \textbf{Contenido}\\ \midrule
Proyecto & Contexto, marco teórico, objetivos y metodología.\\
Calidad de datos & Integridad, tipos y estadísticas descriptivas.\\
Variable objetivo & Distribución BDI-II y clasificación exploratoria.\\
Análisis por sexo & Prevalencias y contrastes entre grupos.\\
Variables numéricas & Histogramas, boxplots e indicadores.\\
Correlaciones & Coeficientes de Spearman y asociaciones con BDI-II.\\
Outliers & Detección mediante el rango intercuartílico.\\
Segmentación & K-means, PCA, perfiles y prevalencia por grupo.\\
Cierre & Hallazgos y limitaciones.\\
Estudios / Acerca de & Publicaciones revisadas, equipo y recursos.\\
\bottomrule
\end{longtable}

\section*{Fundamentos estadísticos}
\subsection*{Prevalencia}
\begin{equation*}
\widehat P_{\mathrm{dep}}=\frac{n_{\mathrm{dep}}}{n_{\mathrm{válidos}}}\times100\%.
\end{equation*}
Al comparar perfiles, el denominador corresponde a las observaciones válidas de cada perfil.

\subsection*{Estandarización y K-means}
\begin{align*}
z_{ij}&=\frac{x_{ij}-\overline x_j}{s_j},\\
\underset{C_1,\ldots,C_K}{\min}\quad &\sum_{k=1}^{K}\sum_{i\in C_k}\|\mathbf z_i-\boldsymbol\mu_k\|^2.
\end{align*}
La proyección PCA muestra los perfiles en dos dimensiones, sin conservar necesariamente toda la información del espacio de agrupamiento.

\subsection*{Asociación con estado depresivo}
\begin{align*}
\chi^2 &= \sum_i\sum_j\frac{(O_{ij}-E_{ij})^2}{E_{ij}},\\
V &= \sqrt{\frac{\chi^2}{n\min(r-1,c-1)}}.
\end{align*}
La prueba es exploratoria: no demuestra causalidad ni valida por sí sola el número elegido de grupos.

\section*{Tecnologías}
R, Shiny, tidyverse, ggplot2, plotly, DT, bslib, CSS y JavaScript. K-means y PCA se utilizan para la segmentación y representación visual.

\section*{Estructura}
\begin{lstlisting}
Dashboard-shiny/
|-- global.R
|-- ui.R
|-- server.R
|-- screen_time_mental_health.csv
|-- www/
|   |-- styles.css
|   |-- theme-plotly.js
|   `-- logo.png
`-- README.md
\end{lstlisting}

\section*{Instalación y ejecución}
Clonar el repositorio:
\begin{lstlisting}[language=bash]
git clone https://github.com/sjimenezurrea-sudo/Dashboard-shiny.git
cd Dashboard-shiny
\end{lstlisting}
Instalar las dependencias y ejecutar en R o RStudio:
\begin{lstlisting}[language=R]
install.packages(c(
  "shiny", "tidyverse", "ggplot2", "DT", "plotly",
  "bslib", "caret", "pROC", "randomForest", "shinyWidgets"
))
shiny::runApp()
\end{lstlisting}
El CSV y los archivos de \texttt{www/} deben estar presentes localmente. La documentación complementaria está disponible en el R Book.

\section*{Investigaciones de referencia}
\begin{itemize}
\item Hökby et al. (2023): \url{https://www.mdpi.com/1660-4601/20/4/3771}
\item Lemke et al. (2023): \url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0293580}
\item Hökby et al. (2025): \url{https://journals.plos.org/globalpublichealth/article?id=10.1371/journal.pgph.0004262}
\item Chen et al. (2026): \url{https://doi.org/10.1186/s12911-026-03482-5}
\end{itemize}

\section*{Equipo y uso responsable}
Proyecto académico desarrollado en el contexto de la \textbf{Universidad del Norte}. La aplicación contiene información del equipo en la sección \emph{Acerca de}.

Las asociaciones observadas no prueban relaciones causales. Los perfiles no son categorías clínicas y los resultados no deben emplearse en diagnósticos o decisiones clínicas individuales.

\end{document}
