# Proyecto – Módulo 8  
# **Análisis de Popularidad en Spotify**

**Integrantes Equipo 13**
- Candelas Juárez Diego
- Domínguez Sánchez Oscar
- Lira González Rosa Linda.
- Ochoa Campos Ana Sofía
- Pérez Rojas Alberto

Este repositorio contiene el proyecto final del **Módulo 8: Visualización de Resultados** del Diplomado en Ciencia de Datos.  
El objetivo principal es analizar los factores acústicos que influyen en la popularidad de una canción en Spotify, utilizando análisis exploratorio, técnicas de visualización y modelos predictivos.

---

## 📊 **Dashboard interactivo**

El proyecto incluye una aplicación **Shiny** donde se pueden explorar:

- Relación entre popularidad y danceability  
- Histogramas interactivos de energía y tempo  
- Distribuciones por meta-género  
- Tendencias generales  
- Top artistas en el dataset  

**Link al al sitio de visualización (Dashboard):**  
https://soffochoa.shinyapps.io/proyecto-modulo8-spotify-popularity/

---

## 📄 **Reporte completo del proyecto**

El reporte final incluye:

- Limpieza y preparación del dataset  
- Análisis exploratorio de datos (EDA)  
- Clustering basado en características acústicas  
- Modelos predictivos:  
  - Regresión lineal  
  - Random Forest  
  - XGBoost  
- Interpretación detallada de resultados  

**Publicación del reporte (RPubs):**  
https://rpubs.com/Oscar_dsc/1371200

---

## 📁 **Contenido del repositorio**

### **1. `Proyecto final Modulo8.Rmd`**
Archivo principal del proyecto.  
Contiene:
- Código completo del análisis en R  
- Gráficas del EDA  
- Modelos predictivos  
- Conclusiones finales  
- Estructura clara para generar un HTML con índice y código plegable  

Es el archivo base desde el cual se genera el reporte en HTML.

---

### **2. `Proyecto-final-Modulo8_V2.html`**
Versión renderizada del `.Rmd`.  
Es el reporte en formato HTML generado con el template visual (tema Bootstrap, índice flotante, código plegable).

Puede abrirse directamente en el navegador.

---

### **3. `app.R`**
Script completo de la aplicación **Shiny** utilizada para generar el dashboard interactivo publicado en shinyapps.io.

Incluye:
- UI con pestañas (Exploración, Predicción, Top Artistas, Tendencias)  
- Server con lógica para filtros, gráficas y renderizados  
- Preprocesamiento de datos dentro de la app

---

### **4. `dataset.csv`**
Dataset utilizado para:

- EDA  
- Modelos predictivos  
- Dashboard Shiny  

Corresponde al dataset de Spotify tracks descargado de Kaggle:
https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset

---

