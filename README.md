# The Geography of Survival: Global Child Mortality Analytics

An interactive data storytelling and predictive analysis initiative implemented in **R** and structured via the **Xaringan Presentation Engine**. This project conducts a programmatic, multivariate statistical audit on dynamic human development metrics to evaluate the macro-level socioeconomic drivers behind global under-five mortality rates (U5MR).

## 🚀 Live Interactive Presentation
Instead of compiling the code locally, you can explore the fully dynamic deployment with live responsive charts here:
👉 **[View Live Interactive Slides](https://dilyshuynh3864.github.io/child-mortality-slides/)**

---

## 📊 Analytical Scope & Insights
The statistical scripts actively layered disparate environmental and structural datasets to isolate critical determinants of pediatric survival:
* **Socioeconomic Wealth Thresholds:** Log-transformed GDP per Capita profiles evaluating non-linear economic returns on basic healthcare safety nets.
* **Maternal Education Returns:** Linear regression models testing correlation vectors between average female schooling years and survival advantages.
* **Infrastructure Mismatches:** Geospatial distribution anomalies cross-examining localized drinking water access against lagging sanitation pipelines.
* **Clinical Workforce Density:** Multivariate facet mapping isolating the immediate risk multipliers triggered by systemic doctor and nurse shortages per 10,000 citizens.

---

## 🛠️ Tech Stack & Core Libraries
* **Core Language:** R (v4.x)
* **Presentation Engine:** `xaringan::moon_reader` (HTML5/CSS3 Slide Architecture)
* **Data Manipulation:** `tidyverse` (`dplyr`, `tidyr`, `purrr`)
* **Interactive Visualization:** `plotly`, `ggplot2`
* **Geospatial Processing:** `maps`

---

## 💻 Local Replication & Setup
To run the analysis pipeline or compile the presentation slides locally on your machine, follow these steps:

### 1. Prerequisites
Ensure you have **R** and **RStudio** installed. Open your R console and install the mandatory dependencies:

install.packages(c("tidyverse", "plotly", "maps", "viridis", "xaringan"))

### 2. Execution
1. Clone this repository to your local directory:
   git clone https://github.com/dilyshuynh3864/child-mortality-slides.git

2. Open the project folder in RStudio.
3. Open the primary RMarkdown file (`index.Rmd`).
4. Click the **"Knit"** button in RStudio to render the presentation into a standalone interactive HTML document.

---

## 📂 Data Sources & Acknowledgments
All indicators used in this research utilize audited open-source data from international health repositories:
* **Under-Five Mortality Statistics:** UNICEF Child Survival Data
* **GDP per Capita & Population Allocation:** World Bank Open Data
* **Medical Workforce & Water Infrastructure Metrics:** World Health Organization (WHO) Data GHO
* **Maternal Education Profiles:** Our World in Data / UNESCO
