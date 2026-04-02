# Digital Divide Insights Dashboard

An interactive Shiny dashboard exploring internet access and usage gaps across low- and middle-income countries (LMICs), with a focus on coverage, usage, and gender dimensions of the digital divide.

**Live app:** https://yunaliang-dashboard.share.connect.posit.cloud/

---

## Overview

Today, over 1 billion people across LMICs remain offline. This dashboard draws on the latest data from the **Global Findex Digital Connectivity Tracker 2025** and the **International Telecommunication Union (ITU)** to help users understand *why* people are offline and *where* the biggest gaps lie.

The dashboard frames the digital divide through three gaps:

| Gap | Definition |
|---|---|
| **Coverage Gap** | Adults living outside mobile broadband network reach |
| **Usage Gap** | Adults with coverage who do not use the internet |
| **Gender Gap** | Difference in internet usage rates between men and women |

---

## Features

### Home
- Global headline statistics on the three gaps
- Policy scenario analysis: estimated new internet users if coverage, usage, or gender gaps were closed

### Country Analysis
- **Diagnostic tab**: country-level profile with gap severity, regional benchmarking, and policy impact scenarios
- **Gender Deep Dive tab**: internet users and usage gap breakdown by gender, plus offline population donuts by gender

### Global Analysis
- Interactive treemaps of coverage gap, usage gap, and women offline by region and country
- Regional bar chart rankings
- Top 10 country tables

### Comparative View
- Side-by-side comparison of up to several countries across internet usage, gender gap, coverage gap, and usage gap

---

## Data

| Source | Coverage | Variables |
|---|---|---|
| Global Findex Digital Connectivity Tracker 2025 | ~81 LMICs | Internet usage rates, coverage, gender breakdown |
| ITU (2022–24) | Global | Supplementary connectivity indicators |

**Country coverage note:** This dashboard covers 81 LMICs. The Findex Tracker collects data for 90 LMICs. Seven countries interviewed exclusively by phone are excluded. West Bank and Gaza and Mauritania are excluded due to incomplete coverage data.

---

## Running Locally

**Requirements:** R 4.x and the following packages:

```r
install.packages(c("shiny", "tidyverse", "plotly", "DT", "shinythemes", "scales"))
```

**Steps:**

```r
# Clone the repo, then from the project directory:
shiny::runApp()
```

The app expects `adoption_data.rds` and `www/hero.png` to be present in the project root.

---

## Deployment

The app is deployed on **Posit Connect Cloud**. To redeploy after changes:

```r
library(rsconnect)
deployApp()
```

---

## Repository Structure

```
dashboard/
├── app.R                  # Single-file Shiny app (UI + server)
├── adoption_data.rds      # Main dataset (not versioned if large)
└── www/
    └── hero.png           # Hero image for home tab
```

---

## Authors

World Bank — Development Data Group
