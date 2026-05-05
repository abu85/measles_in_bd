# Recent Measles Cases in Bangladesh

This repository contains data and scripts for compiling and analyzing **recent measles cases in Bangladesh**, with a primary focus on official figures from the **World Health Organization (WHO)** and supplementary updates from national news reports.

---

## 📊 Data Sources

### 1. World Health Organization (WHO)

Primary dataset: **Measles – Reported Cases and Incidence**

- Global overview:  
  https://immunizationdata.who.int/global/wiise-detail-page/measles-reported-cases-and-incidence

- Bangladesh-specific view (select year as needed):  
  https://immunizationdata.who.int/global/wiise-detail-page/measles-reported-cases-and-incidence?CODE=BGD&YEAR=

**Steps**
1. Select **Country: Bangladesh**
2. Select the desired **Year**
3. Download the table as a CSV or Excel file
4. Save the file in the project’s data directory

---

### 2. Supplementary 2026 Data (News Report)

To include provisional or emerging information not yet available in WHO datasets, manually add **2026 data** based on the following report:

- *“10 children die of measles, similar symptoms in 24 hours across Bangladesh”*  
  New Age Bangladesh  
  https://www.newagebd.net/post/country/298612/10-children-die-of-measles-similar-symptoms-in-24-hours-across-bangladesh

> ⚠️ **Note**  
> News-based data should be clearly marked as *provisional* and treated separately from officially reported WHO figures.

---

## 🧪 Analysis Workflow

1. Download and prepare measles case data from WHO
2. Append or annotate 2026 data using the referenced news source
3. Run the provided R script to process and analyze the dataset

```r
# Example
source("scripts/analyze_measles_data.R")
```


### How to cite
*Siddique, AB. (2026). measles_in_bd (1.0.0) [online_repo]. GitHub. [URL](https://github.com/abu85/measles_in_bd/edit/main/README.md)*



Licence: MIT / CC‑BY‑4.0 / CC0 / GPL


