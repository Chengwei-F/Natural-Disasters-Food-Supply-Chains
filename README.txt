Natural Disasters and U.S. Food Supply Chains

This study uses proprietary data. Access to proprietary datasets is subject to the respective data providers' licensing requirements.

- Compustat, accessed through Wharton Research Data Services (WRDS): supplier-customer sales relationships, firm financial information, and business and geographic segment sales.
- Tapestry: county-level food-industry establishment and employment data.
- DataAxle Business History: establishment and branch locations, parent-subsidiary relationships, employment, and sales.
- Spatial Hazard Events and Losses Database for the United States (SHELDUS): natural disaster frequency and property damage.
- National Oceanic and Atmospheric Administration (NOAA): Natural disaster data.
- U.S. Securities and Exchange Commission (SEC) filings: publicly available company Forms 10-K and 10-Q, used for historical headquarters locations and major-customer information.

Code

"data process.R" performs data cleaning, merges the datasets, constructs variables, produces figures and descriptive statistics, and exports the datasets used in the regression analysis.
"run.do" estimates the econometric models and produces the regression results and tables reported in the paper.
Run "data process.R" first, followed by "run.do", with file paths adjusted to the local data folder.

Table and Figure Correspondence

The table below identifies the software used to produce each table and figure.
The script names follow this package: "data process.R" for R and "run.do" for Stata.
R output filenames are relative to these folders under the local data root:
- Figures: replication_output/figures/
- Descriptive and reference tables: replication_output/tables/
- Datasets used by Stata: replication_output/data/

| Item | Software / script | Output file or display |
| --- | --- | --- |
| Table 1 | Stata / run.do | Stata Results: Panels A and B |
| Table 2 | Stata / run.do | Stata Results: regressions and marginal effects |
| Table 3 | Stata / run.do | Stata Results: regressions and marginal effects |
| Table 4 | Stata / run.do | Stata Results: regressions and marginal effects |
| Table A1 | R / data process.R | Table_A1.csv; Table_A1.html; Table_A1_notes.txt |
| Table A2 | R / data process.R | Table_A2.csv |
| Table A3 | R / data process.R | Table_A3_SIC_codes.csv |
| Table A4 | Stata / run.do | Stata Results |
| Table A5 | Stata / run.do | Stata Results |
| Table A6 | Stata / run.do | Stata Results |
| Table A7 | Stata / run.do | Stata Results |
| Table A8 | Stata / run.do | Stata Results |
| Figure 1 | R / data process.R | Figure_1a.pdf and Figure_1a.png; Figure_1b_large_caption_diagnostic.pdf and Figure_1b_large_caption_diagnostic.png |
| Figure 2 | R / data process.R | Figure_2.pdf; Figure_2.png |
| Figure 3 | R / data process.R | Figure_3.pdf; Figure_3.png |
| Figure 4 | Stata / run.do | Stata Graph: all-disaster and large-disaster coefficient plots |
| Figure 5 | Stata / run.do | Stata Graph: all-estimate and significant-estimate heatmaps |
| Figure A1 | R / data process.R | Figure_A1_customer_concentration.pdf; Figure_A1_customer_concentration.png |
| Figure A2 | R / data process.R | Figure_A2_segment_diversification.pdf; Figure_A2_segment_diversification.png |
| Figure A3 | R / data process.R | Figure_A3.pdf; Figure_A3.png |
| Figure A4 | R / data process.R | Figure_A4.pdf; Figure_A4.png |
| Figure A5 | Stata / run.do | Stata Graph: robustness heatmaps |
