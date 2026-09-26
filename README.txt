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
