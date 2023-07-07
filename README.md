# OpioidsYLL
Calculating the Years of Life Lost to opioid-related deaths in the US.

Work done at Johns Hopkins University, by <a href="https://alsnhll.github.io">Alison L Hill</a> and <a href="https://annehebert.github.io">Anne H. Hébert</a>

## Summary
The opioid crisis is a public health emergency in the US, with opioid overdose deaths increasing each year. Here we use the metric of years of life lost to characterize the burden of the opioid epidemic across demographic groups. We estimate that nearly 3 million years of life were lost to the opioid crisis in 2021 alone, and that the US population's life expectancy at birth was reduced by 0.65 years. We found that the crisis worsened in almost all 50 states during this time, and that the demographics of those most impacted by the opioid epidemic are shifting. People of color are now experiencing the high years of life lost per capita and the largest decrease in life expectancy. 

## Repo contents
This repository contains R code calculating the years of life lost to the opioid-related deaths in the US, as well as the resulting reduction in life expectancy.  

#### Files
<ul>
  <li>functions.R : this R script contains functions used in YearsLifeLost.R </li>
  <li>YearsLifeLost.R : this R script contains code calculating the years of life lost by demographic group </li>
  <li>YLL_state.R : this R script contains code calculating the years of life lost by state </li>
  <li>data_files_into.txt : this text file contains instructions for downloading each of the data files used in the above R scripts</li>
</ul>

#### Data
All data used in this work is publicly available. The data tracking yearly death counts is from the CDC WONDER <a href="https://wonder.cdc.gov/mcd-icd10-expanded.html">Multiple Cause of Death database</a>. The lifetables are from the <a href="https://www.cdc.gov/nchs/nvss/life-expectancy.htm">National Center for Health Statistics</a>.

Info on the specific data files used, as well as download instructions, can be found in the data_files_info.txt.

## Related
These results can be found on <a href="https://annehebert.github.io/dashboard.html">our interactive web app</a>, which visualizes metrics of the opioid crisis.
