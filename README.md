# BD_final_assignment

Final group project for Big Data Class

Summary of Variables Used in the Analysis

Explanatory Variable        Description

1 asset_growth              Asset growth (%) between Year T-2 and Year T-1

2 asset_turnover            Binary variable = 1 if sales growth (%) > asset growth (%) over the past year, otherwise 0

3 assets_total              Total assets

4 bv_per_share              Book value per share

5 debt_assets               Long term debt / Total assets

6 debtpy_debt               Binary variable = 1 if long term debt reported in Year T-1 < long term debt reported in Year T-2, otherwise 0

7 ebitda                    Earnings before interest, taxes and depreciation (EBITDA)

8 ebitda_ev                 EBITDA / Enterprise value

9 ebitda_margin             EBITDA / Revenue

10 enterprise_value         Enterprise value (defined as market capitalization + long term debt)

11 gprofit_assets           Gross profit / Total assets

12 lt_debt_ev               Long term debt / Enterprise value

13 lt_debt_total            Long term debt

14 marketcap_t              Market capitalization at the end of March in Year T0 15 pb_index Deciles of [Price/Book] for each year

16 price_book               Price/Book

17 prior_1yr_return         Return from the end of March in Year T-1 to the end of March in Year T0 (including dividends)

18 profit_index             Deciles of [Gross profit / Total assets] for each year

19 pyreturn_below_median    Binary variable = 1 if the return of a stock in the past year was below the median of all stocks, otherwise 0

20 pyreturn_index           Deciles of prior one-year return for each year

21 revenue                  Revenue (Sales)

22 sales_growth             Sales growth (%) between Year T-2 and Year T-1

23 sharespy_shares          Binary variable = 1 if there are fewer shares outstanding in Year T-1 vs Year T-2, otherwise 0

24 size_index               Deciles of market capitalization for each year

25 value_index              Deciles of [EBITDA / Enterprise value] for each year


Outcome Variable:           FDP Binary variable = 1 for companies that pay down debt over the next year, otherwise 
