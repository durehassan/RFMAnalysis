library("tidyr")
library("tidyverse")
library("Hmisc")
df_sales <- read.csv("C:/Users/Dell/Desktop/IEC/storeData.csv") #absolute path
df_sales
date_today <- lubridate::ymd("20111230")
df_sales|> 
  dplyr::mutate(
    subtotal = Quantity * UnitPrice,
    getDate = strptime(InvoiceDate , format = "%m/%d/%Y %H:%M"),
    date_today
  ) |> 
  dplyr::group_by(CustomerID) |> 
  dplyr::summarise(
    no_of_uniqueTransactions = dplyr::n_distinct(InvoiceNo) ,
    tot_revenue = sum(subtotal),
    lastdate_of_purchase = max(getDate)
  ) |>
  dplyr::mutate(day_of_lastpurchase = difftime(date_today,
                                               lastdate_of_purchase, units = ("days"))) |> 
  dplyr::mutate(days_since_lastpurchase = as.integer(day_of_lastpurchase)) |> 
  drop_na()|> 
  janitor::clean_names() |> 
  filter(tot_revenue > 0)-> dfm_rfm
  dfm_rfm |>
    mutate(monetary_quintile = cut2(tot_revenue, g =5),
           frequency_quintile = cut2(no_of_unique_transactions, g = 5),
           recency_quintile = cut2(days_since_lastpurchase, g = 5)) -> rfm_2
  View(rfm_2)
  levels(rfm_2$recency_quintile)  
  rfm_2 |> 
    mutate(recency_score = as.integer(recency_quintile),
           frequency_score = as.integer(frequency_quintile),
           monetary_score = as.integer(monetary_quintile)) |> 
    mutate(recency_score = dense_rank(desc(recency_score))) -> rfm_3  
  
  rfm_3 |>
    dplyr::mutate(RFM = recency_score * 100 + frequency_score * 10 + monetary_score) |>
    dplyr::mutate(labels = ifelse(recency_score >= 4 & frequency_score >= 3 & monetary_score >= 4, "Champions",
                                  ifelse((recency_score >= 4) & (frequency_score <= 2) & (monetary_score >= 4), "High Spending New Customers",
                                         ifelse((recency_score >= 4) & (frequency_score >= 4) & (monetary_score == 3), "Average Spending Champions",
                                                ifelse((recency_score >= 2 & recency_score <= 4) & (frequency_score >= 3 & frequency_score <= 5) & (monetary_score >= 4), "Loyal Customers", 
                                                       ifelse((recency_score >= 3) & (frequency_score >= 1 & frequency_score <= 3) & (monetary_score >= 1 & monetary_score <= 3), "Potential Loyalists",
                                                              ifelse((recency_score >= 4 & recency_score <= 5) & (frequency_score < 2) & (monetary_score < 2), "New Customers",
                                                                     ifelse((recency_score >= 3 & recency_score <= 4) & (frequency_score < 2) & (monetary_score < 2), "Promising",
                                                                            ifelse((recency_score >= 3 & recency_score <= 4) & (frequency_score >= 2 & frequency_score <= 4) & (monetary_score >= 3 & monetary_score <= 5), "Need attention",
                                                                                   ifelse((recency_score >= 2 & recency_score <= 3) & (frequency_score < 3) & (monetary_score < 3), "About to sleep",
                                                                                          ifelse((recency_score < 3) & (frequency_score >=2 & frequency_score <= 5) & (monetary_score >= 2 & monetary_score <= 5), "At risk",
                                                                                                 ifelse((recency_score < 2) & (frequency_score >= 4 & frequency_score <= 5) & (monetary_score >= 4 & monetary_score <= 5), "Can't loose them",
                                                                                                        ifelse((recency_score >= 2 & recency_score <=3) & (frequency_score >= 2 & frequency_score <= 3) & (monetary_score >= 2 & monetary_score <= 3), "Hibernating",
                                                                                                               ifelse((recency_score <= 2) & (frequency_score <= 2) & (monetary_score >= 4), "High Value Lost",
                                                                                                                      ifelse((recency_score < 2) & (frequency_score <= 3) & (monetary_score <= 2), "Low Value Lost",
                                                                                                                             ifelse((recency_score == 3) & (frequency_score < 2) & (monetary_score >= 4), "High Spending New Customers",
                                                                                                                                    ifelse((recency_score <= 2) & (frequency_score < 2) & (monetary_score == 3), "Average Spending Lost",
                                                                                                                                           ifelse((recency_score <= 2) &(frequency_score <= 4) &(monetary_score == 1), "Low Value Hibernating",
                                                                                                                                                  ifelse((recency_score <= 3) &(frequency_score >= 4) &(monetary_score <=3), "Average Spending Need Attention", "Low Spending Champions"))))))))))))))))))) -> rfm_result
View(rfm_result)  

  
  