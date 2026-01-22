#| label: metadata
#| purl: true
# Name: Landon Hunsaker
# Date: 2025-11-17
# Purpose: Portfolio Analysis #2
# -----------------------------------------------------------------------------


#| label: Packages Setup
#| purl: true

library(ggplot2)
library(tidyverse)

files <- list.files("data", pattern = "*.csv", full.names = TRUE)
gas <- files |> lapply(read_csv) |> bind_rows()



#| label: Tidying
#| purl: true

gasc <- gas |> filter(!if_all(everything(), is.na))

gasg <- gasc |>
  mutate(
    gallons = case_when(
      !is.na(Units) & Units == "Liters"  ~ `Amount of fuel` / 3.78541, ### conversion to gal.
      !is.na(Units) & Units == "Gallons" ~ `Amount of fuel`,
      is.na(Units) & !is.na(`Amount of fuel`) ~ `Amount of fuel`,
      TRUE ~ gallons
    ),
    USD = case_when(
      Currency == "USD" ~ Cost,
      Currency == "CAD" ~ Cost *.72, ### Exchange rate as of 12/10/2025
      TRUE ~ USD
  )
  
)

### Moving Station 'notes' to 'Station' to prep 'notes' for deletion.
gasg <- gasg |> mutate(
  Station = case_when( ### String search to identify the common gas stations in notes
    !is.na(Station) ~ Station,
    grepl("costco|sams|swift|holiday|philip|owatonna|BP|conoco|casey|cenex|valero|Exxon|Kum|Quick|Kwik|shell|M&H|loves|arco|Ace|Reata|Flying|Esso", notes,ignore.case=TRUE)~notes,
    TRUE~Station
  )
)

### Moving notes to Notes to prep for deletion
gas_notes_clean <- gasg |> mutate(
  Notes = case_when( 
    !is.na(Notes) ~ Notes,
    !grepl("costco|sams|swift|holiday|philip|owatonna|BP|conoco|casey|cenex|valero|Exxon|Kum|Quick|Kwik|shell|M&H|loves|arco|Ace|Reata|Flying|Esso", notes,ignore.case=TRUE) & !is.na(notes) ~notes,
    TRUE~Notes
  )
)

### Main coalescing and deletion.

intermed <- gas_notes_clean |> mutate(
   USD = case_when(
     !is.na(CAD) ~ CAD*.72, ### Converting the first half of the csv's CAD
     TRUE~USD
   ),
   gallons = coalesce(gallons, liters / 3.78541),
   ethanol = coalesce(ethanol,`Ethanol %`),
   miles = coalesce(miles,Mileage),
   octane = coalesce(octane,Octane),
   Timestamp = as.Date(Timestamp),
   date = coalesce(date, Timestamp), ### Would've been easier to just select the rows I wanted, but too late.
   ) |> select(-liters, -Octane, -`Ethanol %`, -Mileage, -notes, -`Amount of fuel`,  -Units, -Timestamp, -Cost, -Currency, -CAD)



#| label: More tidying and doing MPG
#| purl: true


int.final <- intermed |> mutate(
  gallons_correct = lag(gallons),
  ethanol_raw= lag(ethanol)
)

final <- int.final |> filter(    
  gallons_correct > 15   ### The tank cannot be mostly full of separate fuel. This is 75%.
) |> mutate(
  ethanol_correct = case_when( ### Combining upper ethanol values for testing + drop nas
    ethanol_raw == 0  ~ "E0",
    ethanol_raw == 10 ~ "E10",
    TRUE ~ NA_character_
  )) |> filter(!is.na(ethanol_correct)) |> select(-ethanol,-ethanol_raw)

mpg <- final |> mutate(
  mpg = case_when(
    !is.na(gallons_correct) & !is.na(miles) ~ miles / gallons_correct, ### removing na's
    TRUE ~ NA_real_
  )
)

table(int.final$ethanol_raw, useNA = "ifany")


#| label: Checking normality
#| purl: true
#| fig-width: 3
#| fig-height: 2
#| layout-ncol: 2

ggplot(data=mpg)+ ### Histogram
  geom_histogram(
    aes(x=mpg),
    color="black",
    fill="gold",
  )+
  labs(
    title= "Frequency of MPG values",
    x= "MPG measured",
    y= "Occurences"
  )+
  theme_bw()

ggplot(data=mpg)+ ### Boxplot
  geom_boxplot(
    mapping=aes(x=ethanol_correct, y= mpg),
    fill="white",
    color="black"
  )+
  labs(
    title= "variances of gas",
    y="Miles per gallon",
    x= "Type of fuel"
  )


#| label: T-test
#| purl: true
### By default, two sample welches t-test.
t.test(mpg ~ethanol_correct, data=mpg,var.equal=TRUE)

