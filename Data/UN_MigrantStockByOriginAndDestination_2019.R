library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(forcats)

CaribbeanGroup <- c("Anguilla", "Antigua and Barbuda", "Bahamas", "Barbados", 
  "British Virgin Islands", "Cayman Islands*", "Cuba", "Dominica", "Dominican Republic",
  "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", "Montserrat*", "Puerto Rico",
  "Saint Barthélemy*", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin (French part)*",
  "Saint Vincent and the Grenadines", "Trinidad and Tobago", "Turks and Caicos Islands*",
  "United States Virgin Islands*")

ASIA <- c("Kazakhstan", "Kyrgyzstan", "Tajikistan", "Turkmenistan", "Uzbekistan", 
  "Eastern Asia", "China", "China, Hong Kong SAR", "China, Macao SAR*",
  "China, Taiwan Province of China*", "Dem. People's Republic of Korea",
  "Japan", "Mongolia", "Republic of Korea", "Brunei Darussalam", "Cambodia",
  "Indonesia", "Lao People's Democratic Republic", "Malaysia*", "Myanmar",
  "Philippines", "Singapore", "Thailand", "Timor-Leste", "Viet Nam", "Afghanistan",
  "Bangladesh", "Bhutan", "India", "Iran (Islamic Republic of)", "Maldives",
  "Nepal", "Pakistan", "Sri Lanka", "Armenia", "Azerbaijan*", "Bahrain",
  "Cyprus*", "Georgia*", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Oman",
  "Qatar", "Saudi Arabia", "State of Palestine*", "Syrian Arab Republic",
  "Turkey", "United Arab Emirates", "Yemen")

EUROPE <- c("Belarus", "Bulgaria", "Czechia", "Hungary", "Poland", "Republic of Moldova*",
  "Romania", "Russian Federation", "Slovakia", "Ukraine*", "Channel Islands*",
  "Denmark*", "Estonia", "Faroe Islands*", "Finland*", "Iceland", "Ireland",
  "Isle of Man*", "Latvia", "Lithuania", "Norway", "Sweden", "United Kingdom",
  "Albania", "Andorra", "Bosnia and Herzegovina", "Croatia", "Gibraltar*", "Greece",
  "Holy See*", "Italy", "Malta", "Montenegro", "North Macedonia", "Portugal",
  "San Marino", "Serbia*", "Slovenia", "Spain", "Austria", "Belgium", "France",
  "Germany", "Liechtenstein", "Luxembourg", "Monaco", "Netherlands", "Switzerland")

LATINAMERICA <- c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico",
  "Nicaragua", "Panama", "Argentina", "Bolivia (Plurinational State of)",
  "Brazil", "Chile", "Colombia", "Ecuador", "Falkland Islands (Malvinas)*",
  "French Guiana*", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay",
  "Venezuela (Bolivarian Republic of)")

NORTHERNAMERICA <- c("Bermuda*", "Canada", "Greenland*", "Saint Pierre and Miquelon*", "United States of America")

OCEANIA <- c("Australia*", "New Zealand", "Fiji", "New Caledonia*", "Papua New Guinea", "Solomon Islands",
  "Vanuatu", "Guam*", "Kiribati", "Marshall Islands", "Micronesia (Fed. States of)", "Nauru",
  "Northern Mariana Islands*", "Palau", "American Samoa*", "Cook Islands*", "French Polynesia*",
  "Niue*", "Samoa", "Tokelau*", "Tonga", "Tuvalu", "Wallis and Futuna Islands*")

dir()
excel_sheets("UN_MigrantStockByOriginAndDestination_2019.xlsx")

UNdata <- read_excel("UN_MigrantStockByOriginAndDestination_2019.xlsx", skip = 15, sheet = 2, col_names = TRUE)

UNdataTemp1 <- UNdata %>% 
  select (-X__2, -X__4, -X__5, -X__6, -Total, -`Other South`, -`Other North`) %>%
  rename( Year = X__1, Destination = X__3) %>%
  filter( Year == 2019, Destination == "Bonaire, Sint Eustatius and Saba") %>%
  mutate_at(vars(-Year,-Destination), ~as.numeric(., na.rm = TRUE)) %>%
  replace(is.na(.), 0) %>%
  gather(Origin, Stock, -Year, -Destination) %>%
  filter(Stock!=0) %>%
  mutate(Perc = Stock/sum(Stock, na.rm = TRUE)) %>%
  mutate(LargeGroups = Origin) %>%
  #mutate(LargeGroups = replace(LargeGroups, Origin %in% AFRICA, "AFRICA")) %>%
  mutate(LargeGroups = replace(LargeGroups, Origin %in% ASIA, "ASIA")) %>%
  mutate(LargeGroups = replace(LargeGroups, Origin %in% EUROPE, "EUROPE")) %>%
  mutate(LargeGroups = replace(LargeGroups, Origin %in% CaribbeanGroup, "Caribbean")) %>%
  mutate(LargeGroups = replace(LargeGroups, Origin %in% LATINAMERICA, "LATIN AMERICA")) %>%
  mutate(LargeGroups = replace(LargeGroups, Origin %in% NORTHERNAMERICA, "NORTHERN AMERICA")) %>%
  mutate(LargeGroups = replace(LargeGroups, Origin %in% OCEANIA, "OCEANIA")) %>%
  arrange(LargeGroups,desc(Perc)) %>%
  select(Destination, Origin, Year, Stock, Perc, LargeGroups) %>%
  #arrange(Perc) %>%
  print() #%>%
  #summarize(TotalStock = sum(Stock, na.rm = TRUE), TotalPerc = sum(Perc, na.rm = TRUE)) %>%
  #print() #%>%

UNdataTemp1 %>% 
  ggplot(aes(x = reorder(Origin, -Perc), y = Perc)) +
  facet_grid(facets = Destination ~ Year) +
  geom_bar(stat = 'identity') +
  labs(x = "Origin country", y = "Percentage of migrants in destination country", 
    caption = "International migrant stock at mid-year by area of destination and origin, 1990-2020") +
  theme(axis.text.x = element_text(angle = 90), text = element_text(size=12)) 

write.csv(UNdataTemp1, file = "../2020/Mathematica/Data/DestinationBESCountries.csv", row.names = TRUE)

UNdataTemp2 <- UNdataTemp1 %>%
  group_by(LargeGroups) %>%
  summarise( Stock = sum(Stock, na.rm = TRUE), Perc = sum(Perc, na.rm = TRUE) ) %>%
  mutate(Destination = "BES", Year = 2019) %>%
  rename(Origin = LargeGroups) %>%
  select(Destination, Origin, Year, Stock, Perc) %>%
  print()

write.csv(UNdataTemp2, file = "../2020/Mathematica/Data/DestinationBESContinents.csv", row.names = TRUE)

