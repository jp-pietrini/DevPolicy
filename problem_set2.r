#####                           Problem set 2                         ######

# Install packages tidyverse, haven, kableExtra, stargazer, ggrepel
install.packages(c("tidyverse", "haven", "kableExtra", "stargazer", "ggrepel"))



# Define working directory
setwd("/Users/jppie/Documents/Harvard/Courses/Dev 309/Data Sets/")


# Load packages
library(tidyverse)
library(haven)
library(kableExtra)
library(stargazer)
library(ggrepel)


##### Load and prepare the trade datasets ######

# Load the sitc_country_product_year_4.dta file countaining the country*product information
sitc_country_product_year_4 <- read_dta("sitc_country_product_year_4.dta")
View(sitc_country_product_year_4)

# Load the product_sitc.csv file countaining the product labels
product_sitc <- read_csv("product_sitc.csv")

# Keep only the product_id and the name_short_en columns
product_sitc <- product_sitc %>% select(product_id, name_short_en)

# Rename the name_short_en column to product_name
product_sitc <- product_sitc %>% rename(product_name = name_short_en)

# Load the location_country.csv file countaining the country labels
location_country <- read_csv("location_country.csv")

# Keep only the location_id the name_short_en and iso3_code
location_country <- location_country %>% select(country_id, iso3_code)


# Left join the sitc_country_product_year_4 dataset with the product_sitc dataset on product_id
sitc_country_product_year_4 <- sitc_country_product_year_4 %>% left_join(product_sitc, by = "product_id")

# Left join the sitc_country_product_year_4 dataset with the location_country dataset on country_id
sitc_country_product_year_4 <- sitc_country_product_year_4 %>% left_join(location_country, by = "country_id")

# Remove the product_sitc and location_country datasets
rm(product_sitc, location_country)

# Keep variable of interest: year, iso3_code, product_name, year, export_val, export_rca, global_market_share, pci, cog
sitc_country_product_year_4 <- sitc_country_product_year_4 %>% select(year, iso3_code, product_name, export_value, 
                                                                      export_rca, global_market_share, pci, cog)

# Rename sitc_country_product_year_4 to trade
trade <- sitc_country_product_year_4

# Remove the sitc_country_product_year_4 dataset
rm(sitc_country_product_year_4)


# Load the DEV309_Dataset.dta macro dataset
macro<- read_dta("DEV309_Dataset.dta")
# Get variable names
variable_names <- names(macro)
# Extract variable labels
label <- sapply(macro, function(x) attr(x, "label"))

# Create a dataframe with variable names and labels
labels <- data.frame(variable_name = variable_names,
                     label = label,
                     stringsAsFactors = FALSE)


# Select country of interest
country <- "MEX"


##### Question: 3 ######

# Filter the trade dataset to keep only the country of interest and years 2017 to 2022
trade_country <- trade %>% filter(iso3_code == country, year >= 2017, year <= 2022) %>%
  # Select year, product_name, export_value, export_rca, global_market_share, pci
  select(year, product_name, export_value, export_rca, global_market_share, pci) %>%
  # Group the data by product_name and calculate the averge of the variables
  group_by(product_name) %>%
  summarise(export_value = mean(export_value, na.rm = TRUE),
            export_rca = mean(export_rca, na.rm = TRUE),
            global_market_share = mean(global_market_share, na.rm = TRUE),
            pci = mean(pci, na.rm = TRUE))


# Top 10 Exports
top_exports <- trade_country %>% 
  # Sort by export_value in descending order
  arrange(desc(export_value)) %>%
  # Top 10 products by export value
  top_n(10, export_value) %>% 
  # Select product_name and export_value columns
  select(product_name, export_value) %>%
  # Divide the export value by 1,000,000 to display in millions
  mutate(export_value = export_value / 1000000)

# Producee using kableExtra.
top_exports %>%
  kbl(caption = "Top 10 Products by Exports in Million USD (Avg. 2022-2017)") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Top 10 RCA
top_rca <- trade_country %>% 
  # Sort by export_rca in descending order
  arrange(desc(export_rca)) %>%
  # Top 10 products by export value
  top_n(10, export_rca) %>% 
  # Select product_name and export_value columns
  select(product_name, export_rca) %>%
  # Round the export_rca to 2 decimal places
  mutate(export_rca = round(export_rca, 2))

# Producee using kableExtra.
top_rca %>%
  kbl(caption = "Top 10 Products by RCA (Avg. 2022-2017)") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# Top 10 Products by Global Market Share
top_market_share <- trade_country %>%
  arrange(desc(global_market_share)) %>%
  top_n(10, global_market_share) %>%
  select(product_name, global_market_share) %>%
  mutate(global_market_share = round(global_market_share, 4))

# Display the table using kableExtra
top_market_share %>%
  kbl(caption = "Top 10 Products by Global Market Share (Avg. 2017-2022)") %>%
  kable_classic(full_width = F, html_font = "Cambria")

# Top 10 Products by PCI
top_pci <- trade_country %>%
  arrange(desc(pci)) %>%
  top_n(10, pci) %>%
  select(product_name, pci) %>%
  mutate(pci = round(pci, 2))

# Display the table using kableExtra
top_pci %>%
  kbl(caption = "Top 10 Products by PCI (Avg. 2017-2022)") %>%
  kable_classic(full_width = F, html_font = "Cambria")


##### Question: 4 ######

# Use the Macro dataset for this question


# Select variables of interest in the macro dataset: year, countrycodeiso, atl_sitc_diversity, atl_sitc_avg_ubiquity, atl_sitc_eci

q4 <- macro %>% select(year, countrycodeiso, atl_sitc_diversity, atl_sitc_avg_ubiquity, atl_sitc_eci) %>%
  # Filter year=2021
  filter(year == 2021) # This dataset contains the necessary variables for this question


# Create a scatterplot between atl_sitc_avg_ubiquity and atl_sitc_diversity using ggplot. Label country of interest
q4 %>% ggplot(aes(x = atl_sitc_diversity, y = atl_sitc_avg_ubiquity)) +
  geom_point(aes(color = countrycodeiso == country), size = 2) +
  geom_text_repel(data = q4 %>% filter(countrycodeiso == country),
                  aes(label = countrycodeiso),
                  box.padding = 0.5,
                  color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  scale_color_manual(values = c("black", "blue"), guide = FALSE) +
  labs(title = "Average Ubiquity vs. Diversity in 2021",
       x = "Diversity",
       y = "Average Ubiquity") +
  theme_minimal()

# Create a scatterplot between atl_sitc_eci and atl_sitc_diversity using ggplot. Label country of interest
q4 %>% ggplot(aes(x = atl_sitc_diversity, y = atl_sitc_eci)) +
  geom_point(aes(color = countrycodeiso == country), size = 2) +
  geom_text_repel(data = q4 %>% filter(countrycodeiso == country),
                  aes(label = countrycodeiso),
                  box.padding = 0.5,
                  color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  scale_color_manual(values = c("black", "blue"), guide = FALSE) +
  labs(title = "ECI vs. Diversity in 2021",
       x = "Diversity",
       y = "Economic Complexity Index (ECI)") +
  theme_minimal()


##### Question: 5 ######

# Use the Macro dataset for this question

# Filter and select the variables of interest in the macro dataset: year, countrycodeiso, wdi_ny_gdp_pcap_pp_kd, wdi_ny_gdp_totl_rt_zs, atl_sitc_coi, atl_sitc_eci
q5 <- macro %>% select(year, countrycodeiso, wdi_ny_gdp_pcap_pp_kd, wdi_ny_gdp_totl_rt_zs, atl_sitc_coi, atl_sitc_eci) %>%
  # Filter year=2021 and rows with missing values
  filter(year == 2021,
         !is.na(wdi_ny_gdp_pcap_pp_kd),
         !is.na(wdi_ny_gdp_totl_rt_zs),
         !is.na(atl_sitc_coi),
         !is.na(atl_sitc_eci)
  ) 
# This dataset contains the necessary variables for this question


# First we run the regression to get the ECI residuals (x-axis): eci = β0+β1*GDP_per_capita+β2*Natural_Resources_Rents+ e
reg <- lm(atl_sitc_eci ~ wdi_ny_gdp_pcap_pp_kd + wdi_ny_gdp_totl_rt_zs, data = q5)
summary(reg)

q5 <- q5 %>% mutate(eci_resid = residuals(reg))

# Create a scatterplot between eci_resid and atl_sitc_coi using ggplot. Label country of interest
q5 %>% ggplot(aes(x = eci_resid, y = atl_sitc_coi)) +
  geom_point(aes(color = countrycodeiso == country), size = 2) +
  geom_text_repel(data = q5 %>% filter(countrycodeiso == country),
                  aes(label = countrycodeiso),
                  box.padding = 0.5,
                  color = "blue") +
  scale_color_manual(values = c("black", "blue"), guide = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "ECI Residuals vs. Complexity in 2021",
       x = "ECI Residuals (ECI - GDP per capita - Natural Resources Rents)",
       y = "Complexity Outlook Index") +
  theme_minimal()


##### Question: 6 ######

# Use the trade dataset and the macro dataset for this question

# Create a dataset contains the number of exported products with export_rca greater than one for each year and country
new_products_exported <- trade %>% 
  # Filter years of interest in the trade dataset (Years 2019, 2014)
  filter(year %in% c(2019, 2014)) %>%
  # Select year, iso3_code, product_name, export_rca
  select(year, iso3_code, product_name, export_rca) %>%
  # Arrange by country, product_name, year
  arrange(iso3_code, product_name, year) %>%
  # Create variable mcp to indicate if the product has export_rca greater than 1
  mutate(mcp = ifelse(export_rca > 1, 1, 0)) %>%
  # Group by iso3_code, product_name
  group_by(iso3_code, product_name) %>%
  # Create dummy new_product: These are products that were added to the export basket
  mutate(new_product = mcp - lag(mcp)) %>%
  # Filter new_product = 1
  filter(new_product == 1) %>%
  # Group by iso3_code
  group_by(iso3_code) %>%
  # Summ the number of new products
  summarise(new_products = sum(new_product))


# Get the necessary variables from the macro dataset: year, countrycodeiso, atl_sitc_coi
coi <- macro %>% select(year, countrycodeiso, atl_sitc_coi) %>%
  # Filter year=2014
  filter(year == 2014) %>%
  # Convert year to string and add the string 'coi_' at the beginning
  mutate(year = paste0("coi_", year)) %>%
  # Pivot the data to have the years as columns
  pivot_wider(names_from = year, values_from = atl_sitc_coi, values_fill = 0) 


# Merge the products_exported and coi datasets iso3_code=countrycodeiso
q6 <- new_products_exported %>% left_join(coi, by = c("iso3_code" = "countrycodeiso")) %>%
  # Drop missing values
  drop_na()


# This dataset contains the necessary variables for this question


# Run a regression model for New products on the Export Basket using base period coi 
reg_5y <- lm(new_products ~ coi_2014, data = q6)
stargazer(reg_5y, type = "text")


# Create scatterplot for change_5y and coi_2014
q6 %>% ggplot(aes(x = coi_2014, y = new_products)) +
  geom_point(aes(color = iso3_code == country), size = 2) +
  geom_text_repel(data = q6 %>% filter(iso3_code == country),
                  aes(label = iso3_code),
                  box.padding = 0.5,
                  color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_color_manual(values = c("black", "blue"), guide = FALSE) +
  labs(title = "Number of New Products (2019-2014) vs. COI in 2014",
       x = "Complexity Outlook Index in 2014",
       y = "Number of products added to the export basket") +
  theme_minimal()


# You could be even more strict and add fixed effects by income level or region, or even by country, to control for unobserved heterogeneity.
# Hoevever, this is a good start to understand the relationship between the complexity outlook index and the number of new products added to the export basket.
# And enough for this exercise
