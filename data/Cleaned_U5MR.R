
# Code for slide 1
# Load necessary libraries
library(tidyverse)
library(viridis)
library(maps)
library(plotly)
library(dplyr)
library(ggplot2)

# Load the dataset
u5mr = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/final_u5mr.csv")

# Clean column names by removing the "X" prefix from year columns
names(u5mr) = gsub("^X", "", names(u5mr))

u5mr <- u5mr %>%
  mutate(Country.Name = case_when(
    Country.Name == "Antigua and Barbuda" ~ "Antigua",
    Country.Name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    Country.Name == "Brunei Darussalam" ~ "Brunei",
    Country.Name == "Côte d'Ivoire" ~ "Ivory Coast",
    Country.Name == "Congo" ~ "Republic of Congo",
    Country.Name == "Cabo Verde" ~ "Cape Verde",
    Country.Name == "Czechia" ~ "Czech Republic",
    Country.Name == "Micronesia (Federated States of)" ~ "Micronesia",
    Country.Name == "United Kingdom" ~ "UK",
    Country.Name == "Iran (Islamic Republic of)" ~ "Iran",
    Country.Name == "Saint Kitts and Nevis" ~ "Saint Kitts",
    Country.Name == "Republic of Korea" ~ "South Korea",
    Country.Name == "Lao People's Democratic Republic" ~ "Laos",
    Country.Name == "Republic of Moldova" ~ "Moldova",
    Country.Name == "Netherlands (Kingdom of the)" ~ "Netherlands",
    Country.Name == "Democratic People's Republic of Korea" ~ "North Korea",
    Country.Name == "State of Palestine" ~ "Palestine",
    Country.Name == "Kosovo (UNSCR 1244)" ~ "Kosovo",
    Country.Name == "Russian Federation" ~ "Russia",
    Country.Name == "Eswatini" ~ "Swaziland",
    Country.Name == "Syrian Arab Republic" ~ "Syria",
    Country.Name == "Trinidad and Tobago" ~ "Trinidad",
    Country.Name == "Tuvalu" ~ "Tuvalu",
    Country.Name == "Türkiye" ~ "Turkey",
    Country.Name == "United Republic of Tanzania" ~ "Tanzania",
    Country.Name == "United States" ~ "USA",
    Country.Name == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
    Country.Name == "British Virgin Islands" ~ "Virgin Islands",
    Country.Name == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    Country.Name == "Viet Nam" ~ "Vietnam",
    TRUE ~ Country.Name
  ))
  
# Get map data
world_map = map_data("world")

title_slide_plot = ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightsteelblue", color = "white") +
  coord_fixed(1.3) +
  annotate("text", x = 0, y = 60, label = "Course: MATH2459", size = 6, hjust = 0.5) +
  annotate("text", x = 0, y = 50, label = "Assignment 3 - Storytelling with Open Data", size = 6, hjust = 0.5) +
  annotate("text", x = 0, y = 40, label = "Tutor: Arthur Tang", size = 6, hjust = 0.5) +
  annotate("text", x = 0, y = 30, label = "Name: Thanh Dung Huynh", size = 6, hjust = 0.5) +
  annotate("text", x = 0, y = 20, label = "IDs: 3938224", size = 6, hjust = 0.5) +
  theme_void() +
  ylim(-90, 90) # Set y-axis limits to encompass the map

title_slide_plot

slide1_plot = ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightsteelblue", color = "white") +
  coord_fixed(1.3) +
  theme_void() +
  labs(
    title = "The Geography of Survival",
    subtitle = "Where a child is born can determine whether they survive",
    caption = "Source: UNICEF, World Bank"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )

slide1_plot

# Code for slide 2 

# Prepare under-five mortality data for 2023
u5mr_2023 = u5mr %>%
  filter(Uncertainty.Bounds. == "Median") %>%
  select(ISO.Code, Country.Name, "2023", Uncertainty.Bounds.) %>%
  rename(MortalityRate = "2023") %>%
  mutate(MortalityRate = as.numeric(MortalityRate))

# Merge mortality data with map
map_data_2023 = world_map %>%
  left_join(u5mr_2023, by = c("region" = "Country.Name"))

slide2_plot = ggplot(map_data_2023, aes(x = long, y = lat, group = group, fill = MortalityRate)) +
  geom_polygon(color = "gray80", size = 0.2) +
  coord_fixed(1.3) +
  scale_fill_viridis(option = "plasma", na.value = "lightgray", name = "Deaths per 1,000") +
  theme_void() +
  labs(title = "Under-Five Mortality Rate in 2023")

slide2_plot

# Code for slide 3 
# Prepare the data for plotting (Long Format)

live_birth_data = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/livebirth.csv")
live_birth_data = live_birth_data %>%
  select(-X)

names(live_birth_data) = gsub("^X", "", names(live_birth_data))

live_birth_long = live_birth_data %>%
  pivot_longer(
    cols = `1960`:`2023`,
    names_to = "Year",
    values_to = "Live_Births"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2000 & Year <= 2023)

u5mr_long = u5mr %>%
  pivot_longer(cols = "2000":"2023", names_to = "Year", values_to = "U5MR") %>%
  mutate(Year = as.numeric(Year))

liveBirth_u5mr_data = u5mr_long %>%
  inner_join(live_birth_long, by = c("Country.Name", "Year")) %>%
  mutate(Estimated_Under_Five_Deaths = (U5MR / 1000) * Live_Births)

# Summing U5MR for each region and year
region_sum = liveBirth_u5mr_data %>%
  group_by(SDG.Region, Year) %>%
  filter(Year == c("2000", "2005", "2010", "2015", "2020")) %>%
  summarize(Total_Deaths = sum(Estimated_Under_Five_Deaths, na.rm = TRUE),
            Total_Live_Births = sum(Live_Births, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(
    Regional_U5MR = (Total_Deaths / Total_Live_Births) * 1000
  ) %>%
  filter(Year %in% c(2000, 2005, 2010, 2015, 2020, 2023))
  
# Create the plot with all countries and regions
slide3_plot = plot_ly(
  data = region_sum,
  x = ~Year,
  y = ~Regional_U5MR,
  type = 'scatter',
  mode = 'lines+markers',
  color = ~SDG.Region,
  line = list(width = 1),
  marker = list(size = 4),
  showlegend = TRUE, 
  height = 600,
  width = 1100
)

slide3_plot

#Code for slide 4
gdp_data = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/GDP.csv", header = TRUE, fill = TRUE, check.names = TRUE)

names(gdp_data) = gsub("^X", "", names(gdp_data))

gdp_data = gdp_data[, colnames(gdp_data) != ""]

gdp_long = gdp_data %>%
  pivot_longer(
    cols = `1960`:`2024`,            
    names_to = "Year",
    values_to = "GDP_Per_Capita"
  ) %>%
  mutate(Year = as.numeric(Year))

gdp_u5mr_data = u5mr_long %>%
  inner_join(gdp_long, by = c("Country.Name", "Year"))

# Basic scatter plot colored by region
region_sum_gdp = gdp_u5mr_data %>%
  group_by(SDG.Region) %>%
  summarize(
    total_gdp = sum(GDP_Per_Capita, na.rm = TRUE), 
    avg_u5mr = mean(U5MR, na.rm = TRUE),
    n_countries = n(),
    .groups = 'drop'
  ) %>%
  rename(Region = SDG.Region)


slide4_plot = plot_ly(region_sum_gdp) %>%
  add_markers(
    x = ~log10(total_gdp),,
    y = ~avg_u5mr,
    text = ~paste0("Region: ", Region, 
                   "<br>Total GDP: ", round(total_gdp, 2),
                   "<br>Average Under-Five Mortality Rate: ", round(avg_u5mr, 2)),
    color = ~Region,       
    marker = list(size = 10),
    hoverinfo = "text"
  ) %>%
  layout(
    title = "GDP per Capita vs Under-Five Mortality Rate",
    xaxis = list(title = "(Log) GDP per Capita (USD)"),
    yaxis = list(title = "Under-Five Mortality Rate (per 1,000 live births)"),
    hovermode = "closest"
  )

slide4_plot 

# Code for slide 5
education_data = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/mean-years-of-schooling-female .csv")

education_u5mr_data = u5mr_long %>% 
  filter(Uncertainty.Bounds. == "Median") %>%
  inner_join(education_data, by = c("Country.Name", "Year"))

education_u5mr_data = education_u5mr_data %>%
  rename(
    Maternal_Education = Mean.years.of.schooling..ISCED.1.or.higher...population.25..years..female
  )

# Fit the linear model for regression
lm_model = lm(U5MR ~ Maternal_Education, data = education_u5mr_data)
predicted_values = predict(lm_model, education_u5mr_data)

slide5_plot = plot_ly(education_u5mr_data) %>%
  add_markers(
    x = ~Maternal_Education,
    y = ~U5MR,
    text = ~paste0("Country: ", Country.Name, 
                   "<br>Year: ", Year,
                   "<br>Maternal Education: ", round(Maternal_Education, 2),
                   "<br>Average Under-Five Mortality Rate: ", round(U5MR, 2)),
    color = ~SDG.Region,       
    marker = list(size = 10),
    hoverinfo = "text"
  ) %>%
  add_lines(
    x = ~Maternal_Education,
    y = predicted_values,
    line = list(color = "red", width = 2),
    name = "Regression Line"
  ) %>%
  layout(
    title = "Maternal Education vs Under-Five Mortality Rate",
    xaxis = list(title = "Average Years of Maternal Education (Age 25+)"),
    yaxis = list(title = "Under-Five Mortality Rate (per 1,000 live births)"),
    hovermode = "closest",
    showlegend = TRUE
  )

slide5_plot

# Code for slide 6 

# Get the sanitizer data
sanitizer_data = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/Sanitizer.csv")
names(sanitizer_data) = gsub("^X", "", names(sanitizer_data))

sanitizer_2022 = sanitizer_data %>%
  select(Country.Name, Country.Code, "2022") %>%
  rename(
    Country = Country.Name,
    codeCountry = Country.Code,
    Sanitizer = "2022"
  ) %>%
  filter(!is.na(Sanitizer))

# Get the water data
water_data = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/Water.csv")

water_2022 = water_data %>%
  filter(Period == 2022) %>%
  select(Location, SpatialDimValueCode, FactValueNumeric) %>%
  rename(
    Country = Location,
    codeCountry = SpatialDimValueCode,
    Water = FactValueNumeric
  ) %>%
  filter(!is.na(Water))

#Merge data
sanitizer_water_data = full_join(sanitizer_2022, water_2022, by = c("Country", "codeCountry"))

# Plot sanitizer map
sanitizer_map = plot_geo(sanitizer_water_data) %>%
  add_trace(
    z = ~Sanitizer, locations = ~codeCountry, color = ~Sanitizer, 
    colors = "Greens", 
    hoverinfo = "text",
    text = ~paste0(Country, 
                   "<br>Sanitizer Value: ", round(Sanitizer, 2), "%"),
    marker = list(line = list(width = 0.5)),
    colorbar = list(x = -0.1,
                    y = 0.65,
                    xanchor = "left", 
                    title = "Sanitizer (%)") 
  ) %>%
  layout(title = "Access to Basic Sanitation (2022)", 
         geo = list(showframe = FALSE)
  )

# Plot water map
water_map = plot_geo(sanitizer_water_data) %>%
  add_trace(
    z = ~Water, locations = ~codeCountry, color = ~Water,
    colors = "Blues",
    hoverinfo = "text",
    text = ~paste0(Country,
                   "<br>Water Value: ", round(Water, 2), "%"), 
    marker = list(line = list(width = 0.5)),
    colorbar = list(x = 1.1,
                    xanchor = "right",
                    title = "Water (%)")
  ) %>%
  layout(title = "Access to Basic Water (2022)", 
         geo = list(showframe = FALSE))

slide6_plot = subplot(sanitizer_map, water_map, nrows = 1, margin = 0.05)

slide6_plot

# Code for slide 7
doctor_data = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/doctor_data.csv")

doctor_avg_data = doctor_data %>%
  group_by(codeCountry) %>%
  mutate(
    DoctorPercentage = if_else(
      is.na(DoctorPercentage),
      mean(DoctorPercentage, na.rm = TRUE),
      DoctorPercentage
    )
  ) %>%
  ungroup()

doctor_u5mr_data <- u5mr_long %>%
  inner_join(doctor_avg_data, by = c("Country.Name", "Year")) %>%
  filter(Uncertainty.Bounds. == "Median",
         Year == "2022") %>%
  mutate(
    text = paste0("Country: ", Country.Name, "<br>",
                  "Avg Doctor: ", round(DoctorPercentage, 2), "<br>",
                  "U5MR: ", round(U5MR, 2))
  )

slide7_plot = ggplot(doctor_u5mr_data, aes(x = DoctorPercentage, y = U5MR,
                                           text = text,
                                           color = SDG.Region)) +
  geom_point(alpha = 0.7, size = 2) +          
  geom_smooth(aes(group = SDG.Region), method = "lm", se = FALSE, color = "black", linewidth = 0.3) +
  facet_wrap(~SDG.Region, scales = "free") +
  scale_color_brewer(palette = "Set1") +       # nice color palette
  labs(
    title = "Healthcare Access vs Under-5 Mortality Rate by Region (2022)",
    subtitle = "Faceted scatter plot showing trend over regions",
    x = "Percentage of Births Attended by Skilled Personnel",
    y = "Under-5 Mortality Rate (per 1,000 live births)",
    color = "SDG Region"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

slide7_plot = ggplotly(slide7_plot, tooltip = "text")

slide7_plot


# Code for slide 7

# Get doctor data
doctor_data = read.csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/doctors.csv")
doctor_data = doctor_data %>%
  rename(
    codeCountry = SpatialDimValueCode,
    doctorCount = FactValueNumeric,
    Year = Period
  ) %>%
  mutate(Profession = "Doctor")

# Get nurse data
nurse_data = read_csv("/Users/dilyshuynh/Library/Group Containers/group.com.readdle.documents/MusicWidget/Documents/RMIT/MATH2459/Assignment 3/nurse.csv")
nurse_data = nurse_data %>%
  rename(
    codeCountry = SpatialDimValueCode,
    nurseCount = FactValueNumeric,
    Year = Period
  ) %>%
  mutate(Profession = "Nurse")

# Merge doctor and nurse data
doctor_nurse_data = bind_rows(doctor_data, nurse_data) %>%
  select(codeCountry, Location, Year, Value, Profession) %>%
  pivot_wider(
    names_from = Profession,
    values_from = Value
  ) %>%
  rename(
    Country.Name = Location,
    doctorCount = Doctor,
    nurseCount = Nurse
  )

# Calculate average for each country and replace NA with average if needed
doctor_nurse_avg = doctor_nurse_data %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  group_by(codeCountry) %>%
  mutate(
    doctorCount = ifelse(is.na(doctorCount), mean(doctorCount, na.rm = TRUE), doctorCount),
    nurseCount = ifelse(is.na(nurseCount), mean(nurseCount, na.rm = TRUE), nurseCount)
  ) %>%
  ungroup() %>%
  mutate(
    doctorCount = ifelse(is.na(doctorCount), mean(doctorCount, na.rm = TRUE), doctorCount),
    nurseCount = ifelse(is.na(nurseCount), mean(nurseCount, na.rm = TRUE), nurseCount)
  )

# Join data
health_u5mr_data = u5mr_long %>% 
  filter(Uncertainty.Bounds. == "Median") %>%
  inner_join(doctor_nurse_avg, by = c("Country.Name", "Year"))

# Plot the facet scatter plot for health factor
doctor_nurse_long = health_u5mr_data %>%
  select(Country.Name, SDG.Region, Year, doctorCount, nurseCount, U5MR) %>%
  pivot_longer(cols = c(doctorCount, nurseCount),
               names_to = "Profession",
               values_to = "CountPer10000"
               ) %>%
  mutate(
    Profession = recode(Profession,
                             doctorCount = "Doctors",
                             nurseCount = "Nurses"),
    text = paste0(
      "Country: ", Country.Name,
      "<br>Profession: ", Profession,
      "<br>Count per 10,000: ", round(CountPer10000, 2),
      "<br>U5MR: ", round(U5MR, 2)
    )
  )

slide7_plot = ggplot(doctor_nurse_long, 
                     aes(x = CountPer10000, y = U5MR, color = Profession, text = text, group = Profession)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ SDG.Region) +
  labs(
    title = "U5MR vs. Healthcare Workers by Region and Profession",
    x = "Healthcare Workers per 10,000",
    y = "Under-5 Mortality Rate (per 1,000 live births)",
    color = "Profession"
  ) +
  scale_color_manual(values = c("Doctors" = "#1f77b4", "Nurses" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )

slide7_plot = ggplotly(slide7_plot, tooltip = "text")

slide7_plot


# Code for slide 8

# Get the data for Sub-Saharan Region
sub_saharan_u5mr <- u5mr_long %>%
  select(ISO.Code, Country.Name, SDG.Region, Uncertainty.Bounds., Year, U5MR) %>%
  filter(SDG.Region == "Sub-Saharan Africa",
         Uncertainty.Bounds.== "Median",
         Year %in% c(2000, 2023)
  )

# Get the data for year 2000 and 2023
sub_saharan_2023 = sub_saharan_u5mr %>%
  filter(Year == 2023) %>%
  arrange(desc(U5MR)) %>%
  rename(U5MR_2023 = U5MR)

sub_saharan_2000 = sub_saharan_u5mr %>%
  filter(Year == 2000) %>% 
  select(ISO.Code, Country.Name, U5MR) %>%
  rename(U5MR_2000 = U5MR)

# Combine top and bottom 5 countries
top_5_high = head(sub_saharan_2023, 5)
top_5_low = tail(sub_saharan_2023, 5)
selected_countries <- c(top_5_high$Country.Name, top_5_low$Country.Name)

# Filter data for selected countries and years
sub_saharan_2000_2023 = sub_saharan_2023 %>%
  left_join(sub_saharan_2000, by = "Country.Name") %>%
  filter(Country.Name %in% selected_countries) %>%
  select(Country.Name, U5MR_2023, U5MR_2000) %>%
  pivot_longer(
    cols = starts_with("U5MR"),
    names_to = "U5MR_Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = gsub("U5MR_", "", U5MR_Year),
    Year = as.factor(Year),
    Top_Category = case_when(
      Country.Name %in% top_5_high$Country.Name ~ "Top High",
      Country.Name %in% top_5_low$Country.Name ~ "Top Low"),
    Category_Year = paste(Top_Category, Year, sep = "_")
  )

country_order <- sub_saharan_2023 %>%
  filter(Country.Name %in% selected_countries) %>%
  arrange(desc(U5MR_2023)) %>%
  pull(Country.Name)
  
# Order countries based on 2023 U5MR for better visualization
sub_saharan_2000_2023$Country.Name <- factor(sub_saharan_2000_2023$Country.Name, 
                                             levels = country_order)

# Plot the data
slide8_plot <- sub_saharan_2000_2023 %>%
  plot_ly(x = ~Country.Name,
          y = ~U5MR,
          color = ~Category_Year,
          colors = c("Top High_2000" = "#d73027", 
                     "Top Low_2000" = "#fc8d59",
                     "Top High_2023" = "#1a9850", 
                     "Top Low_2023" = "#91cf60"),
          text = ~paste0("Country: ", Country.Name,
                         "<br>Year: ", Year,
                         "<br>U5MR: ", round(U5MR, 2)),
          hoverinfo = "text") %>%
  add_bars(name = ~Category_Year) %>%
  layout(
    title = "U5MR Comparison: Top and Bottom 5 Countries in Sub-Saharan Africa (2000 vs 2023)",
    xaxis = list(title = "Country",
                 categoryorder = ~Country.Name), 
    yaxis = list(title = "Under-5 Mortality Rate (per 1,000 live births)"),
    barmode = "group",
    showlegend = TRUE
  )

slide8_plot

# Code for slide 9

# Get U5MR in 2000 and 2023 for each country
u5mr_change = u5mr_long%>%
  filter(Uncertainty.Bounds. == "Median", Year %in% c(2000, 2023)) %>%
  select(Country.Name, Year, U5MR) %>%
  pivot_wider(
    names_from = Year,
    values_from = U5MR,
    names_prefix = "U5MR_") %>% 
  mutate(Change = U5MR_2000 - U5MR_2023) %>%
  arrange(desc(Change)) %>%
  drop_na()

change_map_data = world_map %>%
  left_join(u5mr_change, by = c("region" = "Country.Name")) %>%
  mutate(hover_text = paste0(region, "<br>Reduction: ", round(Change, 2), " deaths per 1,000"))

# Compute centroids for negative-change countries
decreased_countries = change_map_data %>%
  filter(Change < 0) %>%
  group_by(region) %>%
  summarize(
    long = mean(long, na.rm = TRUE),
    lat = mean(lat, na.rm = TRUE),
    Change = first(Change),
    hover_text = paste0(region, "<br>Increase: ", round(-Change, 2), " deaths per 1,000"),
    .groups = "drop"
  )

slide9_plot = ggplot(change_map_data,
                      aes(x = long, y = lat,
                          group = group,
                          fill = Change,
                          text = hover_text)) +
  geom_polygon(color = "gray80", size = 0.2) +
  coord_fixed(1.5) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "green", 
    midpoint = median(u5mr_change$Change),
    limits = c(-20, 150),
    name = "Reduction in U5MR"
  ) +
  geom_point(
    data = centroids,
    aes(x = long, y = lat, text = hover_text),
    color = "black",
    fill = "red",
    size = 1.5,
    shape = 21,
    stroke = 0.2,
    inherit.aes = FALSE
  ) +
  theme_void() +
  labs(title = "Progress on Reducing Under-Five Mortality")

slide9_plot = ggplotly(slide9_plot, tooltip = "text") 

slide9_plot

