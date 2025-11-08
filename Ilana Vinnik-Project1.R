
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(maps)
library(viridis)
library(ggridges)
library(scales)

# Read the data from the CSV file
Data <- read_csv("~/Downloads/NCHS_-_Leading_Causes_of_Death__United_States.csv")

# Display the first few rows of the data
head(Data)

# Display column names
names(Data)

# Remove unnecessary column
Data_cleaned <- select(Data, -`113 Cause Name`)

# Display the first few rows of cleaned data
head(Data_cleaned)

# Display structure of the cleaned data
str(Data_cleaned)

# Filter data to include only USA and all causes
Data_USA <- filter(Data_cleaned, State == "United States" & `Cause Name` != "All causes")

# Find top 10 causes of death in the USA
top_10_causes <- Data_USA %>%
  group_by(`Cause Name`) %>%
  summarise(TotalDeaths = sum(Deaths)) %>%
  arrange(desc(TotalDeaths)) %>%
  top_n(10)

# Display the top 10 causes
head(top_10_causes)

# Define a custom theme
theme_Ilana <- function() {
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        text = element_text(family = "Times"),
        axis.text.y = element_text(hjust = 0, color = "black", size = 15),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_text(hjust = 0, size = 8),
        axis.line = element_line(color = "gray"),
        plot.margin = margin(1, 1, 1, 2, "cm"),
        legend.position = "none")
} 

# Extract year and death rate for all causes
Data_Clean_filtered <- filter(Data_cleaned, `Cause Name` == "All causes", State == "United States")
Year_rate <- select(Data_Clean_filtered, Year, `Age-adjusted Death Rate`)
head(Year_rate)

# Filter data for the year 2017
Data_Clean_filtered1 <- filter(Data_cleaned, `Cause Name` == "All causes", Year == 2017, State != "United States")
Data_State <- select(Data_Clean_filtered1, State, Year, `Age-adjusted Death Rate`, Deaths)
Data_State1 <- select(Data_Clean_filtered1, State, `Age-adjusted Death Rate`, Deaths)
Data_State1$`Age-adjusted Death Rate` <- as.numeric(Data_State1$`Age-adjusted Death Rate`)
head(Data_State1)

# Define color palettes
blue_tones <- c("#446b93", "#4b7198", "#54789d", "#5f7fa2", "#6888a7", "#738eaa", "#7f98b1", "#8ba0b5", "#98a9bb", "#a3b2bf", "#b3bbc5", "#c0c5c8")
gray_tones <- c("#cfcfcf")
red_tones <- c("#cdbec3", "#ccafb7", "#c89fab", "#c48294", "#c27388", "#bc5873", "#b84060", "#b42e4c")
blue_palette <- colorRampPalette(blue_tones)(12)
gray_palette <- colorRampPalette(gray_tones)(1)
red_palette <- colorRampPalette(red_tones)(8)
custom_colors <- c(blue_palette, gray_palette, red_palette)
pie(rep(2, length(custom_colors)), col = custom_colors)

# Get USA map
us_states <- map_data("state")
us_states$region <- tolower(us_states$region)
Data_State1$State <- tolower(Data_State1$State)
merged_data <- merge(us_states, Data_State1, by.x = "region", by.y = "State", all.x = TRUE)

# Filter data for heart disease
heart_disease_data <- filter(Data_cleaned, `Cause Name` == "Heart disease")

# Create a data frame for state abbreviations
state_abbreviations <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
            "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", 
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  Abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                   "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                   "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                   "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                   "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
)

# Replace state names with abbreviations
heart_disease_data <- left_join(heart_disease_data, state_abbreviations, by = "State") %>%
  select(-State) %>%
  rename(State = Abbreviation)

# Remove rows with NA values
heart_disease_data <- heart_disease_data[!is.na(heart_disease_data$State), ]

# Create a horizontal bar plot
ggplot(top_10_causes, aes(x = reorder(`Cause Name`, TotalDeaths), y = TotalDeaths, fill = `Cause Name`)) +
  geom_bar(stat = "identity", width = 0.97) +
  coord_flip() +
  labs(title = "Number of Deaths for the 10 Leading Causes of Death, 2017",
       x = " ",
       y = " ") +
  scale_fill_manual(values = c("#c0c5c8", "#bc5873", "#c89fab", "#a3b2bf", "#b42e4c", "#8ba0b5", "#54789d", "#c27388", "#446b93", "#cdbec3")) +
  scale_y_continuous(labels = comma_format(), expand = expansion(mult = c(0, 0.1))) +
  theme_classic()  +
  theme_Ilana()

# Create a map
ggplot(merged_data, aes(x = long, y = lat, group = group, fill = `Age-adjusted Death Rate`)) +
  geom_polygon(color = "#484848", size = 0.1) +
  coord_map() +
  scale_fill_gradientn(colors = custom_colors, 
                       name = "Legend for age-adjusted death rate per 100,000 U.S. standard population", 
                       breaks = seq(0, max(merged_data$`Age-adjusted Death Rate`), by = 40)) + 
  theme_void() +
  labs(title = "Age-adjusted Death Rates for All causes, by State: 2017 ") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 8),
        text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        legend.title = element_text(size = 10)) +
  guides(fill = guide_colorbar(title.position = "top", barwidth = 20),
         keywidth = unit(5, "inches"))

# Create a timeline graph
ggplot(Year_rate, aes(x = Year, y = `Age-adjusted Death Rate`)) +
  geom_line(color = "#446b93", size = 1.5) +  
  geom_point(color = "#54789d", size = 2) + 
  geom_ribbon(aes(ymin = 60, ymax = `Age-adjusted Death Rate`), fill = "#b42e4c", alpha = 0.1) +
  labs(title = "Age-adjusted Death Rates vs. Years",
       x = " ",
       y = "Rate per 100,000 U.S standard population") +
  scale_y_continuous(limits = c(60, max(Year_rate$`Age-adjusted Death Rate`)),
                     breaks = seq(60, 900, by = 60)) +
  scale_x_continuous(limits = c(1997, max(Year_rate$Year)), breaks = seq(1997, 2017, by = 2)) +
  theme_classic() + 
  theme(text = element_text(family = "Times"),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 15),
        plot.caption = element_text(hjust = 0, size = 8),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10, hjust = 0.5))

# Create a heatmap of heart disease death rates by state over time
ggplot(heart_disease_data, aes(x = Year, y = State, fill = `Age-adjusted Death Rate`)) +
  geom_tile(color = "white", width = 1, height = 1.1) + 
  scale_fill_gradientn(colors = custom_colors, 
                       name = "Death Rate per 100,000 Population", 
                       na.value = "white") +  
  labs(x = "", y = "", 
       title = "Heart Disease Death Rates by State U.S., 1997-2017") +  
  scale_x_continuous(breaks = seq(1997, 2017, by = 5), labels = seq(1997, 2017, by = 5), position = "top") +
  geom_vline(xintercept = 2005, color = "black", linetype = "solid") +
  theme_minimal() +
  theme(text = element_text(family = "Times"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size = 7.5), 
        axis.text.x = element_text(size = 15),
        legend.position = "right",
        panel.grid = element_blank(),
        legend.title = element_text(size = 8),
        axis.title.y = element_text(margin = margin(r = 0), color = "black"),  
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        aspect.ratio = 1.7) + 
  guides(fill = guide_colorbar(title.position = "top", keywidth = unit(8, "cm")))  # Add vertical line after 2006
