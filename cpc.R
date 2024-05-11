# Load the required packages
library(openintro)

# Load the datasets
data(census)
data(sowc_child_mortality)
data(sowc_demographics)
data(sowc_maternal_newborn)

# Let's explore the dataset
print(str(census))

# 1. Remove NA values from personal income variable in census data
census_no_na <- census[!is.na(census$total_personal_income), ]
print(str(census_no_na))

# 2. See only the 25th to 35th observations of the census data
print(census[25:35, ])

# 3. Find the mean total income for every person in the census dataset
mean_total_income <- mean(census$total_personal_income, na.rm = TRUE)
cat("Mean total income for every person in the census dataset:", mean_total_income, "\n")

# 4. Join sowc_demographics and sowc_child_mortality
joined_data <- merge(sowc_demographics, sowc_child_mortality, by = "countries_and_areas", 
                     suffixes = c("", ".drop"))
joined_data <- joined_data[, c("countries_and_areas", "life_expectancy_2018", "under5_mortality_2018")]
print(str(joined_data))

# 5. Left join with sowc_maternal_newborn dataset
joined_data <- merge(joined_data, sowc_maternal_newborn, by = "countries_and_areas", all.x = TRUE)
print(str(joined_data))

# 6. Create a variable called mean_life_exp
sowc_demographics$mean_life_exp <- rowMeans(sowc_demographics[, c("life_expectancy_1970", "life_expectancy_2000", "life_expectancy_2018")], na.rm = TRUE)
print(head(sowc_demographics["mean_life_exp"]))

# 7. Bar chart of top 12 countries with highest mean_life_exp
# Select top 12 countries by mean life expectancy
top_12 <- head(sowc_demographics[order(-sowc_demographics$mean_life_exp), ], 12)

# Define custom colors for the bars
barplot_colors <- c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2", "#59a14f", "#edc948", "#b07aa1", "#ff9da7", "#9c755f", "#bab0ac", "#ffca5d", "#a9cce3")

# Set up plot parameters
par(mar = c(5, 8, 4, 2)) # Adjust plot margins

# Create bar plot
bp <- barplot(height = top_12$mean_life_exp,
        names.arg = top_12$countries_and_areas,
        main = "Top 12 Countries by Mean Life Expectancy",
        xlab = "Country",
        ylab = "Mean Life Expectancy",
        col = barplot_colors,
        las = 2,
        cex.names = 0.7, # Increase font size for country names
        ylim = c(0, max(top_12$mean_life_exp) + 2), # Set y-axis limits
        space = 0.5, # Adjust space between bars
        beside = TRUE) # Plot bars side by side

# Add labels to each bar with adjusted positions
for (i in 1:length(top_12$mean_life_exp)) {
  text(x = bp[i], # x-coordinate of label
       y = top_12$mean_life_exp[i] - 0.75, # y-coordinate of label
       labels = round(top_12$mean_life_exp[i], 1), # mean life expectancy value rounded to 1 decimal place
       pos = 3, # position the label above the bar
       cex = 0.7) # set font size of label
}

# 8. Countries present in sowc_maternal_newborn but not in sowc_child_mortality
countries_diff <- setdiff(sowc_maternal_newborn$countries_and_areas, sowc_child_mortality$countries_and_areas)
cat("Countries present in sowc_maternal_newborn but not in sowc_child_mortality:", "\n", paste(countries_diff, collapse = ", "), "\n")

# 9. Highest personal income for each race
highest_income_by_race <- aggregate(total_personal_income ~ race_general, data = census, max, na.rm = TRUE)
print(highest_income_by_race)

# 10. Fill NA values in under18_pop_2018 with 0
sowc_demographics$under18_pop_2018[is.na(sowc_demographics$under18_pop_2018)] <- 0
