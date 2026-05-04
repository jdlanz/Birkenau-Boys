library(tidyverse)
library(ggplot2)
library(lubridate)

BBoys <- read_csv("data/BirkenauBoys.csv")

glimpse(BBoys)
str(BBoys)
summary(BBoys)

table(BBoys$Nationality)           # counts per nationality

BBoys %>%                          # creates graph of BBoys nationalities
  filter(!is.na(Nationality)) %>%
  count(Nationality, sort = TRUE) %>%
  ggplot(aes(x = reorder(Nationality, n), y = n)) +
  geom_col(fill = "#4C78A8") +
  coord_flip() +  # horizontal bars (easier to read)
  labs(title = "Birkenau Boys' Nationalities",
       x = "Nationality",
       y = "Count") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

BBoys <- BBoys %>%                   # creates new column with age at Birkenau arrival
  mutate(
    birth_date = make_date(BirthYear, BirthMonth, BirthDay),
    arrival_date = make_date(BirkenauArrivalYear, BirkenauArrivalMonth, 1),
    AgeArrival = time_length(interval(birth_date, arrival_date), "years")
  )

BBoys %>%                            # plots age on arrival in Birkenau 
  filter(!is.na(AgeArrival)) %>%
  ggplot(aes(x = AgeArrival)) +
  geom_histogram(binwidth = 1, fill = "#4C78A8", color = "white") +
  labs(title = "Age Upon Arrival at Birkenau",
       x = "Age (years)",
       y = "Number of Boys") +
  theme_minimal(base_size = 12) +
  theme(
     plot.title = element_text(hjust = 0.5),
     panel.grid = element_blank()
     )

BBoys %>%                        # plots the impact of arrival age on survival rates
  filter(!is.na(AgeArrival), !is.na(DiedBeforeLiberation)) %>%
  mutate(DiedBeforeLiberation = ifelse(DiedBeforeLiberation == "Y", "Yes",
                                       ifelse(DiedBeforeLiberation == "N", "No", DiedBeforeLiberation))) %>%
  ggplot(aes(x = DiedBeforeLiberation, y = AgeArrival, color = DiedBeforeLiberation)) +
  geom_jitter(width = 0.4, height = 0, alpha = 0.7, size = 2) +
  labs(
    title = "Age and Survival Rates (Relative Frequency)",
    x = "Died Before Liberation",
    y = "Age at Arrival (years)",
    color = "Died Before Liberation"
  ) +
  scale_color_manual(values = c("No" = "#4C78A8", "Yes" = "#F58518")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, vjust = 2, face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid = element_blank(),    # completely removes all grid lines
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

BBoys %>%                           # compares age at Birkenau with nationality
  filter(!is.na(AgeArrival), !is.na(Nationality), Nationality != "Yugoslav") %>%
  group_by(Nationality) %>%
  summarise(mean_age = mean(AgeArrival)) %>%
  arrange(desc(mean_age)) %>%
  ggplot(aes(x = reorder(Nationality, mean_age), y = mean_age)) +
  geom_col(fill = "steelblue") +
  coord_flip(ylim = c(5, NA)) +  # only zoom, doesn't remove bars
  labs(
    title = "Average Age at Arrival by Nationality",
    x = "Nationality",
    y = "Age at Arrival"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

# calculate Boys' age upon arrival at Terezin
BBoys$BirthDate <- as.Date(with(BBoys, paste(BirthYear, BirthMonth, BirthDay, sep = "-")))
BBoys$TerezinDate <- as.Date(with(BBoys, paste(TerezinArrivalYear, TerezinArrivalMonth, "15", sep = "-")))
BBoys$AgeAtTerezin <- as.numeric(difftime(BBoys$TerezinDate, BBoys$BirthDate, units = "days")) / 365.25

# creates variable tracking amount of time between arrival in Terezin and Birkenau
BBoys$TimeBetweenCamps <- (BBoys$BirkenauArrivalYear - BBoys$TerezinArrivalYear) * 12 +
  (BBoys$BirkenauArrivalMonth - BBoys$TerezinArrivalMonth)

# Count of prisoners per Post-Birkenau camp
PostBirkenauCamps <- BBoys %>%
  count(PostBirkenau, name = "Total_Prisoners") %>%
  arrange(desc(Total_Prisoners))

# Splits post-Birkenau camps apart

PostBirkenauCamps_split <- PostBirkenauCamps %>%
  # remove rows where PostBirkenau is NA
  filter(!is.na(PostBirkenau)) %>%
  # split semicolon-separated camp names into separate rows
  separate_rows(PostBirkenau, sep = ";") %>%
  # remove extra spaces
  mutate(PostBirkenau = trimws(PostBirkenau)) %>%
  # sum Total_Prisoners if the same camp appears multiple times
  group_by(PostBirkenau) %>%
  summarise(Total_Prisoners = sum(Total_Prisoners), .groups = "drop") %>%
  arrange(desc(Total_Prisoners))

# Bar chart of post-Birkenau Camps

PostBirkenauCamps_clean <- PostBirkenauCamps_split %>%
  mutate(PostBirkenau = gsub("_", " ", PostBirkenau))  # replace underscores with spaces

ggplot(PostBirkenauCamps_clean, aes(x = reorder(PostBirkenau, Total_Prisoners), y = Total_Prisoners)) +
  geom_bar(stat = "identity", fill = "#2E8B57", width = 0.7) +  # dark green bars
  coord_flip() +  # horizontal orientation
  theme_minimal(base_size = 12, base_family = "Helvetica") +  # clean font
  labs(
    title = "Life After the Death Marches",
    x = "Post-Birkenau Camp",
    y = "Number of Birkenau Boys"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 15)), # centered, bigger
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)), # bold x-axis title
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)), # bold y-axis title
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    panel.grid = element_blank(),  # remove grids
    panel.background = element_rect(fill = "white", color = NA),  # clean panel
    plot.background = element_rect(fill = "white", color = NA),
    axis.ticks = element_line(color = "black")  # subtle ticks
  )
