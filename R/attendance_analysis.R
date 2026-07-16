library(tidyverse)
library(PHEindicatormethods)

# import attendance data up to 28th June
attendance <- read.csv("data/Reception Attendance by School - 202526 YTD.csv") |> 
  janitor::clean_names()

# mark which schools received intervention
intervention_schools <- c("St Joseph's Catholic Primary School (B30)",
                          "Maryvale Catholic Primary School",
                          "Bellfield Infant School (NC)",
                          "St Gerard's Catholic Primary School",
                          "St Chad's Catholic Primary School")

attendance <- attendance |> 
  mutate(treatment = case_when(establishment_name %in% intervention_schools ~ "intervention",
                               .default = "control"))

# clean up attendance df
attendance <- attendance |> 
  mutate(overall_absence_rate = str_replace(overall_absence_rate, "%", ""),
         overall_absence_rate = as.numeric(overall_absence_rate))

# Work out school days per term -------------------------------------------

terms <- c("Autumn", "Spring", "Summer")
start_dates <- c(as.Date("2025-09-01"), as.Date("2026-01-05"), as.Date("2026-04-13"))
end_dates <- c(as.Date("2025-12-19"), as.Date("2026-03-27"), as.Date("2026-06-28"))

# combine into DF
term_lengths <- data.frame(term = terms,
                           start_date = start_dates,
                           end_date = end_dates)

# calculate number of weeks/days
term_lengths <- term_lengths |> 
  mutate(weeks = interval(start = start_date, end = end_date)/weeks(1), # find number of weeks between the dates
         weeks = round(weeks), # round up to whole number
         weeks = weeks - 1, # subtract 1 for half term
         days = weeks * 5) # multiply by 5 to get number of days

# merge onto attendance
attendance <- left_join(attendance,
                        term_lengths,
                        by = join_by("academic_term" == "term"))


# Calculate person-days and person-days missed --------------------------------------------

attendance <- attendance |> 
  mutate(person_days = no_pupils * days,
         person_days_missed = overall_absence_rate / 100 * no_pupils * days)


# Aggregate treatment groups ----------------------------------------------

attendance_agg <- attendance |> 
  group_by(treatment, academic_term) |> 
  summarise(person_days = sum(person_days),
            person_days_missed = sum(person_days_missed))

# calculate rate
attendance_agg <- attendance_agg |> 
  group_by(treatment, academic_term) |> 
  phe_rate(x = person_days_missed,
           n = person_days,
           multiplier = 1000)

ggplot(attendance_agg) +
  geom_col(aes(x = academic_term,
               y = value,
               fill = treatment,
               group = treatment),
           position = "dodge") +
  geom_errorbar(aes(x = academic_term,
                    ymin = lowercl,
                    ymax = uppercl,
                    group = treatment),
                width = .2,
                position = position_dodge(0.9)) +
  theme_light() +
  scale_fill_manual(values = c("#2c7fb8", "#a1dab4"),
                    breaks = c("control", "intervention"),
                    labels = c("Control schools", "Intervention schools"),
                    name = "") +
  custom_theming +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "Academic Term 2025-26",
       y = "Person-days missed per 1,000 person-days",
       fill = NULL,
       title = "")


# Calculate rate difference and CIs ---------------------------------------

library(ratesci)

ctrl_autumn_pd <- 951225
ctrl_autumn_pdm <- round(74312.25)
ctrl_summer_pd <- 657200
ctrl_summer_pdm <- round(45438.35)

ivn_autumn_pd <- 12300
ivn_autumn_pdm <- round(838.95)
ivn_summer_pd <- 8100
ivn_summer_pdm <- round(494.20)


ctrl_scoreci <- scoreci(x1 = ctrl_autumn_pdm,
        n1 = ctrl_autumn_pd,
        x2 = ctrl_summer_pdm,
        n2 = ctrl_summer_pd,
        distrib = "poi")
ctrl_rd <- ctrl_scoreci$estimates[2]
ctrl_lowerci <- ctrl_scoreci$estimates[1]
ctrl_upperci <- ctrl_scoreci$estimates[3]

ivn_scoreci <- scoreci(x1 = ivn_autumn_pdm,
                        n1 = ivn_autumn_pd,
                        x2 = ivn_summer_pdm,
                        n2 = ivn_summer_pd,
                        distrib = "poi")
ivn_rd <- ivn_scoreci$estimates[2]
ivn_lowerci <- ivn_scoreci$estimates[1]
ivn_upperci <- ivn_scoreci$estimates[3]

attendance_rd <- data.frame(treatment = c("control", "intervention"),
           rd = c(ctrl_rd, ivn_rd),
           lowerci = c(ctrl_lowerci, ivn_lowerci),
           upperci = c(ctrl_upperci, ivn_upperci))

attendance_rd |> 
  mutate(rd = rd*1000,
         lowerci = lowerci*1000,
         upperci = upperci*1000) |> 
  ggplot(aes(x = treatment)) +
  geom_col(aes(y = rd,
               fill = treatment)) +
  geom_errorbar(aes(ymin = lowerci,
                    ymax = upperci),
                width = 0.2) +
  theme_light() +
  scale_fill_manual(values = c("#2c7fb8", "#a1dab4"),
                    breaks = c("control", "intervention"),
                    labels = c("Control schools", "Intervention schools"),
                    name = "") +
  custom_theming +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "Treatment",
       y = "Autumn/Summer rate difference per 1,000 person-days",
       fill = NULL)
