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
                position = position_dodge(0.9))
