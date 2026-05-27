library(tidyverse)
library(readxl)
library(ggstats)

# set up likert scales
likert_importance <- c("Not At All Important",
                       "Slightly Important",
                       "Moderately Important",
                       "Important",
                       "Very Important")

likert_knowledge <- c("No Knowledge",
                      "Low Knowledge",
                      "Moderate Knowledge",
                      "Good Knowledge",
                      "Very Good Knowledge")

likert_understanding <- c("No Understanding",
                          "Low Understanding",
                          "Moderate Understanding",
                          "Good Understanding",
                          "Very Good Understanding")

likert_frequency <- c("Not At All",
                      "Occasionally",
                      "About Half The Time",
                      "Most Of The Time",
                      "Every Time")

likert_effectiveness <- c("Not Effective At All",
                          "Slightly Effective",
                          "Moderately Effective",
                          "Effective",
                          "Very Effective")

likert_expectations <- c("Far Below Expectations",
                         "Below Expectations",
                         "Met Expectations",
                         "Above Expectations",
                         "Far Above Expectations")

# import each tab as a separate DF
# pivot longer and add phase/group identifiers
t1_teachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
           sheet = "PRE WEBINAR TandTAs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T1",
         group = "Teachers/TAs")

t1_headteachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
                              sheet = "PRE WEBINAR HTs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T1",
         group = "Headteachers")

t2_teachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
                          sheet = "POST WEBINAR TandTAs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T2",
         group = "Teachers/TAs")

t2_headteachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
                              sheet = "POST WEBINAR HTs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T2",
         group = "Headteachers")

t3_teachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
                          sheet = "POST REINFORCEMENT TandTAs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T3",
         group = "Teachers/TAs")

t3_headteachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
                              sheet = "POST REINFORCEMENT HTs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T3",
         group = "Headteachers")

t4_teachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
                          sheet = "FOLLOW UP TandTAs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T4",
         group = "Teachers/TAs")

t4_headteachers <- read_excel("data/MASTER COPY OF ALL SURVEYS - QUANT Qs.xlsx",
                              sheet = "FOLLOW UP HTs") |> 
  pivot_longer(cols = -participant,
               values_to = "response",
               names_to = "question_code") |> 
  mutate(phase = "T4",
         group = "Headteachers")

# bind into one DF
all_qs <- rbind(t1_teachers,
                t1_headteachers) |> 
  rbind(t2_teachers) |> 
  rbind(t2_headteachers) |> 
  rbind(t3_teachers) |> 
  rbind(t3_headteachers) |> 
  rbind(t4_teachers) |> 
  rbind(t4_headteachers)

# trim whitespace from responses
all_qs <- all_qs |> 
  mutate(response = str_trim(response,
                             side = "both"))

# import code-question lookup
q_code_lookup <- read_excel("data/question_code_lookup.xlsx")

# left join questions and lookup
all_qs <- left_join(all_qs,
                    q_code_lookup,
                    by = "question_code")

# try out gglikert() using faceting
all_qs |> 
  filter(question_code == "amr_handwashing") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = likert_knowledge))) |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group)) + 
  ggtitle("")

# try out gglikert_stacked()
all_qs |> 
  filter(question_code == "amr_handwashing",
         group == "Teachers/TAs") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = likert_knowledge))) |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   add_median_line = TRUE) + 
  ggtitle("")




