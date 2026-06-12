library(tidyverse)
library(readxl)
library(ggstats)
library(labelled)

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

likert_effectiveness <- c("Not At All Effective",
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
  select(-participant, -full_question, -likert_scale)
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group)) + 
  ggtitle("How would you rate your current knowledge of the relationship between AMR and handwashing?") +
  scale_fill_brewer(palette = "YlGnBu")

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
  ggtitle("How would you rate your current knowledge of the relationship between \nAMR and handwashing?") +
  scale_fill_brewer(palette = "YlGnBu")

# use gglikert() for a question which spans all 4 phases
all_qs |> 
  filter(question_code == "relevant_times_importance") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = likert_importance))) |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group)) + 
  scale_fill_brewer(palette = "YlGnBu") +
  ggtitle("To what extent do you think it's important for pupils to be washing their hands \nat the relevant times/ contexts within the school day?")

# compare effectiveness of elements - supporting pupils to wash hands at relevant times/contexts
elements_context <- t3_teachers |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(participant, element_time_handwashing_song:element_time_stickers) |> 
  filter(participant == 1) |> 
  pivot_longer(cols = element_time_handwashing_song:element_time_stickers,
               names_to = "question_code",
               values_to = "response") |> 
  pull(question_code)

elements_context_labels <- all_qs |> 
  filter(question_code %in% elements_context,
         group == "Teachers/TAs",
         phase == "T3") |> 
  filter(participant == 1) |> 
  select(full_question) |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "full_question_2")) |> 
  select(full_question_2) |> 
  mutate(full_question_2 = str_trim(full_question_2)) |> 
  pull(full_question_2)
  
elements_context_data <- all_qs |> 
  filter(question_code %in% elements_context,
         group == "Teachers/TAs",
         phase == "T3") |> 
  select(-phase, -group, -full_question) |> 
    pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  mutate(across(element_time_handwashing_song:element_time_stickers, ~ factor(.x, levels = likert_effectiveness))) |> 
  select(-participant, -likert_scale)

var_label(elements_context_data) <- elements_context_labels
  
elements_context_data |> 
  gglikert_stacked(sort = "descending",
                   sort_method = "median") + 
  scale_fill_brewer(palette = "YlGnBu") +
  ggtitle("In your opinion, how effective were the following elements of the project in \nsupporting pupils to wash their hands during the relevant times and contexts?")

# compare effectiveness of elements - supporting pupils to follow handwashing steps
elements_steps <- t3_teachers |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(participant, element_steps_handwashing_song:element_steps_stickers) |> 
  filter(participant == 1) |> 
  pivot_longer(cols = element_steps_handwashing_song:element_steps_stickers,
               names_to = "question_code",
               values_to = "response") |> 
  pull(question_code)

elements_steps_labels <- all_qs |> 
  filter(question_code %in% elements_steps,
         group == "Teachers/TAs",
         phase == "T3") |> 
  filter(participant == 1) |> 
  select(full_question) |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "full_question_2")) |> 
  select(full_question_2) |> 
  mutate(full_question_2 = str_trim(full_question_2)) |> 
  pull(full_question_2)

elements_steps_data <- all_qs |> 
  filter(question_code %in% elements_steps,
         group == "Teachers/TAs",
         phase == "T3") |> 
  select(-phase, -group, -full_question, -likert_scale) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  mutate(across(element_steps_handwashing_song:element_steps_stickers, ~ factor(.x, levels = likert_effectiveness))) |> 
  select(-participant, -likert_scale)

var_label(elements_steps_data) <- elements_steps_labels
  
elements_steps_data |> 
  gglikert_stacked(sort = "descending",
                   sort_method = "median") + 
  scale_fill_brewer(palette = "YlGnBu") +
  ggtitle("In your opinion, how effective were the following elements \nof the project in supporting pupils to wash their hands \nfollowing the NHS recommended handwashing steps?")


all_qs |> 
  filter(question_code %in% elements_steps,
         group == "Teachers/TAs",
         phase == "T3") |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "element")) |>
  mutate(element = str_trim(element)) |> 
  group_by(element) |> 
  count(response) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(pct, "% (", n, ")")) |> 
  select(-n, -total, - pct) |> 
  pivot_wider(names_from = "response",
              values_from = "text")


# compare frequency of observing each step within a phase

freq_steps <- t3_teachers |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(participant, freq_step_1:freq_step_9) |> 
  filter(participant == 1) |> 
  pivot_longer(cols = freq_step_1:freq_step_9,
               names_to = "question_code",
               values_to = "response") |> 
  pull(question_code)

t2_steps_freq_plot <- all_qs |> 
  filter(question_code %in% freq_steps,
         phase == "T2") |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("question", "step")) |> 
  mutate(step = str_trim(step)) |> 
  select(-phase, -group, -question_code) |> 
  pivot_wider(names_from = "step",
              values_from = "response") |>
  select(-question, -participant, -likert_scale) |> 
  mutate(across(everything(), ~ factor(.x, levels = likert_frequency))) |>
  gglikert(totals_include_center = TRUE) + 
  scale_fill_brewer(palette = "YlGnBu") +
  ggtitle("Within these contexts, overall how often do pupils in your class do the following NHS recommended handwashing steps?")

t3_steps_freq_plot <- all_qs |> 
  filter(question_code %in% freq_steps,
         phase == "T3") |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("question", "step")) |> 
  mutate(step = str_trim(step)) |> 
  select(-phase, -group, -question_code) |> 
  pivot_wider(names_from = "step",
              values_from = "response") |>
  select(-question, -participant, -likert_scale) |> 
  mutate(across(everything(), ~ factor(.x, levels = likert_frequency))) |>
  gglikert() + 
  scale_fill_brewer(palette = "YlGnBu") +
  ggtitle("")

cowplot::plot_grid(t2_steps_freq_plot,
                   t3_steps_freq_plot,
                   labels = c("Pre-intervention (T2)", "Post-intervention (T3)"),
                   ncol = 2,
                   align = "h")

library(patchwork)

t2_steps_freq_plot | t3_steps_freq_plot
  
# compare frequency of observing a step between phases
all_qs |> 
  filter(question_code == "freq_step_4") |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("question", "step")) |> 
  mutate(step = str_trim(step)) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |>
  select(starts_with("T")) |> 
  mutate(across(everything(), ~ factor(.x, levels = likert_frequency))) |>
  gglikert() + 
  scale_fill_brewer(palette = "YlGnBu") +
  ggtitle("Within these contexts, overall how often do pupils in your class do the following\nNHS recommended handwashing steps? Step 4: Rub The Inside Of Your Fingers")
