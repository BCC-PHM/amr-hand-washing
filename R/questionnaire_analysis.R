library(tidyverse)
library(readxl)
library(ggstats)
library(labelled)
library(patchwork)

# Data import -------------------------------------------------------------


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


# Plots -------------------------------------------------------------------


# set up var_label list for time points
timepoint_labels <- list(
  participant = NULL,
  group = NULL,
  full_question = NULL,
  likert_scale = NULL,
  T1 = "Pre-webinar",
  T2 = "Post-webinar",
  T3 = "Post-reinforcement",
  T4 = "Follow up"
)

# define custom theme
custom_theming <- theme(legend.position = "bottom",
                        strip.background = element_rect(fill = "black"),
                        strip.text = element_text(colour = "white"),
                        text = element_text(size = 12),
                        plot.title = element_text(face="bold", size = 16),
                        plot.title.position = "plot")

count_lookup <- all_qs |> 
  distinct(participant, group,phase) |> 
  group_by(group, phase) |> 
  count() |> 
  mutate(label_group = paste0(group, " (n=", n, ")"),
         label_phase = paste0(phase, " (n=", n, ")"))



# Knowledge of AMR --------------------------------------------------------
ref <- "amr_knowledge"

# gglikert() with faceting
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  set_variable_labels(T1 = "Pre-webinar",
                      T2 = "Post-webinar") |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# gglikert_stacked() with faux faceting

p1 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Headteachers") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=6)",
                      T2 = "Post-webinar\n(n=6)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p2 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |>
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=15)",
                      T2 = "Post-webinar\n(n=10)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p1 / p2 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "between phases"), 80)) &
  custom_theming

# table
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  group_by(group, phase, response) |> 
  count() |> 
  group_by(group, phase) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(round(pct, 1), "% (", n, ")")) |> 
  select(-n, -pct, -total) |> 
  pivot_wider(names_from =  "response",
              values_from = "text") |> 
  mutate(phase = case_when(phase == "T1" ~ "Pre-webinar",
                           phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  select(group, phase, `No Knowledge`, `Low Knowledge`, `Moderate Knowledge`, `Good Knowledge`, `Very Good Knowledge`) |> 
  arrange(group)

# AMR and handwashing -----------------------------------------------------

ref <- "amr_handwashing"

# gglikert() with faceting
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  set_variable_labels(T1 = "Pre-webinar",
                      T2 = "Post-webinar") |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# gglikert_stacked() with faux faceting

p1 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Headteachers") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=6)",
                      T2 = "Post-webinar\n(n=6)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p2 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=15)",
                      T2 = "Post-webinar\n(n=10)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p1 / p2 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "between phases"), 80)) &
  custom_theming

# table
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  group_by(group, phase, response) |> 
  count() |> 
  group_by(group, phase) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(round(pct, 1), "% (", n, ")")) |> 
  select(-n, -pct, -total) |> 
  pivot_wider(names_from =  "response",
              values_from = "text") |> 
  mutate(phase = case_when(phase == "T1" ~ "Pre-webinar",
                           phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  select(group, phase, `No Knowledge`, `Low Knowledge`, `Moderate Knowledge`, `Good Knowledge`, `Very Good Knowledge`) |> 
  arrange(group)


# Understanding of NHS handwashing steps  ---------------------------------

ref <- "handwashing_steps_understanding"

# gglikert() with faceting
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  set_variable_labels(T1 = "Pre-webinar",
                      T2 = "Post-webinar") |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# gglikert_stacked() with faux faceting

p1 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Headteachers") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=6)",
                      T2 = "Post-webinar\n(n=6)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p2 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=15)",
                      T2 = "Post-webinar\n(n=10)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p1 / p2 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "between phases"), 80)) &
  custom_theming

# table
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  group_by(group, phase, response) |> 
  count() |> 
  group_by(group, phase) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(round(pct, 1), "% (", n, ")")) |> 
  select(-n, -pct, -total) |> 
  pivot_wider(names_from =  "response",
              values_from = "text") |> 
  mutate(phase = case_when(phase == "T1" ~ "Pre-webinar",
                           phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  mutate(`No Understanding` = NA,
         `Low Understanding` = NA) |> 
  select(group, phase, `No Understanding`, `Low Understanding`, `Moderate Understanding`, `Good Understanding`, `Very Good Understanding`) |> 
  arrange(group)

# Perceived important of children washing hands using NHS steps -----------

ref <- "handwashing_steps_importance"

# gglikert() with faceting
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  set_variable_labels(T1 = "Pre-webinar",
                      T2 = "Post-webinar",
                      T3 = "Post-reinforcement",
                      T4 = "Follow up") |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# gglikert_stacked() with faux faceting

p1 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Headteachers") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=6)",
                      T2 = "Post-webinar\n(n=6)",
                      T3 = "Post-reinforcement\n(n=5)",
                      T4 = "Follow up\n(n=2)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p2 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=15)",
                      T2 = "Post-webinar\n(n=10)",
                      T3 = "Post-reinforcement\n(n=8)",
                      T4 = "Follow up\n(n=6)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p1 / p2 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "between phases"), 80)) &
  custom_theming

# table
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  group_by(group, phase, response) |> 
  count() |> 
  group_by(group, phase) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(round(pct, 1), "% (", n, ")")) |> 
  select(-n, -pct, -total) |> 
  pivot_wider(names_from =  "response",
              values_from = "text") |> 
  mutate(phase = case_when(phase == "T1" ~ "Pre-webinar",
                           phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  mutate(`Not At All Important` = NA,
         `Slightly Important` = NA) |> 
  select(group, phase, `Not At All Important`, `Slightly Important`, `Moderately Important`, Important, `Very Important`) |> 
  arrange(group)

# Understanding of relevant handwashing context/times  ---------------------------------

ref <- "relevant_times_understanding"

# gglikert() with faceting
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  set_variable_labels(T1 = "Pre-webinar",
                      T2 = "Post-webinar") |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# gglikert_stacked() with faux faceting

p1 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Headteachers") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=6)",
                      T2 = "Post-webinar\n(n=6)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p2 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=15)",
                      T2 = "Post-webinar\n(n=10)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p1 / p2 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "between phases"), 80)) &
  custom_theming

# table
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  group_by(group, phase, response) |> 
  count() |> 
  group_by(group, phase) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(round(pct, 1), "% (", n, ")")) |> 
  select(-n, -pct, -total) |> 
  pivot_wider(names_from =  "response",
              values_from = "text") |> 
  mutate(phase = case_when(phase == "T1" ~ "Pre-webinar",
                           phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  mutate(`No Understanding` = NA) |> 
  select(group, phase, `No Understanding`, `Low Understanding`, `Moderate Understanding`, `Good Understanding`, `Very Good Understanding`) |> 
  arrange(group)

# Perceived important of children washing hands at relevant times/ --------

ref <- "relevant_times_importance"

# gglikert() with faceting
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  set_variable_labels(T1 = "Pre-webinar",
                      T2 = "Post-webinar",
                      T3 = "Post-reinforcement",
                      T4 = "Follow up") |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_rows = vars(group),
           labels_accuracy = 0.1,
           y_label_wrap = 12) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# gglikert_stacked() with faux faceting

p1 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Headteachers") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=6)",
                      T2 = "Post-webinar\n(n=6)",
                      T3 = "Post-reinforcement\n(n=5)",
                      T4 = "Follow up\n(n=2)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p2 <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-question_code) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T1 = "Pre-webinar\n(n=15)",
                      T2 = "Post-webinar\n(n=10)",
                      T3 = "Post-reinforcement\n(n=8)",
                      T4 = "Follow up\n(n=6)") |> 
  gglikert_stacked(include = starts_with("T", ignore.case = F),
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~group,
             strip.position = "right")

p1 / p2 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "between phases"), 80)) &
  custom_theming

all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  group_by(group, phase, response) |> 
  count() |> 
  group_by(group, phase) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(round(pct, 1), "% (", n, ")")) |> 
  select(-n, -pct, -total) |> 
  pivot_wider(names_from =  "response",
              values_from = "text") |> 
  mutate(phase = case_when(phase == "T1" ~ "Pre-webinar",
                           phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  mutate(`Not At All Important` = NA,
         `Slightly Important` = NA,
         `Moderately Important` = NA) |> 
  select(group, phase, `Not At All Important`, `Slightly Important`, `Moderately Important`, Important, `Very Important`) |> 
  arrange(group)

# Perceived effectiveness of project elements - relevant times/contexts--------

ref <- "element_time"

elements_data <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs",
         phase == "T3") |> 
  mutate(group = "Teachers/TAs (n=8)") |> 
  select(-phase, -full_question) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with("element"), ~ factor(.x, levels = get_likert_scale(ref))))
  

var_label(elements_data) <- c("group", get_labels(.starts_with = ref, .compare = "within phase"))
  
elements_data |> 
  gglikert_stacked(include = starts_with("element"),
                   sort = "descending",
                   sort_method = "mean",
                   labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "within phase"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming +
  facet_wrap(~group,
             strip.position = "right")

all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "element")) |>
  mutate(element = str_trim(element)) |> 
  group_by(element, group, phase) |> 
  count(response) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(pct, "% (", n, ")"),
         wt = case_when(response == "Not At All Effective" ~ 1,
                        response == "Slightly Effective" ~ 2,
                        response == "Moderately Effective" ~ 3,
                        response == "Effective" ~ 4,
                        response == "Very Effective" ~ 5)) |>
  group_by(element) |> 
  mutate(mean = mean(n*wt)) |> 
  arrange(desc(mean)) |> 
  mutate(element = fct_inorder(element)) |> 
  select(-n, -total, -pct, -wt, -mean) |> 
  pivot_wider(names_from = "response",
              values_from = "text") |> 
  select(group, phase, element, `Not At All Effective`, `Slightly Effective`, `Moderately Effective`, `Effective`, `Very Effective`)

# Perceived effectiveness of project elements - handwashing steps --------

ref <- "element_steps"

elements_data <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs",
         phase == "T3") |> 
  mutate(group = "Teachers/TAs (n=8)") |>
  select(-phase, -full_question) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with("element"), ~ factor(.x, levels = get_likert_scale(ref))))


var_label(elements_data) <- c("group",get_labels(.starts_with = ref, .compare = "within phase"))

elements_data |> 
  gglikert_stacked(include = starts_with("element"),
                   sort = "descending",
                   sort_method = "mean",
                   labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "within phase"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming +
  facet_wrap(~group,
             strip.position = "right")

all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "element")) |>
  mutate(element = str_trim(element)) |> 
  group_by(element, group, phase) |> 
  count(response) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(pct, "% (", n, ")"),
         wt = case_when(response == "Not At All Effective" ~ 1,
                        response == "Slightly Effective" ~ 2,
                        response == "Moderately Effective" ~ 3,
                        response == "Effective" ~ 4,
                        response == "Very Effective" ~ 5)) |>
  group_by(element) |> 
  mutate(mean = mean(n*wt)) |> 
  arrange(desc(mean)) |> 
  mutate(element = fct_inorder(element)) |> 
  select(-n, -total, -pct, -wt, -mean) |> 
  pivot_wider(names_from = "response",
              values_from = "text") |> 
  mutate(`Slightly Effective` = NA) |> 
  select(group, phase, element, `Not At All Effective`, `Slightly Effective`, `Moderately Effective`, `Effective`, `Very Effective`)

# Perceived effectiveness of project elements - remind and prompt --------

ref <- "element_reminder"

elements_data <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs",
         phase == "T3") |> 
  mutate(group = "Teachers/TAs (n=8)") |>
  select(-phase, -full_question) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with("element"), ~ factor(.x, levels = get_likert_scale(ref))))


var_label(elements_data) <- c("group",get_labels(.starts_with = ref, .compare = "within phase"))

elements_data |> 
  gglikert_stacked(include = starts_with("element"),
                   sort = "descending",
                   sort_method = "mean",
                   labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "within phase"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming +
  facet_wrap(~group,
             strip.position = "right")

all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "element")) |>
  mutate(element = str_trim(element)) |> 
  group_by(element, group, phase) |> 
  count(response) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(pct, "% (", n, ")"),
         wt = case_when(response == "Not At All Effective" ~ 1,
                        response == "Slightly Effective" ~ 2,
                        response == "Moderately Effective" ~ 3,
                        response == "Effective" ~ 4,
                        response == "Very Effective" ~ 5)) |>
  group_by(element) |> 
  mutate(mean = mean(n*wt)) |> 
  arrange(desc(mean)) |> 
  mutate(element = fct_inorder(element)) |> 
  select(-n, -total, -pct, -wt, -mean) |> 
  pivot_wider(names_from = "response",
              values_from = "text") |> 
  select(group, phase, element, `Not At All Effective`, `Slightly Effective`, `Moderately Effective`, `Effective`, `Very Effective`)

# Perceived effectiveness of project elements - headteachers --------

ref <- "element_ht"

elements_data <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Headteachers",
         phase == "T3") |> 
  mutate(group = "Headteachers (n=5)") |>
  select(-phase, -full_question) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with("element"), ~ factor(.x, levels = get_likert_scale(ref))))


var_label(elements_data) <- c("group",get_labels(.starts_with = ref, .compare = "within phase"))

elements_data |> 
  gglikert_stacked(include = starts_with("element"),
                   sort = "descending",
                   sort_method = "median",
                   labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "within phase"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming +
  facet_wrap(~group,
             strip.position = "right")

all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "element")) |>
  mutate(element = str_trim(element)) |> 
  group_by(element, group, phase) |> 
  count(response) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(pct, "% (", n, ")"),
         wt = case_when(response == "Not At All Effective" ~ 1,
                        response == "Slightly Effective" ~ 2,
                        response == "Moderately Effective" ~ 3,
                        response == "Effective" ~ 4,
                        response == "Very Effective" ~ 5)) |>
  group_by(element) |> 
  mutate(mean = mean(n*wt)) |> 
  arrange(desc(mean)) |> 
  mutate(element = fct_inorder(element)) |> 
  select(-n, -total, -pct, -wt, -mean) |> 
  pivot_wider(names_from = "response",
              values_from = "text") |> 
  mutate(`Not At All Effective` = NA,
         `Slightly Effective` = NA) |> 
  select(group, phase, element, `Not At All Effective`, `Slightly Effective`, `Moderately Effective`, `Effective`, `Very Effective`)

# Pupil behaviour - handwashing steps -------------------------------------

ref <- "freq_step"

# compare steps, facet by phase
elements_data <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-group, -full_question) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with(ref), ~ factor(.x, levels = get_likert_scale(ref))))


var_label(elements_data) <- c("phase", get_labels(.starts_with = ref, .compare = "within phase"))

p1 <- elements_data |> 
  filter(phase == "T2") |> 
  mutate(phase = "Post-webinar (n=10)") |> 
  gglikert_stacked(include = starts_with(ref),
                   sort = "none",
                   labels_accuracy = 0.1) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~phase,
             strip.position = "top")

p2 <- elements_data |> 
  filter(phase == "T3") |> 
  mutate(phase = "Post-reinforcement (n=8)") |> 
  gglikert_stacked(include = starts_with(ref),
                   sort = "none",
                   labels_accuracy = 0.1) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~phase,
             strip.position = "top")

p3 <- elements_data |> 
  filter(phase == "T4") |> 
  mutate(phase = "Follow up (n=6)") |> 
  gglikert_stacked(include = starts_with(ref),
                   labels_accuracy = 0.1) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~phase,
             strip.position = "top")

p1 + p2 + p3 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "within phase"), 80)) &
  custom_theming

# compare single step between phases

step <- "3"

all_qs |> 
  filter(question_code %in% paste0("freq_step_", step),
         group == "Teachers/TAs") |> 
  select(-group, -full_question) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  select(-participant, -likert_scale, -question_code) |> 
  mutate(across(starts_with("T"), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T2 = "Post-webinar",
                      T3 = "Post-reinforcement",
                      T4 = "Follow up") |> 
  gglikert_stacked(labels_accuracy = 0.1) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming +
  ggtitle(str_wrap(get_title(.starts_with = paste0("freq_step_", step),
                             .compare = "between phases"), 80))

all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  separate_wider_delim(full_question,
                       delim = "?",
                       names = c("full_question_1", "element")) |>
  mutate(element = str_trim(element)) |>
  group_by(group, phase, response, element) |> 
  count() |> 
  group_by(group, phase, element) |> 
  mutate(total = sum(n),
         pct = n/total*100,
         text = paste0(round(pct, 1), "% (", n, ")")) |> 
  select(-n, -pct, -total) |> 
  pivot_wider(names_from =  "response",
              values_from = "text") |> 
  mutate(phase = case_when(phase == "T1" ~ "Pre-webinar",
                           phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  select(group, phase, `Not At All`, `Occasionally`, `About Half The Time`, `Most Of The Time`, `Every Time`) |> 
  arrange(desc(phase), element) |> 
  View()

# Pupil behaviour - handwashing contexts -------------------------------------

ref <- "freq_context"

# compare contexts, facet by phase (staggered)
all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-group, -full_question) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with(ref), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(freq_context_before_lunch = "Before Lunch",
                      freq_context_after_toilet = "After Going To The Toilet",
                      freq_context_after_play = "After Outside Play") |> 
  mutate(phase = case_when(phase == "T2" ~ "Post-webinar",
                           phase == "T3" ~ "Post-reinforcement",
                           phase == "T4" ~ "Follow up")) |> 
  gglikert(include = starts_with(ref, ignore.case = F),
           facet_cols = vars(phase),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "within phase"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# compare contexts, facet by phase (stacked)
elements_data <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-group, -full_question) |> 
  pivot_wider(names_from = "question_code",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with(ref), ~ factor(.x, levels = get_likert_scale(ref))))


var_label(elements_data) <- c("phase" ,get_labels(.starts_with = ref, .compare = "within phase"))

p1 <- elements_data |> 
  filter(phase == "T2") |> 
  mutate(phase = "Post-webinar (n=10)") |> 
  gglikert_stacked(include = starts_with(ref),
                   sort = "none",
                   labels_accuracy = 0.1) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~phase,
             strip.position = "top")

p2 <- elements_data |> 
  filter(phase == "T3") |> 
  mutate(phase = "Post-reinforcement (n=8)") |> 
  gglikert_stacked(include = starts_with(ref),
                   sort = "none",
                   labels_accuracy = 0.1) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~phase,
             strip.position = "top")

p3 <- elements_data |> 
  filter(phase == "T4") |> 
  mutate(phase = "Follow up (n=6)") |> 
  gglikert_stacked(include = starts_with(ref),
                   labels_accuracy = 0.1) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~phase,
             strip.position = "top")

p1 + p2 + p3 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "within phase"), 80)) &
  custom_theming

# compare phases, facet by context (staggered)
all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-group, -full_question) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with("T"), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T2 = "Post-webinar",
                      T3 = "Post-reinforcement",
                      T4 = "Follow up") |> 
  mutate(question_code = case_when(question_code == "freq_context_before_lunch" ~ "Before Lunch",
                                   question_code == "freq_context_after_toilet" ~ "After Going To The Toilet",
                                   question_code == "freq_context_after_play" ~ "After Outside Play")) |> 
  gglikert(include = starts_with("T", ignore.case = F),
           facet_cols = vars(question_code),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "within phase"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# compare phases, facet by context (stacked)
elements_data <- all_qs |> 
  filter(question_code %in% get_codes(ref),
         group == "Teachers/TAs") |> 
  select(-group, -full_question) |> 
  pivot_wider(names_from = "phase",
              values_from = "response") |> 
  select(-participant, -likert_scale) |> 
  mutate(across(starts_with("T"), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  set_variable_labels(T2 = "Post-webinar (n=10)",
                      T3 = "Post-reinforcement (n=8)",
                      T4 = "Follow up (n=6)")

p1 <- elements_data |> 
  filter(question_code == "freq_context_before_lunch") |> 
  mutate(question_code = "Before Lunch") |> 
  gglikert_stacked(include = starts_with("T"),
                   sort = "none",
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~question_code,
             strip.position = "top")

p2 <- elements_data |> 
  filter(question_code == "freq_context_after_toilet") |> 
  mutate(question_code = "After Going To The Toilet") |> 
  gglikert_stacked(include = starts_with("T"),
                   sort = "none",
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~question_code,
             strip.position = "top")

p3 <- elements_data |> 
  filter(question_code == "freq_context_after_play") |> 
  mutate(question_code = "After Outside Play") |> 
  gglikert_stacked(include = starts_with("T"),
                   sort = "none",
                   labels_accuracy = 0.1,
                   y_label_wrap = 12) + 
  scale_fill_brewer(palette = "YlGnBu") +
  facet_wrap(~question_code,
             strip.position = "top")

p1 + p2 + p3 +
  plot_layout(guides = "collect",
              axes = "collect") +
  plot_annotation(title = str_wrap(get_title(ref, "within phase"), 80)) &
  custom_theming


# Extent to which project met expectations  ------------------------------

ref <- "expectations_met"

# gglikert() with faceting
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  pivot_wider(names_from = "group",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  #set_variable_labels(T3 = "Post-reinforcement") |> 
  gglikert(include = `Teachers/TAs`:Headteachers,
           #facet_rows = vars(group),
           labels_accuracy = 0.1) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming

# gglikert_stacked()
all_qs |> 
  filter(question_code %in% get_codes(ref)) |> 
  select(-question_code) |> 
  mutate(phase = "Post-intervention",
         group = case_when(group == "Teachers/TAs" ~ "Teachers/TAs (n=8)",
                           group == "Headteachers" ~ "Headteachers (n=5)")) |> 
  pivot_wider(names_from = "group",
              values_from = "response") |> 
  mutate(across(starts_with("T", ignore.case = F), ~ factor(.x, levels = get_likert_scale(ref)))) |> 
  select(-participant, -full_question, -likert_scale) |> 
  #set_variable_labels(T3 = "Post-reinforcement") |> 
  gglikert_stacked(include = `Teachers/TAs (n=8)`:`Headteachers (n=5)`,
           #facet_rows = vars(group),
           labels_accuracy = 0.1,
           y_label_wrap = 13) + 
  ggtitle(str_wrap(get_title(ref, "between phases"), 80)) +
  scale_fill_brewer(palette = "YlGnBu") +
  custom_theming +
  facet_wrap(~phase,
             strip.position = "right")


# Other -------------------------------------------------------------------



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
         text = paste0(pct, "% (", n, ")"),
         text = case_when(!is.na(text) ~ text,
                          TRUE ~ "0%")) |> 
  select(-n, -total, -pct) |> 
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
