library(pacman)
p_load(tidyverse, psych, irr, here, stringdist, stringr, purrr, kableExtra)
# read data. keep headers, drop first two lines after that

df <- read_csv(here(".", "retora_February 2, 2026_15.54.csv")) %>% 
  drop_na(PROLIFIC_PID)

# 1) Map evaluation number -> dimension key
dim_map <- tibble::tibble(
  eval_n = 1:10,
  dimension = c(
    "persuasiveness",
    "credibility",
    "trustworthiness",
    "agreement",
    "clarity",
    "relevance",
    "emotional_impact",
    "engagement_intention",
    "attitude_change",
    "memorability"
  )
)



df_labeled <- df %>%
  mutate(
    age_label = factor(
      age,
      levels = c(1,2,3,4,5,6,7),
      labels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    ),
    gender_label = factor(
      gender,
      levels = c(1,2,3,4,5),
      labels = c("Male", "Female", "Non-binary / third gender", "Prefer to self-describe", "Prefer not to say")
    ),
    gender_label = if_else(
      gender == 4 & !is.na(gender_4_TEXT) & gender_4_TEXT != "",
      paste0("Self-describe: ", gender_4_TEXT),
      as.character(gender_label)
    ),
    gender_label = factor(gender_label)
  )

# quick check
df_labeled %>% count(age, age_label)
df_labeled %>% count(gender, gender_label)
  
  
# df = your Qualtrics dataframe
df_long <- df_labeled %>%
  pivot_longer(
    cols = matches("^m(10|[1-9])_([1-9]|10)$"),
    names_to = "item",
    values_to = "score"
  ) %>%
  tidyr::extract(
    col = item,
    into = c("message_n", "eval_n"),
    regex = "^m(10|[1-9])_([1-9]|10)$",
    remove = TRUE
  ) %>%
  mutate(
    message_n = as.integer(message_n),
    eval_n = as.integer(eval_n)
  ) %>%
  left_join(dim_map, by = "eval_n") %>%
  relocate(message_n, dimension, score) %>% 
  drop_na(score)

msg_avg_rank <- df_long %>%
  group_by(message_n) %>%
  summarise(
    n_ratings = n(),
    avg_score = mean(score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(rank = dense_rank(desc(avg_score))) %>%
  arrange(rank, desc(avg_score))
df_long %>% glimpse

# now let's add retora data

# first let's try to get ids for messages

map_data<- read_csv("messages_surveyjs_id_map.csv")


retora_data <- read_csv(here(".", "custom_feedback_runs_by_group.csv"))

# let's check if map data matches retora data
setdiff(map_data$message_text, retora_data$message)
 
missing_txt <- setdiff(map_data$message_text, retora_data$message)

# find the closest candidate in retora_data
candidates <- retora_data %>% distinct(message) %>% pull(message)

# manually inspect best candidate
candidates[str_detect(candidates, "If the EU is serious about carbon neutrality")]

# once you identify the correct one, patch it:
map_data2 <- map_data %>%
  mutate(message_text = if_else(message_text == missing_txt,
                                candidates[str_detect(candidates, "If the EU is serious about carbon neutrality")][1],
                                message_text))
# now join
retora_data2 <- retora_data %>%
  left_join(map_data2, by = c("message" = "message_text"))

dims <- c(
  "persuasiveness","credibility","trustworthiness","agreement","clarity",
  "relevance","emotional_impact","engagement_intention","attitude_change","memorability"
)

# --- helper for robust joining on message text
make_key <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish() %>%
    str_sub(1, 60)
}

# 1) Retora wide -> long (0-10 already)
retora_long <- retora_data2 %>%
  pivot_longer(
    cols = all_of(dims),
    names_to = "dimension",
    values_to = "score"
  ) %>%
  mutate(
    source = "retora",
    msg_key = make_key(message)
  ) %>%
  select(source, msg_key, message_prefix, run_number, age, gender, dimension, score)




map2 <- map_data %>%
  mutate(msg_key = make_key(message_text)) %>%
  select(msg_key, message_prefix)

# 1) Human long: add message_prefix from message_n
human_long <- df_long %>%
  mutate(
    source = "human",
    message_prefix = sprintf("msg_%02d", message_n)
  ) %>%
  select(source, message_prefix, ResponseId, age=age_label, gender=gender_label, dimension, score)


# 3) Combine
both_long <- bind_rows(
  human_long %>% select(source, message_prefix, dimension, score,age, gender),
  retora_long %>% select(source, message_prefix, dimension, score,age, gender)
)


# 4) Compare mean score per message × source (averaging across 10 dimensions and all raters/runs)
msg_compare <- both_long %>%
  group_by(source, message_prefix, age) %>%
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    n = sum(!is.na(score)),
    .groups = "drop"
  ) %>%
  group_by(source) %>%
  mutate(rank = dense_rank(desc(mean_score))) %>%
  ungroup() %>%
  arrange(source, rank)

msg_compare

# 5) Message-level agreement: differences + correlations
msg_wide <- msg_compare %>%
  select(source, message_prefix, mean_score) %>%
  pivot_wider(names_from = source, values_from = mean_score) %>%
  mutate(diff_retora_minus_human = retora - human)

msg_wide

msg_wide %>%
  summarise(
    spearman = cor(retora, human, method = "spearman", use = "complete.obs"),
    pearson  = cor(retora, human, method = "pearson",  use = "complete.obs")
  )

# 6) Plot: mean message scores
ggplot(msg_compare, aes(x = message_prefix, y = mean_score, color = source, group = source)) +
  geom_point(size = 2) +
  geom_line() +
  labs(x = "Message", y = "Mean score (0–10)", title = "Human vs Retora: mean score by message") +
  theme_minimal()

# let plot for age
ggplot(msg_compare, aes(x = message_prefix, y = mean_score, color = source, group = source)) +
  geom_point(size = 2) +
  geom_line() +
  labs(x = "Message", y = "Mean score (0–10)", title = "Human vs Retora: mean score by message") +
  theme_minimal()+
  facet_wrap(~age)

# 7) Plot: scatter (message-level) 
# let's also add correlation line?
# can we also add correlation coef?
ggplot(msg_wide, aes(x = human, y = retora, label = message_prefix)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_text(vjust = -0.7, size = 3) +
  labs(x = "Human mean score", y = "Retora mean score", title = "Human vs Retora (per message)") +
  theme_minimal()



# let's do it for several dimensions

dim_map <- tibble::tribble(
  ~dimension,              ~dimension_label,      ~question_text,
  "persuasiveness",        "Persuasiveness",      "Overall, this message is convincing.",
  "credibility",           "Credibility",         "The claims in this message are believable.",
  "trustworthiness",       "Trustworthiness",     "The communicator behind this message seems honest rather than misleading.",
  "agreement",             "Agreement",           "I agree with the main point of this message.",
  "clarity",               "Clarity",             "This message is clear and easy to understand.",
  "relevance",             "Relevance",           "This message feels personally relevant to me.",
  "emotional_impact",      "Emotional impact",    "This message triggers a strong emotional response in me.",
  "engagement_intention",  "Engagement intention","I would be willing to share or discuss this message with others.",
  "attitude_change",       "Attitude change",     "This message makes me more supportive of its position than I was before.",
  "memorability",          "Memorability",        "This message is memorable; I will remember it later."
)


library(dplyr)
library(tidyr)
library(ggplot2)

# Overall mean score per message (across dimensions), then rank within source
msg_rank_overall <- both_long %>%
  group_by(source, message_prefix) %>%
  summarise(mean_score = mean(score, na.rm = TRUE), .groups = "drop") %>%
  group_by(source) %>%
  mutate(rank = dense_rank(mean_score)) %>%  # lowest=1, highest=10
  ungroup()

# Human vs Retora rank comparison (overall)
msg_rank_overall_wide <- msg_rank_overall %>%
  select(source, message_prefix, mean_score, rank) %>%
  pivot_wider(
    names_from = source,
    values_from = c(mean_score, rank),
    names_sep = "_"
  ) %>%
  mutate(rank_diff = rank_retora - rank_human)

stats <- msg_rank_overall_wide %>%
  summarise(
    spearman = cor(rank_human, rank_retora, method = "spearman", use = "complete.obs"),
    pearson  = cor(rank_human, rank_retora, method = "pearson",  use = "complete.obs")
  ) %>%
  mutate(
    r2 = summary(lm(rank_retora ~ rank_human, data = msg_rank_overall_wide))$r.squared,
    label = sprintf("Spearman ρ = %.2f\nPearson r = %.2f\nLM R² = %.2f", spearman, pearson, r2)
  )

ggplot(msg_rank_overall_wide, aes(x = rank_human, y = rank_retora, label = message_prefix)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  geom_text(vjust = -0.6, size = 3) +
  annotate("text",
           x = 1, y = 10,
           hjust = 0, vjust = 1,
           label = stats$label,
           size = 3.5) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10)) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(
    x = "Human rank (1=worst, 10=best)",
    y = "Retora rank (1=worst, 10=best)",
    title = "Rank comparison by message (overall across dimensions)"
  ) +
  theme_minimal()



rank_lines <- msg_rank_overall %>%
  mutate(
    message_id = as.integer(str_extract(message_prefix, "\\d+"))
  ) %>%
  arrange(message_id)

# Line plot of ranks by message id
ggplot(rank_lines, aes(x = message_id, y = rank, color = source, group = source)) +
  geom_point(size = 2) +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  labs(
    x = "Message ID",
    y = "Average rank (1=worst, 10=best)",
    title = "Human vs Retora: average rank by message"
  ) +
  theme_minimal()


l 

# 1) Overall mean score per message × source (averaging across all 10 dimensions + all raters/runs)
msg_means <- bind_rows(human_long, retora_long) %>%
  group_by(source, message_prefix) %>%
  summarise(mean_score = mean(score, na.rm = TRUE),
            n = sum(!is.na(score)),
            .groups = "drop")

# 2) Convert mean scores into ranks within each source (1=worst, 10=best)
# If you want 10=best, compute worst_rank then flip.
msg_ranks <- msg_means %>%
  group_by(source) %>%
  mutate(rank_worst_is_1 = dense_rank(mean_score)) %>%     # low mean = 1
  ungroup() %>%
  group_by(source) %>%
  mutate(rank_best_is_10 = max(rank_worst_is_1) - rank_worst_is_1 + 1) %>%
  ungroup()

# 3) Wide table: human vs retora ranks side-by-side
rank_wide <- msg_ranks %>%
  select(source, message_prefix, mean_score, n, rank_best_is_10) %>%
  pivot_wider(
    names_from  = source,
    values_from = c(mean_score, n, rank_best_is_10),
    names_sep   = "_"
  ) %>%
  rename(
    human_mean   = mean_score_human,
    retora_mean  = mean_score_retora,
    human_n      = n_human,
    retora_n     = n_retora,
    rank_human   = rank_best_is_10_human,
    rank_retora  = rank_best_is_10_retora
  )

# 4) Add message text from map_data and make the final display table
# map_data must contain message_prefix and message_text (it does in your file)
msg_rank_table <- rank_wide %>%
  left_join(map_data %>% select(message_prefix, message_text), by = "message_prefix") %>%
  mutate(
    message_stub = str_trunc(message_text, 90)
  ) %>%
  arrange(rank_retora) %>%   # or arrange(rank_human)
  select(message_prefix, message_stub, rank_retora, rank_human, retora_mean, human_mean, retora_n, human_n)

msg_rank_table %>% kable() %>% kable_cleaner()
