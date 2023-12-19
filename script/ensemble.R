library(tidyverse)

train_log <- read_csv("data/raw/train_log.csv", col_types = cols(yad_no = col_character()))
train_label <- read_csv("data/raw/train_label.csv", col_types = cols(yad_no = col_character()))
test_log <- read_csv("data/raw/test_log.csv", col_types = cols(yad_no = col_character()))
test_session <- read_csv("data/raw/test_session.csv")
yado <- read_csv("data/raw/yado.csv", col_types = cols(yad_no = col_character()))
sample_submission <- read_csv("data/raw/sample_submission.csv")

test_rule_1 <- read_csv("data/output/test_rule_1.csv", col_types = cols(yad_no = col_character())) %>% filter(rank <= 20, is.na(yad_no) != 1)
test_rule_2 <- read_csv("data/output/test_rule_2.csv", col_types = cols(yad_no = col_character())) %>% filter(rank <= 20, is.na(yad_no) != 1)
test_rule_3 <- read_csv("data/output/test_rule_3.csv", col_types = cols(yad_no = col_character())) %>% filter(rank <= 20, is.na(yad_no) != 1)
test_rule_4 <- read_csv("data/output/test_rule_4.csv", col_types = cols(yad_no = col_character())) %>% filter(rank <= 20, is.na(yad_no) != 1)
test_rule_5 <- read_csv("data/output/test_rule_5.csv", col_types = cols(yad_no = col_character())) %>% filter(rank <= 20, is.na(yad_no) != 1)
test_rule_6 <- read_csv("data/output/test_rule_6.csv", col_types = cols(yad_no = col_character())) %>% filter(rank <= 20, is.na(yad_no) != 1)
test_rule_7 <- read_csv("data/output/test_rule_7.csv", col_types = cols(yad_no = col_character())) %>% filter(rank <= 20, is.na(yad_no) != 1)

ensemble <- function(W, N, folds, lasts, df_1, df_2, df_3, df_4, df_5, df_6, df_7) {
  
  folds <- folds %>% filter(fold == N)
  
  df_1 <- df_1 %>% filter(session_id %in% folds$session_id) %>% left_join(lasts, by = c("session_id", "yad_no")) %>% filter(is.na(is_last) == 1) %>% mutate(rank = ifelse(rank>10, 10.5, rank), point = (11-rank)*W[1])
  df_2 <- df_2 %>% filter(session_id %in% folds$session_id) %>% left_join(lasts, by = c("session_id", "yad_no")) %>% filter(is.na(is_last) == 1) %>% mutate(rank = ifelse(rank>10, 10.5, rank), point = (11-rank)*W[2])
  df_3 <- df_3 %>% filter(session_id %in% folds$session_id) %>% left_join(lasts, by = c("session_id", "yad_no")) %>% filter(is.na(is_last) == 1) %>% mutate(rank = ifelse(rank>10, 10.5, rank), point = (11-rank)*W[3])
  df_4 <- df_4 %>% filter(session_id %in% folds$session_id) %>% left_join(lasts, by = c("session_id", "yad_no")) %>% filter(is.na(is_last) == 1) %>% mutate(rank = ifelse(rank>10, 10.5, rank), point = (11-rank)*W[4])
  df_5 <- df_5 %>% filter(session_id %in% folds$session_id) %>% left_join(lasts, by = c("session_id", "yad_no")) %>% filter(is.na(is_last) == 1) %>% mutate(rank = ifelse(rank>10, 10.5, rank), point = (11-rank)*W[5])
  df_6 <- df_6 %>% filter(session_id %in% folds$session_id) %>% left_join(lasts, by = c("session_id", "yad_no")) %>% filter(is.na(is_last) == 1) %>% mutate(rank = ifelse(rank>10, 10.5, rank), point = (11-rank)*W[6])
  df_7 <- df_7 %>% filter(session_id %in% folds$session_id) %>% left_join(lasts, by = c("session_id", "yad_no")) %>% filter(is.na(is_last) == 1) %>% mutate(rank = ifelse(rank>10, 10.5, rank), point = (11-rank)*W[7])
  
  df_merged <- bind_rows(df_1, df_2, df_3, df_4, df_5, df_6, df_7) %>% 
    group_by(session_id, yad_no) %>% 
    summarise(total_point = sum(point)) %>% 
    arrange(session_id, desc(total_point)) %>% 
    ungroup() %>% 
    group_by(session_id) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank <= 10)
  
  return(df_merged)
}

W <- c(0.5, 0.28, 0.12, 0.1, 0.0, 0.0, 0.0)

rule_0_test <- test_log %>% 
  group_by(session_id) %>% 
  mutate(seq_no_max = max(seq_no)) %>% 
  filter(seq_no == seq_no_max) %>% 
  select(session_id, yad_no) %>% 
  mutate(is_last = 1)

test_fold <- test_session %>% 
  mutate(
    num = row_number()-1,
    fold = floor(num/nrow(test_session)*20)
  )

test_merged <- ensemble(W, 0, test_fold, rule_0_test, test_rule_1, test_rule_2, test_rule_3, test_rule_4, test_rule_5, test_rule_6, test_rule_7)

for (i in 1:19) {
  tmp <- ensemble(W, i, test_fold, rule_0_test, test_rule_1, test_rule_2, test_rule_3, test_rule_4, test_rule_5, test_rule_6, test_rule_7)
  test_merged <- bind_rows(test_merged, tmp)
}

test_wide <- test_merged %>% 
  mutate(name = paste0("predict_", (rank-1))) %>% 
  pivot_wider(id_cols = session_id, names_from = name, values_from = yad_no)

sub <- test_session %>% 
  left_join(test_wide, by = "session_id") %>% 
  select(-session_id)

summary(sub)

sub %>% write_csv("data/sub/sub.csv")
