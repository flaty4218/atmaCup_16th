library(tidyverse)

train_log <- read_csv("data/raw/train_log.csv", col_types = cols(yad_no = col_character()))
train_label <- read_csv("data/raw/train_label.csv", col_types = cols(yad_no = col_character()))
test_log <- read_csv("data/raw/test_log.csv", col_types = cols(yad_no = col_character()))
test_session <- read_csv("data/raw/test_session.csv")
yado <- read_csv("data/raw/yado.csv", col_types = cols(yad_no = col_character()))
sample_submission <- read_csv("data/raw/sample_submission.csv")

#Rule0: session内最後に登場しているもの = 除外
rule_0_train <- train_log %>% 
  group_by(session_id) %>% 
  mutate(seq_no_max = max(seq_no)) %>% 
  filter(seq_no == seq_no_max) %>% 
  select(session_id, yad_no) %>% 
  mutate(is_last = 1)

rule_0_test <- test_log %>% 
  group_by(session_id) %>% 
  mutate(seq_no_max = max(seq_no)) %>% 
  filter(seq_no == seq_no_max) %>% 
  select(session_id, yad_no) %>% 
  mutate(is_last = 1)

#Rule1: session内に登場しているもの
rule_train <- train_log %>% 
  left_join(rule_0_train, by = c("session_id", "yad_no")) %>% 
  filter(is.na(is_last) == 1) %>% 
  mutate(p = 1 +(seq_no*0.1)) %>% 
  group_by(session_id, yad_no) %>% 
  summarise(point = sum(p)) %>% 
  arrange(session_id, desc(point)) %>% 
  ungroup() %>% 
  group_by(session_id) %>% 
  mutate(rank = row_number())

rule_test <- test_log %>% 
  left_join(rule_0_test, by = c("session_id", "yad_no")) %>% 
  filter(is.na(is_last) == 1) %>% 
  mutate(p = 1 +(seq_no*0.1)) %>% 
  group_by(session_id, yad_no) %>% 
  summarise(point = sum(p)) %>% 
  arrange(session_id, desc(point)) %>% 
  ungroup() %>% 
  group_by(session_id) %>% 
  mutate(rank = row_number())

rule_train %>% select(session_id, yad_no, rank) %>% write_csv("data/output/train_rule_1.csv")
rule_test %>% select(session_id, yad_no, rank) %>% write_csv("data/output/test_rule_1.csv")