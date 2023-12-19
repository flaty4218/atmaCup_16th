library(tidyverse)

train_log <- read_csv("data/raw/train_log.csv", col_types = cols(yad_no = col_character()))
train_label <- read_csv("data/raw/train_label.csv", col_types = cols(yad_no = col_character()))
test_log <- read_csv("data/raw/test_log.csv", col_types = cols(yad_no = col_character()))
test_session <- read_csv("data/raw/test_session.csv")
yado <- read_csv("data/raw/yado.csv", col_types = cols(yad_no = col_character()))
sample_submission <- read_csv("data/raw/sample_submission.csv")

#Rule7: session内に登場しているwid_cdで予約数の多い宿
yado_tmp <- train_label %>% 
  group_by(yad_no) %>% 
  summarise(count_reserved = n())

yado_tmp2 <- train_log %>% 
  distinct(session_id, yad_no) %>% 
  group_by(yad_no) %>% 
  summarise(count_search_train = n())

yado_tmp3 <- test_log %>% 
  distinct(session_id, yad_no) %>% 
  group_by(yad_no) %>% 
  summarise(count_search_test = n())

yado_merged <- yado %>% 
  left_join(yado_tmp, by = "yad_no") %>% 
  left_join(yado_tmp2, by = "yad_no") %>% 
  left_join(yado_tmp3, by = "yad_no") %>% 
  mutate(
    count_reserved = replace_na(count_reserved, 0),
    count_search_train = replace_na(count_search_train, 0),
    count_search_test = replace_na(count_search_test, 0)
  ) %>% 
  mutate(count_reserved_mod = (count_reserved+1)*((count_search_test+5)/(count_search_train+5)))

reserved_wid <- yado_merged %>% 
  arrange(wid_cd, desc(count_reserved_mod)) %>% 
  rename(yad_reserved = yad_no, count = count_reserved_mod) %>% 
  ungroup() %>% 
  group_by(wid_cd) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 20) %>% 
  select(wid_cd, yad_reserved, count, rank)

rule_train <- train_log %>% 
  #head(1000) %>% 
  distinct(session_id, yad_no) %>% 
  left_join(yado, by = "yad_no") %>% 
  select(session_id, yad_no, wid_cd) %>% 
  group_by(session_id) %>% 
  mutate(yado_count = n_distinct(yad_no)) %>% 
  ungroup() %>% 
  left_join(reserved_wid, by = "wid_cd") %>% 
  group_by(session_id, yad_reserved, wid_cd) %>% 
  summarise(point = sum(count/yado_count)) %>% 
  arrange(session_id, desc(point)) %>% 
  ungroup() %>% 
  group_by(session_id) %>% 
  mutate(rank = row_number())

rule_test <- test_log %>% 
  #head(1000) %>% 
  distinct(session_id, yad_no) %>% 
  left_join(yado, by = "yad_no") %>% 
  select(session_id, yad_no, wid_cd) %>% 
  group_by(session_id) %>% 
  mutate(yado_count = n_distinct(yad_no)) %>% 
  ungroup() %>% 
  left_join(reserved_wid, by = "wid_cd") %>% 
  group_by(session_id, yad_reserved, wid_cd) %>% 
  summarise(point = sum(count/yado_count)) %>% 
  arrange(session_id, desc(point)) %>% 
  ungroup() %>% 
  group_by(session_id) %>% 
  mutate(rank = row_number())

rule_train %>% rename(yad_no = yad_reserved) %>% select(session_id, yad_no, rank) %>% write_csv("data/output/train_rule_7.csv")
rule_test %>% rename(yad_no = yad_reserved) %>% select(session_id, yad_no, rank) %>% write_csv("data/output/test_rule_7.csv")