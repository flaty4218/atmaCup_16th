library(tidyverse)

train_log <- read_csv("data/raw/train_log.csv", col_types = cols(yad_no = col_character()))
train_label <- read_csv("data/raw/train_label.csv", col_types = cols(yad_no = col_character()))
test_log <- read_csv("data/raw/test_log.csv", col_types = cols(yad_no = col_character()))
test_session <- read_csv("data/raw/test_session.csv")
yado <- read_csv("data/raw/yado.csv", col_types = cols(yad_no = col_character()))
sample_submission <- read_csv("data/raw/sample_submission.csv")

#yado_merge
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
  left_join(yado_tmp3, by = "yad_no")

yado_merged <- yado_merged %>% 
  select(yad_no, count_reserved, count_search_train, count_search_test) %>% 
  rename(yad_reserved = yad_no) %>% 
  mutate(
    count_reserved = replace_na(count_reserved, 0),
    count_search_train = replace_na(count_search_train, 0),
    count_search_test = replace_na(count_search_test, 0)
  )

#Rule2: session内で一緒に検索されている宿
log_merged <- bind_rows(train_log, test_log)

tmp <- log_merged %>% distinct(session_id, yad_no)

rule_2_list <- tmp %>% left_join(tmp, by = "session_id") %>% 
  filter(yad_no.x != yad_no.y) %>% 
  group_by(yad_no.x, yad_no.y) %>% 
  summarise(count = n()) %>% 
  rename(yad_no = yad_no.x, yad_reserved = yad_no.y) %>% 
  arrange(yad_no, desc(count))

rule_train <- train_log %>% 
  #head(1000) %>% 
  distinct(session_id, yad_no) %>% 
  left_join(rule_2_list, by = "yad_no") %>% 
  left_join(yado_merged, by = "yad_reserved") %>% 
  mutate(count_mod = count*((count_search_test+5)/(count_search_train+5))+count) %>% 
  group_by(session_id, yad_reserved) %>% 
  summarise(
    point_mod = sum(count_mod), 
    point = sum(count), 
    point_2 = sum(count_reserved), 
    point_3 = sum(count_search_test),
    point_4 = sum(count_search_train)
  ) %>% 
  arrange(session_id, desc(point_mod), desc(point), desc(point_2), desc(point_3), desc(point_4)) %>% 
  ungroup() %>% 
  group_by(session_id) %>% 
  mutate(rank = row_number())

rule_test <- test_log %>% 
  #tail(100000) %>% 
  distinct(session_id, yad_no) %>% 
  left_join(rule_2_list, by = "yad_no") %>% 
  left_join(yado_merged, by = "yad_reserved") %>% 
  mutate(count_mod = count*((count_search_test+5)/(count_search_train+5))+count) %>% 
  group_by(session_id, yad_reserved) %>% 
  summarise(
    point_mod = sum(count_mod), 
    point = sum(count), 
    point_2 = sum(count_reserved), 
    point_3 = sum(count_search_test),
    point_4 = sum(count_search_train)
  ) %>% 
  arrange(session_id, desc(point_mod), desc(point), desc(point_2), desc(point_3), desc(point_4)) %>% 
  ungroup() %>% 
  group_by(session_id) %>% 
  mutate(rank = row_number())

rule_train %>% rename(yad_no = yad_reserved) %>% select(session_id, yad_no, rank) %>% write_csv("data/output/train_rule_2.csv")
rule_test %>% rename(yad_no = yad_reserved) %>% select(session_id, yad_no, rank) %>% write_csv("data/output/test_rule_2.csv")
