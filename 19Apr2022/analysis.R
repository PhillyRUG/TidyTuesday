
# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2022-04-19')
tuesdata <- tidytuesdayR::tt_load(2022, week = 16)

big_dave <- tuesdata$big_dave

# Or read in the data manually

big_dave <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/big_dave.csv')
times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

## most common answers and definition

combined <- big_dave %>%
  rbind(times) 

big_dave %>%
  rbind(times) %>%
  group_by(answer) %>%
  summarize(n = n()) %>%
  filter(!is.na(answer)) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(answer, n),
             y = n)) + 
  geom_col() +
  coord_flip()

## most common long answers

combined %>%
  mutate(answer = gsub(" ", "", answer)) %>%
  filter(nchar(answer) > 10) %>%
  group_by(answer) %>%
  summarize(n = n()) %>%
  filter(!is.na(answer)) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(answer, n),
             y = n)) + 
  geom_col() +
  coord_flip()

## longest word

combined %>%
  #mutate(answer = gsub(" ", "", answer)) %>%
  mutate(nchar = nchar(answer)) %>%
  arrange(desc(nchar)) %>%
  top_n(10) %>%
  View()


## the top 20 clues in each source

combined

library(tidytext)

tidy_combined <- combined %>%
  mutate(clue = gsub('[[:digit:]]+', '', clue)) %>%
  unnest_tokens(word, clue) %>%
  group_by(word) %>%
  filter(n() > 10) %>%
  ungroup()

tidy_combined %>%
  count(source, word, sort = TRUE) %>%
  anti_join(get_stopwords()) %>%
  group_by(source) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(word, n, source), n,
             fill = source
  )) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~source, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    x = NULL, y = "Word count",
    title = "Most frequent words after removing stop words",
    #subtitle = "Words like 'said' occupy similar ranks but other words are quite different"
  )

## build a ML model

library(rsample)
combined_split <- combined %>%
  select(rowid) %>%
  initial_split()
train_data <- training(combined_split)
test_data <- testing(combined_split)

sparse_words <- tidy_combined %>%
  count(rowid, word) %>%
  inner_join(train_data) %>%
  cast_sparse(rowid, word, n)

word_rownames <- as.integer(rownames(sparse_words))

combined_joined <- data_frame(rowid = word_rownames) %>%
  left_join(combined %>%
              select(rowid, source))

## glmnet lasso reg
library(glmnet)
library(doMC)
registerDoMC(cores = 8)

times_xwd <- combined_joined$source == "times_xwd_times"
model <- cv.glmnet(sparse_words, times_xwd,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)


coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

coefs %>%
  filter(!is.na(term)) %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Coefficients that increase/decrease probability the most",
    #subtitle = "A document mentioning Martians is unlikely to be written by Jane Austen"
  )

intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

classifications <- tidy_combined %>%
  inner_join(test_data) %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(rowid) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(intercept + score))

classifications

library(yardstick)

comment_classes <- classifications %>%
  left_join(combined %>%
              select(source, rowid), by = "rowid") %>%
  mutate(source = as.factor(source))

## ROC
comment_classes %>%
  roc_curve(source, probability) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(
    color = "midnightblue",
    size = 1.5
  ) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  ) +
  labs(
    title = "ROC curve for text classification using regularized regression",
    #subtitle = "Predicting whether text was written by Jane Austen or H.G. Wells"
  )

## AUC

comment_classes %>%
  roc_auc(source, probability)
comment_classes %>%
  mutate(
    prediction = case_when(
      probability > 0.5 ~ "times_xwd_times",
      TRUE ~ "bigdave44"
    ),
    prediction = as.factor(prediction)
  ) %>%
  yardstick::conf_mat(source, prediction)

