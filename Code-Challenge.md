Code Challenge
================
Yifei Xu
2023-03-10

# Question 1 - Data Cleaning

``` r
# Import the data and id
load("data/Data.RData")
id = read.table("data/IDs.txt", header=FALSE) %>%
  rename("id" = "V1")

# Map IDs onto masterdemoid and join
map = bsrc.findid(id, idmap, "id")
subjects = merge(x = HAM, y = map, by.x = "masterdemoid", by.y = "id")

# Calculate the HAM scores of each subject (except 3a to 3e) and the mean score
final_df = subjects %>% 
  mutate(ham_date = lubridate::ymd(ham_date)) %>%
  mutate(ham_score = subjects %>% 
           select(starts_with("ham_")) %>% 
           select(-starts_with("ham_3a_"):-starts_with("ham_3e_"), -ham_date) %>% 
           rowSums(na.rm = TRUE)) %>%
  group_by(masterdemoid) %>%
  mutate(mean_ham_score = mean(ham_score)) %>%
  slice_max(ham_date) %>%
  select(masterdemoid, ham_date, ham_score, mean_ham_score) %>%
  distinct()

# Subjects with id 211253, 211669, 220244, 221183, 221220 had multiple visit time points on their latest visit date and the scores were the same. Only one latest value for each subject was kept.
```

# Question 2 - Data Visualization

``` r
# Total number of subjects
p_total = recruitment_data %>%
  ggplot(aes(x = RecruitSource)) +
  geom_bar(aes(fill="All Concented Participants")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 0.5, size = 3) + 
  labs(x = "Recruitment Sources",
       y = "# of Participants",
       title = "Recruitment Sources_Total",
       subtitle = "Data updated on 10-04-2020",
       fill = "Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_manual(values = c("All Concented Participants" = "#8fcaee"))
  

# Number of subjects by Age
p_age = recruitment_data %>%
  mutate(age_group = case_when(Age >= 30 & Age <= 39 ~ "30-39",
                               Age >= 40 & Age <= 49 ~ "40-49",
                               Age >= 50 & Age <= 59 ~ "50-59",
                               Age >= 60 & Age <= 69 ~ "60-69",
                               Age >= 70 ~ "70+")) %>%
  ggplot(aes(x = RecruitSource)) +
  geom_bar(aes(fill = age_group)) +
  geom_text(aes(label = ..count.., group = age_group), stat = "count", position = position_stack(vjust = 1), size = 3) + 
  labs(x = "Recruitment Sources",
       y = "# of Participants",
       title = "Recruitment Sources_Age",
       subtitle = "Data updated on 10-04-2020",
       fill = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_brewer(palette = 1) 


# Number of subjects by Gender
p_gender = recruitment_data %>%
  ggplot(aes(x = RecruitSource)) +
  geom_bar(aes(fill = Gender)) +
  geom_text(aes(label = ..count.., group = Gender), stat = "count", position = position_stack(vjust = 1), size = 3) + 
  labs(x = "Recruitment Sources",
       y = "# of Participants",
       title = "Recruitment Sources_Gender",
       subtitle = "Data updated on 10-04-2020",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_brewer(palette = 1, labels=c("Female", "Male"))


# Number of subjects by Group
p_group = recruitment_data %>%
  ggplot(aes(x = RecruitSource)) +
  geom_bar(aes(fill = Group)) +
  geom_text(aes(label = ..count.., group = Group), stat = "count", position = position_stack(vjust = 1), size = 3) + 
  labs(x = "Recruitment Sources",
       y = "# of Participants",
       title = "Recruitment Sources_Group",
       subtitle = "Data updated on 2020-04-11",
       fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_fill_brewer(palette = 1) 


# export
ggsave(plot = p_total, width = 8, height = 6, dpi = 300, filename = "Sources_Total.png")
```

    ## Warning: The dot-dot notation (`..count..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(count)` instead.

``` r
ggsave(plot = p_age, width = 8, height = 6, dpi = 300, filename = "Sources_Age.png")
ggsave(plot = p_gender, width = 8, height = 6, dpi = 300, filename = "Sources_Gender.png")
ggsave(plot = p_group, width = 8, height = 6, dpi = 300, filename = "Sources_Group.png")
```
