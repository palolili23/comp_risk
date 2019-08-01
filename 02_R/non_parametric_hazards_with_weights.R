vect <- data_long %>% pull(cens_denom)

### Creates a count of events/total alive but adding up the weights of C

### First for exposure ==  1

# denominator
indices <- data_long %>% 
  filter(exposure == 1 & cens_plr == 0 & competing_plr == 0) %>% 
  group_by(time) %>% 
  summarize(indices = sum(cens_sw))

# numerator
event_indic <- data_long %>% 
  filter(exposure == 1 & cens_plr == 0 & competing_plr == 0 & outcome_plr == 1 ) %>% 
  group_by(time) %>% 
  summarize(event_indic = sum(cens_sw))

# hazards 
outputhazards1 <- indices %>% 
  left_join(event_indic) %>% 
  mutate(event_indic = ifelse(is.na(event_indic), 0, event_indic)) %>% 
  mutate(outputhazard = event_indic/indices)

### For exposure == 0 

indices <- data_long %>% 
  filter(exposure == 0 & cens_plr == 0 & competing_plr == 0) %>% 
  group_by(time) %>% 
  summarize(indices = sum(cens_sw))


event_indic <- data_long %>% 
  filter(exposure == 0 & cens_plr == 0 & competing_plr == 0 & outcome_plr == 1 ) %>% 
  group_by(time) %>% 
  summarize(event_indic = sum(cens_sw))

outputhazards0 <- indices %>% 
  left_join(event_indic) %>% 
  mutate(event_indic = ifelse(is.na(event_indic), 0, event_indic)) %>% 
  mutate(outputhazard = event_indic/indices)


