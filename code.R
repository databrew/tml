library(gsheet)
library(dplyr)
library(ggplot2)

tml <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1dw4_r2Lkf_ZZnzD45YkBs8TByOSmKfVSUr4Z3Ufk2fU/edit#gid=1307382405')

# Remove those with missing information
tml <- tml %>%
  filter(!is.na(`Date of birth`),
         !is.na(`Vaccinated (yes/no)`))

# Create a binary covid variable
tml <- tml %>%
  mutate(covid = !is.na(`Positive covid test date(s)`)) %>%
  # and a prior infection covid variable
  mutate(prior_infection = !is.na(`Previous infection date`))

# Get odds of covid as a function of prior infection
fit <- glm(covid ~ prior_infection, family = binomial, data = tml)
exp(coef(fit))

# Chart covid by prior infection status
pd <- tml %>%
  group_by(covid, prior_infection) %>%
  tally %>%
  ungroup %>%
  group_by(prior_infection) %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup %>%
  mutate(prior_infection = ifelse(prior_infection,
                                  'Previously infected',
                                  'Not previously infected')) %>%
  mutate(covid = ifelse(covid, 'Infected at TML',
                        '_Not infected at TML'))

ggplot(data = pd, 
       aes(prior_infection,
           y = p,
           fill = covid)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(position = position_dodge(width = 0.8),
            aes(y = p + 5,
                label = paste0(round(p, digits = 2), '%'))) +
  labs(x = 'Previous infection status',
       y = 'Percentage',
       title = 'Attack rate by previous infection status',
       subtitle = 'Significantly higher attack rate among those not previously infected') +
  scale_fill_manual(name = '',
                    values = rev(c('red', 'black'))) +
  theme(legend.position = 'bottom')


# Get odds of covid as a function of prior infection
fit <- glm(covid ~ prior_infection, family = binomial, data = tml)
exp(coef(fit))

# Chart covid by vaccine doses
pd <- tml %>%
  mutate(vaccine = as.numeric(`Vaccine doses (integer)`)) %>%
  mutate(vaccine = ifelse(is.na(vaccine), 0, vaccine)) %>%
  group_by(covid, vaccine) %>%
  tally %>%
  ungroup %>%
  group_by(vaccine) %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup %>%
  mutate(covid = ifelse(covid, 'Infected at TML',
                        '_Not infected at TML'))

ggplot(data = pd, 
       aes(vaccine,
           y = p,
           fill = covid)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(position = position_dodge(width = 0.8),
            aes(y = p + 5,
                label = paste0(round(p, digits = 2), '%'))) +
  labs(x = 'Vaccine doses',
       y = 'Percentage',
       title = 'Attack rate by vaccine doses',
       subtitle = '(Confounded by age)') +
  scale_fill_manual(name = '',
                    values = rev(c('red', 'black'))) +
  theme(legend.position = 'bottom')
