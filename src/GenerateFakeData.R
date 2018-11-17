library(tidyverse)
library(readxl)
library(here)

setwd(here())

# Read in business details
biz_details = read_excel("data/raw/RVMS_Current_Property_and_BIZ_Owner_List - vCurrent (1).xlsx", 
                         sheet = "Biz & Prop Owner MAIN list")

# Read in NAICS
naics = read_excel("data/raw/2-6 digit_2017_Codes.xlsx") %>%
  select(`2017 NAICS US   Code`, `2017 NAICS US Title`)
colnames(naics) = c("Naics_Code", "name")
#naics$Naics_Code = as.double(naics$Naics_Code)

# Create the business matrix by extracting the business names and initializing columns
biz_matrix = biz_details %>%
  select(`Business Name`,`NAICS Code`) %>%
  filter(`Business Name` != "VACANT",
         ! is.na(`NAICS Code`))
colnames(biz_matrix) = c("Business_Name","Naics_Code")

# Create sub NAICS Codes
biz_matrix$Naics_3_digit = substr(biz_matrix$Naics_Code, 1, 3)
biz_matrix$Naics_2_digit = substr(biz_matrix$Naics_Code, 1, 2)

# Add in Naics names
biz_matrix = left_join(biz_matrix, naics, by=c("Naics_3_digit" = "Naics_Code"))

n_biz = nrow(biz_matrix)
prob = 0.4

biz_matrix$R2B_email_sponsorship_promotion = rbernoulli(n_biz, prob)
biz_matrix$R2B_offer_resources             = rbernoulli(n_biz, prob)
biz_matrix$R2B_liason                      = rbernoulli(n_biz, prob)
biz_matrix$B2R_event_participation         = rbernoulli(n_biz, prob)
biz_matrix$B2R_sponsorship_or_donation     = rbernoulli(n_biz, prob)
biz_matrix$B2R_share_business_info         = rbernoulli(n_biz, prob)
biz_matrix$B2R_volunteer                   = rbernoulli(n_biz, prob)
biz_matrix$B2R_use_rvms_resources          = rbernoulli(n_biz, prob)

biz_matrix = biz_matrix %>%
  mutate(R2B_score = R2B_email_sponsorship_promotion + R2B_offer_resources + R2B_liason,
         B2R_score = B2R_event_participation + B2R_sponsorship_or_donation + B2R_share_business_info + 
                     B2R_volunteer + B2R_use_rvms_resources)

# add noise to score for visual clarity
biz_matrix = biz_matrix %>%
  mutate(R2B_score = R2B_score + 0.5 + runif(n_biz, -0.25, 0.25),
         B2R_score = B2R_score + 0.5+ runif(n_biz, -0.25, 0.25))

biz_matrix %>%
  ggplot() +
  aes(x = B2R_score, y = R2B_score, color=Naics_2_digit) + 
  geom_point() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Business to RVMS") +
  ylab("RVMS to Business") +
  scale_x_continuous(limits = c(0,6), expand = c(0, 0), labels = c(0,1,2,3,4,5,"")) +
  scale_y_continuous(limits = c(0,4), expand = c(0, 0), labels = c(0,1,2,3,"")) +
  geom_vline(xintercept = c( 1, 2, 3, 4, 5, 6)) +
  geom_hline(yintercept = c( 1, 2, 3, 4))


biz_matrix %>%
  filter(! is.na(name)) %>%
  group_by(name) %>%
  summarize(counts = n()) %>%
  arrange(counts) %>%                                
  mutate(name = factor(name, name),
         freq = 100 * counts/sum(counts)) %>%
  ggplot() +
  aes(x = name, y = freq) +
  geom_bar(stat='identity',
           fill = "#dd8d50") + 
  coord_flip() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0), 
                                           add = c(0, 2))) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border = element_blank()) +
  labs(title = "Business Composition of Roslindale",
       subtitle = "Percentage; Sums to 100%",
       x = "3-digit NAICS Code") + 
  geom_text(aes(label=round(freq,0)), position=position_dodge(width=0.9), hjust=-0.25) 
  