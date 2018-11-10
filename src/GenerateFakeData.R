library(tidyverse)
library(readxl)
library(here)

setwd(here())

# Read in business details
biz_details = read_excel("data/raw/RVMS_Current_Property_and_BIZ_Owner_List - vCurrent (1).xlsx", 
                         sheet = "Biz & Prop Owner MAIN list")

# Create the business matrix by extracting the business names and initializing columns
biz_matrix = data_frame(biz_details$`Business Name`)
biz_matrix$Naics_Code = biz_details$`NAICS Code`
biz_matrix$Naics_3_digit = substr(biz_matrix$Naics_Code, 1, 3)
biz_matrix$Naics_2_digit = substr(biz_matrix$Naics_Code, 1, 2)
colnames(biz_matrix)[1] = c("Business_Name")

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
  aes(x = B2R_score, y = R2B_score) + 
  geom_point() +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Business to RVMS") +
  ylab("RVMS to Business") +
  scale_x_continuous(limits = c(0,6), expand = c(0, 0), labels = c(0,1,2,3,4,5,"")) +
  scale_y_continuous(limits = c(0,4), expand = c(0, 0), labels = c(0,1,2,3,"")) +
  geom_vline(xintercept = c( 1, 2, 3, 4, 5, 6)) +
  geom_hline(yintercept = c( 1, 2, 3, 4))
