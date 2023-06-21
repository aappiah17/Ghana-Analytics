# survey_data <- read_sheet(ss="https://docs.google.com/spreadsheets/d/1ODiDU9dhgx0_TNol78SmYUlmjbnSivHrAZCbZWYTGeM/edit#gid=622674871") %>% 
#   clean_names()

# ggchicklet which I use for rounded bar graphs is not on CRAN so needs to be installed with: 
install.packages("ggchicklet", repos="https://cinc.rud.is")

# Load packages - assuming these packages are installed, if not need to install 
suppressPackageStartupMessages({
  library(janitor)
  library(readxl)
  library(tidyverse)
  library(RColorBrewer)
  require(scales)
  library(zoo)
  library(gt)
  library(googlesheets4)
  library(glue)
  library(ggtext)
  library(systemfonts)
  library(extrafont)
  library(stringr)
  library(patchwork)
  library(webshot)
  library(ggchicklet)
  #library(ggstream)
  library(gtExtras)
  #library(ggpattern)
})


# My custom theme 

AAtheme_patch<-theme(plot.title=element_markdown(face="bold", size=22,family="Arial",hjust=0.5, color = "grey30"),
                     plot.subtitle=element_markdown(size=15, color="grey55",family="Arial", face="bold", hjust = 0.5),
                     axis.text.y = element_text(size=13, color="grey35",family="Arial",face = "bold"),
                     axis.title.y = element_text(size=13, color="grey35",family="Arial",face = "bold"),
                     axis.title.x = element_text(size=13, color="grey35",family="Arial",face = "bold"),
                     axis.text.x = element_text(size=13, color="grey35",family="Arial",face = "bold"),
                     #axis.line.x = element_line( size = 1.2, linetype = "solid"),
                     axis.ticks.y=element_blank(),
                     axis.ticks.x=element_blank(),
                     # plot.background = element_rect(fill = "grey95", color = "transparent"),
                     plot.background = element_rect(fill = "floralwhite",color = NA),
                     # panel.border = element_blank(),
                     panel.grid.major.y = element_blank(), 
                     panel.grid.major.x = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     legend.text=element_blank(),
                     legend.title = element_blank(),
                     legend.background =element_blank(),
                     legend.position="none",
                     legend.direction = "horizontal",
                     plot.caption = element_markdown(size = 15, face = "bold",family="Arial",color = "grey55"))

# Read the data - assumes you have the raw csv saved in folder called data in your current wd
survey_data <- read_csv("data/raw_data.csv") %>% 
  clean_names()
# for time stamping vizzes - this was when I was pulling the live google sheet so not that relevant now
time <- format(as.POSIXct(Sys.time(), tz = "GMT"), "%Y-%m-%d %H:%M:%S %Z")


survey_data_analytical <- survey_data %>% 
  # Exclude non-Ghanaian respondents
  filter(nationality=="Ghanaian") %>% 
  rename(monthly_income=what_is_the_range_of_your_take_home_monthly_income_including_all_allowances_and_side_endeavours,
         months_spending_saved=how_many_months_worth_of_expenses_do_you_have_saved_up,
         amount_saved=including_your_retirement_benefits_but_excluding_any_physical_assets_how_much_do_you_have_saved_up) %>% 
  mutate(monthly_income_group=case_when(monthly_income %in% c("GH¢1,000 and below","GH¢1,001 - GH¢2,499","GH¢2,500 - GH¢4,999")~"Less than GH¢5,000",
                                        monthly_income %in% c("GH¢5,000 - GH¢7,499","GH¢7,500 - GH¢9,999")~"GH¢5,000 - GH¢9,999",
                                        monthly_income %in% c("GH¢10,000 - GH¢14,999","GH¢15,000 - GH¢19,999")~"GH¢10,000 - GH¢19,999",
                                        TRUE~"GH¢20,000 and above"),
         monthly_income=factor(monthly_income,levels = c("GH¢1,000 and below","GH¢1,001 - GH¢2,499","GH¢2,500 - GH¢4,999","GH¢5,000 - GH¢7,499","GH¢7,500 - GH¢9,999",
                                                         "GH¢10,000 - GH¢14,999","GH¢15,000 - GH¢19,999","GH¢20,000 - GH¢29,999","GH¢30,000 - GH¢49,999","GH¢50,000 and above"  
                               )),
         months_spending_saved=factor(months_spending_saved,levels = c("Zero months","1 - 3 months","4 - 7 months","8 - 12 months","More than 12 months")),
         monthly_income_group=factor(monthly_income_group,levels = c("Less than GH¢5,000","GH¢5,000 - GH¢9,999","GH¢10,000 - GH¢19,999","GH¢20,000 and above")),
         number_of_dependents=factor(number_of_dependents,levels = c("None","1-3","4-7","More than 7"),labels = c("None","1-3","4-7","More than 7")),
         highest_educational_level=factor(highest_educational_level,levels = c("No Formal Education","Senior High School and Below","Professional Certification Only","Graduate",
                                                                               "Post-Graduate"),labels =c("No Formal Education","Senior High School and Below","Professional Certification Only","Graduate",
                                                                                                          "Post-Graduate")),
         amount_saved_group=case_when(amount_saved %in% c("GH¢1,000,000 - GH¢2,999,999","GH¢3,000,000 - GH¢9,999,999",
                                                          "GH¢500,000 - GH¢999,999","GH¢10,000,000 and above","GH¢200,000 - GH¢499,999")~"GH¢200,000 and above",
                                      TRUE~amount_saved),
         amount_saved_group=factor(amount_saved_group,levels = c("GH¢10,000 or less","GH¢10,001 - GH¢29,999","GH¢30,000 - GH¢49,999","GH¢50,000 - GH¢99,999",
                                                                 "GH¢100,000 - GH¢199,999","GH¢200,000 and above")))
# 
# sanity_check <- survey_data_analytical %>% 
#   count(amount_saved,amount_saved_group)


# Define a function for summarizing demographic data 
sum_demo <- function(df,var,label){df %>% 
  group_by({{var}}) %>% 
  tally() %>% 
  mutate(prop=n/sum(n),
         group=label) %>% 
    rename(sub_group={{var}})%>% 
    mutate(sub_group=ifelse(n>=10,as.character(sub_group),"Other"),
           sub_group=as.factor(sub_group)
          ) %>% 
    # Exclude cell sizes less than 10
    filter(n>=10)
}

# Make a table of demographics
sum_demo(survey_data_analytical,age,"Age") %>% 
  bind_rows(
    sum_demo(survey_data_analytical,gender,"Sex"),
    sum_demo(survey_data_analytical,marital_status,"Marital Status"),
    sum_demo(survey_data_analytical,number_of_dependents,"Number of Dependents"),
    sum_demo(survey_data_analytical,highest_educational_level,"Highest Level of Education"),
    sum_demo(survey_data_analytical,which_region_of_ghana_are_you_based_in,"Region"),
    
  ) %>% 
  # group_by(sub_group,group) %>% 
  # summarise(across(c(n,prop),~sum(.))) %>% 
  #select(-n) %>% 
  #mutate(group=factor(group,levels = c("Age","Gender","Marital Status","Number of Dependents","Highest Level of Education","Region"))) %>% 
  gt(rowname_col = "sub_group", groupname_col = "group") %>% 
  row_group_order(groups = c("Age","Sex","Marital Status","Number of Dependents","Highest Level of Education","Region")) %>% 
  fmt_percent(c(prop),decimals = 1,sep_mark = ',') %>% 
  fmt_number(c(n),decimals = 0,sep_mark = ',') %>% 
  cols_label(n="Number of Respondents",
             prop= "Percentage") %>%
  cols_align(c(n,prop),align = "center") %>% 
  gt_theme_538() %>% 
  tab_options(
    heading.align = 'left',
    heading.background.color = "#E3B778",
    heading.title.font.size = px(25)
  ) %>% 
  tab_style(
    style = list(
      cell_fill("#FFE5B4"),
      cell_text(color = "grey25", weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_style(
    style = list(
      cell_text(align = "left")
    ),
    locations = cells_stub(rows = TRUE)
  ) %>% 
  tab_header(
    title = 'Demographic Profile of Respondents',
    subtitle = glue::glue('Data: Earnings Survey initiated by @readjerome, as of {time}')
  ) %>% 
  tab_source_note(
    source_note = md(
      "Response categories with less than 10 responses suppressed or combined into Other where applicable<br>
        Analysis and viz by @CallmeAlfredo"
    )
  ) %>% 
  gtsave("Charts/demo_tab.png")

# Make a table of income, months of savings and demographics 

# Define a function for the summary

summary_outcome_demo <- function(df,demo,outcome1,outcome2,label){
  df %>% 
    group_by(demo_group={{demo}},outcome_group={{outcome1}}) %>% 
    tally() %>% 
    mutate(prop=n/sum(n)) %>% 
    bind_rows(df %>% 
                group_by(demo_group={{demo}},outcome_group={{outcome2}}) %>% 
                tally() %>% 
                mutate(prop=n/sum(n))
    ) %>% select(-n) %>% 
    mutate(group=label)
}

summary_table <- 
summary_outcome_demo(survey_data_analytical %>% 
                       mutate(age_group=ifelse(age %in% c("36 - 45","46 - 59","60 and above"),"35 and above",age)),
                     age_group,monthly_income_group,amount_saved_group,"Age") %>% 
  bind_rows(
    summary_outcome_demo(survey_data_analytical %>% 
                           filter(gender %in% c("Female","Male")),
                         gender,monthly_income_group,amount_saved_group,"Sex"),
    summary_outcome_demo(survey_data_analytical %>% 
                           mutate(marital_status=ifelse(marital_status=="Married","Married","Unmarried")),
                         marital_status,monthly_income_group,amount_saved_group,"Marital Status"),
    summary_outcome_demo(survey_data_analytical %>% 
                           mutate(number_of_dependents=ifelse(number_of_dependents %in% c("4-7","More than 7"),"4 and above",as.character(number_of_dependents)),
                                  number_of_dependents=factor(number_of_dependents,levels = c("None","1-3","4 and above"))),
                                  number_of_dependents,monthly_income_group,amount_saved_group,"Number of Dependents"),
    summary_outcome_demo(survey_data_analytical %>% filter(highest_educational_level!="No Formal Education"),
                           highest_educational_level,monthly_income_group,amount_saved_group,"Highest Level of Education")
    
  ) 

# The following could be made into a function if we were going to do multiple cross tabs   
# Income by demo
summary_table %>% 
    filter(outcome_group %in% c("Less than GH¢5,000","GH¢5,000 - GH¢9,999","GH¢10,000 - GH¢19,999","GH¢20,000 and above")) %>% 
  pivot_wider(names_from = outcome_group,values_from = prop) %>% 
  gt(rowname_col = "demo_group", groupname_col = "group") %>% 
  #row_group_order(groups = c("Age","Sex","Marital Status","Number of Dependents","Highest Level of Education","Region")) %>% 
  fmt_percent(c(`Less than GH¢5,000`:`GH¢20,000 and above`),decimals = 1,sep_mark = ',') %>% 
  tab_spanner(label = md("**Monthly Earnings**"),
              columns = c(`Less than GH¢5,000`:`GH¢20,000 and above`)) %>% 
  # tab_spanner(label = md("**Amount of Savings**"),
  #             columns = c(`GH¢10,000 or less`:`GH¢200,000 and above`)) %>% 
  #cols_align(c(n,prop),align = "center") %>% 
  gt_theme_538() %>% 
  sub_missing(columns = everything(),rows = everything(),
              missing_text = "-") %>% 
  tab_options(
    heading.align = 'left',
    heading.background.color = "#E3B778",
    heading.title.font.size = px(25)
  ) %>% 
  tab_style(
    style = list(
      cell_fill("#FFE5B4"),
      cell_text(color = "grey25", weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_style(
    style = list(
      cell_text(align = "left")
    ),
    locations = cells_stub(rows = TRUE)
  ) %>% 
  tab_header(
    title = 'Monthly Income by Demographics',
    subtitle = glue::glue('Data: Earnings Survey initiated by @readjerome, as of {time}')
  ) %>% 
  tab_source_note(
    source_note = md(
      "Cell sizes with less than 10 responses are removed or combined with other groups<br>
        Analysis and viz by @CallmeAlfredo"
    )
  )%>% 
  gtsave("Charts/demo_income_tab.png",vwidth=670)

# Amount of savings by demo
summary_table %>% 
  filter(outcome_group %in% c("GH¢10,000 or less","GH¢10,001 - GH¢29,999","GH¢30,000 - GH¢49,999","GH¢50,000 - GH¢99,999",
                              "GH¢100,000 - GH¢199,999","GH¢200,000 and above")) %>% 
  pivot_wider(names_from = outcome_group,values_from = prop) %>% 
  gt(rowname_col = "demo_group", groupname_col = "group") %>% 
  #row_group_order(groups = c("Age","Sex","Marital Status","Number of Dependents","Highest Level of Education","Region")) %>% 
  fmt_percent(c(`GH¢10,000 or less`:`GH¢200,000 and above`),decimals = 1,sep_mark = ',') %>% 
  # tab_spanner(label = md("**Monthly Earnings**"),
  #             columns = c(`Less than GH¢5,000`:`GH¢20,000 and above`)) %>% 
  tab_spanner(label = md("**Amount of Savings**"),
              columns = c(`GH¢10,000 or less`:`GH¢200,000 and above`)) %>%
#  cols_align(c(n,prop),align = "center") %>%
  gt_theme_538() %>% 
  sub_missing(columns = everything(),rows = everything(),
              missing_text = "-") %>% 
  tab_options(
    heading.align = 'left',
    heading.background.color = "#E3B778",
    heading.title.font.size = px(25)
  ) %>% 
  tab_style(
    style = list(
      cell_fill("#FFE5B4"),
      cell_text(color = "grey25", weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>% 
  tab_style(
    style = list(
      cell_text(align = "left")
    ),
    locations = cells_stub(rows = TRUE)
  ) %>% 
  tab_header(
    title = 'Amount of Savings by Demographics',
    subtitle = glue::glue('Data: Earnings Survey initiated by @readjerome, as of {time}')
  ) %>% 
  tab_source_note(
    source_note = md(
      "Cell sizes with less than 10 responses are removed or combined with other groups<br>
        Analysis and viz by @CallmeAlfredo"
    )
  )%>% 
  gtsave("Charts/savings_tab.png",vwidth=670)

# survey_data_analytical %>%
#   count(type_of_employment)

# Income 
survey_data_analytical %>% 
  group_by(monthly_income) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=fct_reorder(monthly_income,desc(monthly_income)), y=prop))+
  geom_point(color="#990000", size=10)+
  geom_segment(aes(x=monthly_income, xend=monthly_income, 
                   y=0, yend=prop), color = "#990000", 
               size = 2)+
  coord_flip(clip = "off", expand = F)+
  geom_text(aes(label=glue::glue("{round(prop*100,0)}%")), fontface="bold", color="white", size=4)+
  AAtheme_patch+
  expand_limits(y=c(0,.35))+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        legend.background =element_blank(),
        strip.text = element_text(face="bold", size=17,family="Gadugi"),
        legend.position="top",
        legend.direction = "horizontal")+
  labs(
       title = "The majority of respondents earn below 5K cedis a month", subtitle =glue("Data: Earnings Survey initiated by @readjerome, as of {time}<br>"),
       caption = "Analysis and viz by @CallmeAlfredo") 
ggsave("Charts/overall_income.png", width = 10.5, height = 6.5, dpi=800)


# Define a function for plotting groups of data 

plot_two_variables <- function(df,var1,var2,label=NULL,sub_label=NULL,x_label=NULL,y_label=NULL) {df %>% 
  filter(!is.na({{var1}}),!is.na({{var2}})) %>% 
  group_by({{var1}},{{var2}}) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=fct_rev({{var1}}),y=prop,fill=fct_rev({{var2}})))+
  geom_chicklet(position = "fill",width = .8,alpha=.8)+
  coord_flip(expand = F)+
  AAtheme_patch+
  theme(axis.title.y = element_text(size=13, color="grey35",family="Arial",face = "bold"),
          axis.title.x = element_text(size=13, color="grey35",family="Arial",face = "bold"),
        axis.text.x = element_blank(),
        legend.text=element_text(size=13),
        legend.title = element_blank(),
        legend.background =element_blank(),
        legend.key = element_rect(fill = "floralwhite"),
        legend.position="top",
        legend.direction = "horizontal")+
  scale_fill_brewer(palette = "Paired")+
  guides(fill = guide_legend(reverse = T))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent)+
  geom_text(aes(label=glue::glue("{round(prop*100,0)}%")), fontface="bold", color="white", size=4.5, position=position_stack(vjust=0.5))+
  labs(title = label,x=x_label,y=y_label,
       subtitle = glue(sub_label),
       caption = "Analysis and viz by @CallmeAlfredo"
  )}

# Private v public by income
plot_two_variables(
  survey_data_analytical,
  are_you_in_the_public_or_private_sector,
  monthly_income_group,
  label="Respondents employed in the private sector appear to earn more on a monthly basis than the public sector",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>"
)
ggsave("Charts/public_private_income.png", width = 17.5, height = 6, dpi=800)

# Calculate population of people working remotely for abroad companies for use in chart beloe
abroad_prop <- survey_data_analytical %>% 
  filter(!is.na(is_your_income_mainly_from_a_company_business_based_in_ghana_or_abroad)) %>% 
  group_by(is_your_income_mainly_from_a_company_business_based_in_ghana_or_abroad) %>% 
  tally() %>% 
  mutate(prop=round(n*100/sum(n),0)) %>% 
  filter(is_your_income_mainly_from_a_company_business_based_in_ghana_or_abroad=="Abroad") %>% 
  pull(prop)

# Remote v Gh-based company and income

plot_two_variables(
  survey_data_analytical,
  is_your_income_mainly_from_a_company_business_based_in_ghana_or_abroad,
  monthly_income_group,
  label="Respondents working for non-Ghanaian based companies earn the big bucks!",
  sub_label = "However, those respondents account for only {abroad_prop}% of the sample<br>"
)

ggsave("Charts/abroad_ghana_income.png", width = 15.5, height = 6, dpi=800)

# Employment type/status v income
plot_two_variables(
  survey_data_analytical,
  type_of_employment,
  monthly_income_group,
  label="Employment Status/Type and Income",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>"
)

ggsave("Charts/employment_type_income.png", width = 14, height = 9, dpi=800)
# p1/p2
# ggsave("Charts/income_summary_group.png", width = 17, height = 12.5, dpi=800)

# Savings - months of savings
survey_data_analytical %>% 
  group_by(months_spending_saved) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=fct_rev(months_spending_saved),y=prop))+
  geom_chicklet(width = .8,fill="#990000")+
  coord_flip(expand = F)+
  AAtheme_patch+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        legend.background =element_blank(),
        strip.text = element_text(face="bold", size=17,family="Gadugi"),
        legend.position="none",
        legend.direction = "horizontal")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent)+
  geom_text(aes(label=glue::glue("{round(prop*100,0)}%")), fontface="bold", color="white", size=6, position=position_stack(vjust=0.5))+
  labs(title = "Most respondents have less than 4 months of expenses saved up",
       subtitle = glue("Data: Earnings Survey initiated by @readjerome, as of {time}<br>"),
       caption = "Analysis and viz by @CallmeAlfredo"
  )
ggsave("Charts/savings_overall.png", width = 11.5, height = 7.5, dpi=800)

# Value of savings
survey_data_analytical %>% 
  group_by(amount_saved_group) %>% 
  tally() %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=fct_rev(amount_saved_group),y=prop))+
  geom_chicklet(width = .8,fill="#990000")+
  coord_flip(expand = F)+
  AAtheme_patch+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        legend.background =element_blank(),
        strip.text = element_text(face="bold", size=17,family="Gadugi"),
        legend.position="none",
        legend.direction = "horizontal")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent)+
  geom_text(aes(label=glue::glue("{round(prop*100,0)}%")), fontface="bold", color="white", size=6, position=position_stack(vjust=0.5))+
  labs(title = "Over half of respondents have 10k or less saved up",
       subtitle = glue("Data: Earnings Survey initiated by @readjerome, as of {time}<br>"),
       caption = "Analysis and viz by @CallmeAlfredo"
  )
ggsave("Charts/savings_amount_overall.png", width = 11.5, height = 7.5, dpi=800)


# Months of savings by income group 
plot_two_variables(
  survey_data_analytical,
  monthly_income_group,
  months_spending_saved,
  label="Respondents who earn lower also have the least amount of monthly expenses saved up",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Monthly Income"
)
ggsave("Charts/savings_by_income.png", width = 15.5, height = 9, dpi=800)

# Amount of savings by income group 
plot_two_variables(
  survey_data_analytical,
  monthly_income_group,
  amount_saved_group,
  label="Respondents who earn lower also have the least amounts saved up",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Monthly Income"
)
ggsave("Charts/savings_amount_by_income.png", width = 15.5, height = 9, dpi=800)

plot_two_variables(
  survey_data_analytical,
  type_of_employment,
  months_spending_saved,
  label="Employment Status/Type and Months of Savings",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>"
)

ggsave("Charts/savings_by_employment.png", width = 15.5, height = 9, dpi=800)

# Monthly income and car ownership
plot_two_variables(
  survey_data_analytical %>% 
    mutate(car_ownership=ifelse(do_you_own_a_car=="Yes","Own a car","Doesn't own a car")),
  monthly_income_group,
  car_ownership,
  label="As one would expect, owning a car is for the rich!",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>"
)
ggsave("Charts/car_ownership_income.png", width = 14, height = 9, dpi=800)

# Months of savings and car ownership
plot_two_variables(
  survey_data_analytical %>% 
    mutate(car_ownership=ifelse(do_you_own_a_car=="Yes","Own a car","Doesn't own a car")),
  months_spending_saved,
  car_ownership,
  label="Months of Savings and Car Ownership",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Number of Months of Savings"
)
ggsave("Charts/car_ownership_savings.png", width = 14, height = 9, dpi=800)

# Amount of savings and car ownership
plot_two_variables(
  survey_data_analytical %>% 
    mutate(car_ownership=ifelse(do_you_own_a_car=="Yes","Own a car","Doesn't own a car")),
  amount_saved_group,
  car_ownership,
  label="Amount of Savings and Car Ownership",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Value of Savings"
)
ggsave("Charts/car_ownership_savings_amount.png", width = 14, height = 9, dpi=800)


plot_two_variables(
  survey_data_analytical %>% 
    filter(age!="60 and above") %>% 
    mutate(car_ownership=ifelse(do_you_own_a_car=="Yes","Own a car","Doesn't own a car")),
  age,
  car_ownership,
  label="Age and Car Ownership",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Age"
)
ggsave("Charts/car_ownership_age.png", width = 14, height = 9, dpi=800)

# Monthly income and home ownership

plot_two_variables(
  survey_data_analytical %>% 
    mutate(home_ownership=ifelse(do_you_own_a_home=="Yes","Owns a home","Doesn't own a home")),
  monthly_income_group,
  home_ownership,
  label="Home Ownership by Income Group",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>"
)
ggsave("Charts/home_ownership_income.png", width = 14, height = 9, dpi=800)

# Monthly income and education
plot_two_variables(
  survey_data_analytical %>% 
    filter(highest_educational_level!="No Formal Education"),
  highest_educational_level,
  monthly_income_group,
  label="Monthly Income by Highest Level of Education",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>"
)
ggsave("Charts/education_income.png", width = 14, height = 9, dpi=800)

# Age and savings 
plot_two_variables(
  survey_data_analytical %>% 
    filter(age!="60 and above"),
  age,
  amount_saved_group,
  label="Age and Amount of Savings",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Age"
)
ggsave("Charts/savings_age.png", width = 14, height = 9, dpi=800)

# Sex and savings 
plot_two_variables(
  survey_data_analytical %>% 
    filter(gender %in% c("Female","Male")),
  gender,
  amount_saved_group,
  label="Sex and Amount of Savings",
  sub_label = "Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Sex"
)
ggsave("Charts/savings_sex.png", width = 12, height = 5, dpi=800)

# Earnings and Income by Industry

survey_data_analytical %>% 
  count(what_industry_do_you_work_in) %>% 
  mutate(industy_group=ifelse(n<15,"Other",what_industry_do_you_work_in)) %>% 
  group_by(industy_group) %>% 
  summarise(n=sum(n)) %>% 
  mutate(prop=n/sum(n)) %>% 
  ggplot(aes(x=reorder(str_wrap(industy_group,40),prop),y=prop))+
  geom_chicklet(width = .8,fill="#990000")+
  coord_flip(expand = F)+
  AAtheme_patch+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        legend.background =element_blank(),
        strip.text = element_text(face="bold", size=17,family="Gadugi"),
        legend.position="none",
        legend.direction = "horizontal")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), labels = percent)+
  geom_text(aes(label=glue::glue("{round(prop*100,0)}%")), fontface="bold", color="white", size=6, position=position_stack(vjust=0.5))+
  labs(title = "Which industries do respondents work in?",
       subtitle = glue("Data: Earnings Survey initiated by @readjerome, as of {time}<br>"),
       caption = "Analysis and viz by @CallmeAlfredo"
  )
ggsave("Charts/industry_overall.png", width = 13.5, height = 8, dpi=800)
  

industry <- survey_data_analytical %>% 
  count(what_industry_do_you_work_in) %>% 
  filter(n>50)

plot_two_variables(
  survey_data_analytical %>% 
    filter(what_industry_do_you_work_in %in% industry$what_industry_do_you_work_in),
  what_industry_do_you_work_in,
  monthly_income_group,
  label="Monthly Income by Top Industries of Employment",
  sub_label = "Industries with at least 50 responses shown. <br>Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Industry"
) 
ggsave("Charts/income_industry.png", width = 15.5, height = 10, dpi=800)

plot_two_variables(
  survey_data_analytical %>% 
    filter(what_industry_do_you_work_in %in% industry$what_industry_do_you_work_in),
  what_industry_do_you_work_in,
  amount_saved_group,
  label="Amount of Savings by Top Industries of Employment",
  sub_label = "Industries with at least 50 responses shown. <br>Data: Earnings Survey initiated by @readjerome, as of {time}<br>",
  x_label = "Industry"
)

ggsave("Charts/savings_industry.png", width = 15.5, height = 10, dpi=800)
