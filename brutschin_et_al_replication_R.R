#paper replication
#data: June, 30, 2024


# to generate the dataset from the original survey data please refer to "main_cleaning_17_10_2023")

#Packages
library(tidyverse)
library(haven)
library(here)
library(labelled)
library(likert)
library(ggpmisc)
library(WDI)
library(countrycode)
library(cowplot)
library(hrbrthemes)
library(gridExtra)
library(grid)
library(patchwork)

#import data
survey_clean2<-read_dta("survey_clean2_2024062024.dta")

##################
#Figure 2 income
#################


income_country<-survey_clean2 %>%
 # left_join(lending)%>%
  select( record, dCountry,technology, broader_deployment, income) %>%
  mutate(broader_deployment = case_when(
    broader_deployment== 1 ~ "Strictly reject",
    broader_deployment== 2 ~ "Somewhat reject",
    broader_deployment== 3 ~ "Neither reject nor support",
    broader_deployment== 4 ~ "Somewhat support",
    broader_deployment== 5 ~ "Fully support")) %>%
  mutate(broader_deployment=factor(broader_deployment, levels=c("Strictly reject", "Somewhat reject","Neither reject nor support","Somewhat support","Fully support"))) %>%
  mutate(income = case_when(
    income== "Lower middle income" ~ "M",
    income== "Upper middle income" ~ "M",
    income== "High income" ~ "H")) 

color <- c( "Strictly reject"="red3","Somewhat reject"="orangered2","Neither reject nor support"="#aaaaaa","Somewhat support"="#6baed6","Fully support"="#2171b5")

income_srm3<-income_country %>%
  drop_na() %>%
  mutate(technology = case_when(
    technology== "Space-based geoengineering" ~ "SBG",
    technology== "Stratospheric aerosol injection" ~ "SAI",
    technology== "Marine cloud brightening" ~ "MCB",
    technology== "Enhanced weathering" ~ "EW",
    technology== "Direct air capture with carbon storage" ~ "DACCS",
    technology== "Bioenergy with carbon capture and storage" ~ "BECCS",
    technology== "Biochar" ~ "Biochar",
    technology== "Marine biomass and blue carbon" ~ "MBBC",
    technology== "Soil carbon sequestration" ~ "SCS",
    technology==  "Afforestation and reforestation" ~ "AF")) %>%
  mutate(technology=factor(technology,c("SBG","SAI", "MCB","EW", "DACCS", "BECCS", "Biochar","MBBC","SCS", "AF"))) %>%
  # filter(technology%in%srm)%>%
  mutate(broader_deployment=factor(broader_deployment, levels=c("Strictly reject", "Somewhat reject","Neither reject nor support","Somewhat support","Fully support"))) %>%
  mutate(income=factor(income, levels=c("M", "H"))) %>%
  ggplot(aes(fill=broader_deployment, y=dCountry, x=income)) + 
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values=color)+
  geom_hline(yintercept = 0.5)+
  theme_ipsum()+
  theme(legend.position="bottom",
        legend.title=element_blank(),axis.text.x = element_text(angle =90))+
  facet_wrap(~technology, nrow=1)+
  xlab("")+ylab("Ratio of respondents (broader deployment)")
income_srm3

ggsave("Figure2_income.jpeg", units="in", width=12, height=5, dpi=300)

#add exact statistics

library(kableExtra)

results <- survey_clean2 %>%
  left_join(lending)%>%
  mutate(income = case_when(
    income== "Lower middle income" ~ "M",
    income== "Upper middle income" ~ "M",
    income== "High income" ~ "H")) %>%
  group_by(income, technology) %>%
  summarize(
    Mean = mean(broader_deployment, na.rm = TRUE),
    SD = sd(broader_deployment, na.rm = TRUE)
  ) %>%
  ungroup()

results <- results %>%
  mutate(Mean = round(Mean, 2),
         SD = round(SD, 2))


# Create a new column combining mean and SD values
results <- results %>%
  mutate(Mean_SD = paste0(Mean, " (", SD, ")")) %>%
  select(-Mean, -SD) %>%
  ungroup()

# Pivot the results to wide format
wide_results <- results %>%
  group_by(income) %>%
  pivot_wider(
    names_from = technology,
    values_from = Mean_SD
  )

# Print the wide format results in HTML format
kable(wide_results, format = "html", table.attr = "style='border-collapse: collapse;'", escape = FALSE) %>%
  kable_styling(full_width = FALSE)

#to check for number of observations for each technology:

unique_records <- survey_clean2 %>%
  distinct(record, technology, .keep_all = TRUE) %>%
  drop_na(broader_deployment)  

# Calculate the number of observations for each technology (without duplicates)
obs_table <- unique_records %>%
  #  group_by(technology) %>%
  summarize(n = n()) %>%
  ungroup()

# Print the observation table in HTML format
kable(obs_table, format = "html", table.attr = "style='border-collapse: collapse;'", escape = FALSE) %>%
  kable_styling(full_width = FALSE)

###############################
#Figure 3B - Table
###############################


wdi<-WDI(country = "all",
         indicator = c(
           'age'='SP.POP.65UP.TO.ZS', 
           'gdp_pc'='NY.GDP.PCAP.CD', 
           'child_mortality'='SH.DYN.MORT', 
           'literacy'='SE.ADT.LITR.ZS', 
           'gdp_capita'='NY.GDP.PCAP.PP.CD'),
         start = 2015,end = 2022,extra = TRUE)%>%
  as_tibble() 


lending<-wdi  %>%
  filter(year==2020) %>%
  select(iso3c, income) %>%
  rename(iso=iso3c)

socio<-wdi  %>%
  filter(year==2021) %>%
  select(iso3c, age, gdp_capita, literacy) %>%
  rename(iso=iso3c)


#harm 
climate_harm_d<-survey_clean2%>%
  select(country, climate_harm, record) %>%
  mutate(climate_harm=as.factor(climate_harm)) %>%
  mutate(
    climate_harm = case_when(
      climate_harm== "1" ~ "Not at all",
      climate_harm== "2" ~ "A little bit",
      climate_harm== "3" ~ "Somewhat",
      climate_harm== "4" ~ "A great deal"
    )) %>%
  mutate(climate_harm=as.factor(climate_harm)) %>%
  mutate(climate_harm=factor(climate_harm, levels=c("Not at all","A little bit","Somewhat","A great deal"))) %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  # select(-c(record, record_ID)) %>%
  pivot_wider(names_from = country, values_from =climate_harm) %>%
  select(-c(record, record_ID)) 


climate_harm_d<-as.data.frame(climate_harm_d)
climate_harm_d2<-likert(climate_harm_d)
harm_sum<-summary(climate_harm_d2) %>%
  rename(country=Item, harm_high=high) %>%
  select(country, harm_high)%>%
  mutate(iso=countrycode(country, "country.name", "iso3c")) %>%
  select(harm_high, iso)

#nature
nature_d<-survey_clean2%>%
  select(country, nature_no_meddle, record) %>%
  mutate(nature_no_meddle=as.factor(nature_no_meddle)) %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  # select(-c(record, record_ID)) %>%
  pivot_wider(names_from = country, values_from =nature_no_meddle) %>%
  select(-c(record, record_ID)) 


nature_d<-as.data.frame(nature_d)
nature_d2<-likert(nature_d)
nature_sum<-summary(nature_d2) %>%
  rename(country=Item, nature_high=high) %>%
  select(country, nature_high)%>%
  mutate(iso=countrycode(country, "country.name", "iso3c")) %>%
  select(nature_high, iso)


#environment_concern

#Need to adjust environment variable in the main file!

environment_d<-survey_clean2%>%
  select(country, environment_concern, record) %>%
  mutate(environment_concern=as.factor(environment_concern)) %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  # select(-c(record, record_ID)) %>%
  pivot_wider(names_from = country, values_from =environment_concern) %>%
  select(-c(record, record_ID)) 


environment_d<-as.data.frame(environment_d)
environment_d2<-likert(environment_d)
environment_sum<-summary(environment_d2) %>%
  rename(country=Item, environment_high=high) %>%
  select(country, environment_high)%>%
  mutate(iso=countrycode(country, "country.name", "iso3c")) %>%
  select(environment_high, iso)


#Science

science_d<-survey_clean2%>%
  select(country, science_belief, record) %>%
  mutate(science_belief=as.factor(science_belief)) %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  # select(-c(record, record_ID)) %>%
  pivot_wider(names_from = country, values_from =science_belief) %>%
  select(-c(record, record_ID)) 


science_d<-as.data.frame(science_d)
science_d2<-likert(science_d)
science_sum<-summary(science_d2) %>%
  rename(country=Item, science_high=high) %>%
  select(country, science_high)%>%
  mutate(iso=countrycode(country, "country.name", "iso3c")) %>%
  select(iso, science_high)

#trust_industry

trusti_d<-survey_clean2%>%
  select(country, trust_industry, record) %>%
  mutate(trust_industry=as.factor(trust_industry)) %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  # select(-c(record, record_ID)) %>%
  pivot_wider(names_from = country, values_from =trust_industry) %>%
  select(-c(record, record_ID)) 


trusti_d<-as.data.frame(trusti_d)
trusti_d2<-likert(trusti_d)
trusti_sum<-summary(trusti_d2) %>%
  rename(country=Item, trusti_high=high) %>%
  select(country, trusti_high)%>%
  mutate(iso=countrycode(country, "country.name", "iso3c")) %>%
  select(trusti_high, iso)

#trust national government

trustn_d<-survey_clean2%>%
  select(country, trust_nat_gov, record) %>%
  mutate(trust_nat_gov=as.factor(trust_nat_gov)) %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  # select(-c(record, record_ID)) %>%
  pivot_wider(names_from = country, values_from =trust_nat_gov) %>%
  select(-c(record, record_ID)) 


trustn_d<-as.data.frame(trustn_d)
trustn_d2<-likert(trustn_d)
trustn_sum<-summary(trustn_d2) %>%
  rename(country=Item, trustn_high=high) %>%
  select(country, trustn_high)%>%
  mutate(iso=countrycode(country, "country.name", "iso3c")) %>%
  select(trustn_high, iso)

lending<-wdi  %>%
  filter(year==2020) %>%
  select(iso3c, income) %>%
  rename(iso=iso3c)


full_beliefs<-science_sum%>%
  left_join(nature_sum) %>%
  left_join(trusti_sum) %>%
  left_join(trustn_sum) %>%
  left_join(environment_sum) %>%
  left_join(harm_sum) %>%
  left_join(lending) %>%
  left_join(socio) %>%
  arrange(income)

desired_order <- c("High income", "Upper middle income", "Lower middle income")



age_d<-survey_clean2%>%
  select(country, age, record) %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  ungroup() %>%
  group_by(country)%>%
  summarise(median(age))


full_beliefs<-full_beliefs %>%
  mutate(income = factor(income, levels = desired_order)) %>%
  arrange(income) %>%
  mutate(country=countrycode(iso, "iso3c", "country.name")) 
  

write.csv(full_beliefs, "overview_beliefs_main_paper.csv")


###############################
#Figure 3A - PCA
###############################

library("FactoMineR")
library("factoextra")
library(ggrepel)

full_beliefs<-full_beliefs %>%
              rename(environment=environment_high, harm=harm_high, nature=nature_high, science=science_high, trustn=trustn_high, trusti=trusti_high)

row.names(full_beliefs) <- full_beliefs$iso

data.pca<-full_beliefs%>%
  select(-literacy, -iso, -income, -country)

beliefs.pca <- PCA(data.pca, graph = FALSE)
fviz_pca_biplot(beliefs.pca,  geom = "text", col.var="contrib")+
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=11)+
  labs(title = "Principal Component Analysis", color = "Importance") +
  theme_bw()

print(beliefs.pca$var$contrib)


##########################
#SI- Figure A2.3

###############################

independent_vars <- c("trust_industry","climate_worried", "science_belief")

#independent_vars <- c("science_belief", "nature_no_meddle", "trust_industry","trust_nat_gov" ,"environment_concern" ,"climate_harm")


# Create an empty list to store regression results for each technology
results_list <- list()

# Loop through each technology and each country
for (tech in technologies) {
  reg_results <- list()  # Empty list to store regression results for the current technology
  
  for (c in countries) {
    # Subset the data for the current country and technology
    subset_data <- subset(survey_clean2, iso3c == c & technology == tech)
    
    # Perform the regressions for each independent variable
    for (var in independent_vars) {
      lm_model <- lm(broader_deployment ~ ., data = subset_data[, c("broader_deployment", var)])
      reg_results[[paste(c, var, sep = "_")]] <- list(coef(summary(lm_model)), vcov(lm_model))
    }
  }
  
  # Extract coefficients and standard errors for each technology
  tech_results_list <- lapply(reg_results, function(reg_result) {
    coefs <- reg_result[[1]]
    vcovs <- reg_result[[2]]
    se <- sqrt(diag(vcovs))
    coefs_with_se <- data.frame(coefs, SE = se, term = rownames(coefs))
    coefs_with_se
  })
  
  # Combine the results into a single data frame for each technology
  tech_coefficients_df <- do.call(rbind, Map(cbind, Country_IndVar = names(tech_results_list), tech_results_list))
  
  # Split the combined column to separate country, independent_variable
  tech_coefficients_df <- tech_coefficients_df %>%
    separate(Country_IndVar, c("Country", "Independent_Variable"), sep = "_")
  
  # Add the technology name as a separate column
  tech_coefficients_df$Technology <- tech
  
  # Append the results for the current technology to the list
  results_list[[tech]] <- tech_coefficients_df
}

# Combine all results into a single data frame
coefficients_df <- do.call(rbind, results_list)

# Filter out the "(Intercept)" rows
coefficients_df <- coefficients_df %>%
  filter(term != "(Intercept)")

#Key Figure

# Set a fixed y-axis range for both plots
y_axis_range <- max(abs(coefficients_df$Estimate))  # Maximum absolute value of the estimates


all_colors <- c(srm_colors, other_colors)

overview_fig2<-coefficients_df %>%
  filter(Independent_Variable=="climate") %>%
  mutate(Technology=factor(Technology, levels=c("Space-based geoengineering","Marine cloud brightening","Stratospheric aerosol injection", "Bioenergy with carbon capture and storage","Biochar","Enhanced weathering","Direct air capture with carbon storage","Soil carbon sequestration","Marine biomass and blue carbon","Afforestation and reforestation"))) %>%
  ggplot(aes(x=reorder(Country, Estimate), y = Estimate, color = Technology)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0)+
  #geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2) +
  #facet_wrap(~ Independent_Variable, scales = "free") +
  labs(title = "Regression Coefficients by Country and Technology \nLevel of Climate Worry and Level of Acceptance",
       x = "",
       y = "") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(limits = c(-0.1, y_axis_range), expand = c(0, 0))+
  scale_color_manual(values = all_colors)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(color = NULL)+
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),  
        axis.ticks = element_line(size = 1))

overview_fig2

country_fig<-plot_grid(overview_fig2,overview_fig,labels = c("A", "B"))

country_fig
ggsave("ind_reg.jpeg", units="in", width=20, height=10, dpi=300)


#For figure 4 please refer to the STATA do file.


########################################
#Distribution of age - SI Figure A2.1
########################################
histogram_age_country<-survey_clean2 %>%
  unite("record_ID", c(country,record), remove = FALSE) %>%
  filter(!duplicated(record_ID)) %>%
  group_by(country)%>%
  mutate(median_age=median(age))%>%
  ggplot(aes(x = age)) +
  geom_bar(color = "black", position = "dodge") +
  theme_ipsum() +
  facet_wrap(~ country)+
  #ggplot(aes(x=age)) + 
  #geom_histogram(aes(y=..density..), colour="black", fill="white")+
  #geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=median_age),
             color="blue", linetype="dashed", size=1)
#+
#  theme_ipsum()+
#  facet_wrap(~country)

histogram_age_country

ggsave("age_distribution.jpeg", units="in", width=15, height=15, dpi=300)

#####################################
#Regression for each country - A2.3
#####################################

independent_vars <- c("trust_industry","climate_worried", "science_belief")

# Create an empty list to store regression results for each technology
results_list <- list()

# Loop through each technology and each country
for (tech in technologies) {
  reg_results <- list()  # Empty list to store regression results for the current technology
  
  for (c in countries) {
    # Subset the data for the current country and technology
    subset_data <- subset(survey_clean2, iso3c == c & technology == tech)
    
    # Perform the regressions for each independent variable
    for (var in independent_vars) {
      lm_model <- lm(broader_deployment ~ ., data = subset_data[, c("broader_deployment", var)])
      reg_results[[paste(c, var, sep = "_")]] <- list(coef(summary(lm_model)), vcov(lm_model))
    }
  }
  
  # Extract coefficients and standard errors for each technology
  tech_results_list <- lapply(reg_results, function(reg_result) {
    coefs <- reg_result[[1]]
    vcovs <- reg_result[[2]]
    se <- sqrt(diag(vcovs))
    coefs_with_se <- data.frame(coefs, SE = se, term = rownames(coefs))
    coefs_with_se
  })
  
  # Combine the results into a single data frame for each technology
  tech_coefficients_df <- do.call(rbind, Map(cbind, Country_IndVar = names(tech_results_list), tech_results_list))
  
  # Split the combined column to separate country, independent_variable
  tech_coefficients_df <- tech_coefficients_df %>%
    separate(Country_IndVar, c("Country", "Independent_Variable"), sep = "_")
  
  # Add the technology name as a separate column
  tech_coefficients_df$Technology <- tech
  
  # Append the results for the current technology to the list
  results_list[[tech]] <- tech_coefficients_df
}

# Combine all results into a single data frame
coefficients_df <- do.call(rbind, results_list)

# Filter out the "(Intercept)" rows
coefficients_df <- coefficients_df %>%
  filter(term != "(Intercept)")

#Key Figure

# Set a fixed y-axis range for both plots
y_axis_range <- max(abs(coefficients_df$Estimate))  # Maximum absolute value of the estimates


all_colors <- c(srm_colors, other_colors)

overview_fig2<-coefficients_df %>%
  filter(Independent_Variable=="climate") %>%
  mutate(Technology=factor(Technology, levels=c("Space-based geoengineering","Marine cloud brightening","Stratospheric aerosol injection", "Bioenergy with carbon capture and storage","Biochar","Enhanced weathering","Direct air capture with carbon storage","Soil carbon sequestration","Marine biomass and blue carbon","Afforestation and reforestation"))) %>%
  ggplot(aes(x=reorder(Country, Estimate), y = Estimate, color = Technology)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0)+
  #geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2) +
  #facet_wrap(~ Independent_Variable, scales = "free") +
  labs(title = "Regression Coefficients by Country and Technology \nLevel of Climate Worry and Level of Acceptance",
       x = "",
       y = "") +
  theme_minimal() +
  coord_flip() +
  scale_y_continuous(limits = c(-0.1, y_axis_range), expand = c(0, 0))+
  scale_color_manual(values = all_colors)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(color = NULL)+
  theme(legend.text = element_text(size = 14),
        axis.text = element_text(size = 14),  
        axis.ticks = element_line(size = 1))

overview_fig2

country_fig<-plot_grid(overview_fig2,overview_fig,labels = c("A", "B"))

country_fig
ggsave("ind_reg.jpeg", units="in", width=20, height=10, dpi=300)



#####################################
#Variance importance SI Figure A2.4
#####################################

# Load necessary libraries
library(dplyr)
library(randomForest)
library(ggplot2)
library(cowplot) # or library(patchwork)

# List of technologies
technologies <- unique(survey_clean2$technology)

# Placeholder for storing plots
plots_list <- list()

# Step 1: Loop through each technology
for (tech in technologies) {
  
  # Step 2: Filter, model, and calculate importance
  rf_model <- survey_clean2 %>%
    filter(technology == tech) %>%
    select(c(age_cat, income_level, education_level, religion, conservative, nature_no_meddle, environment_concern, climate_harm, familiarity, trust_industry, broader_deployment)) %>%
    mutate_all(as.numeric) %>%  # Convert all selected variables to numeric
    drop_na() %>%
    randomForest(broader_deployment ~ ., data = ., ntree = 500)
  
  var_importance <- importance(rf_model)
  actual_var_names <- rownames(var_importance)
  
  # Ensure the correct mapping of names
  descriptive_names <- name_mapping[actual_var_names]
  
  var_importance_df <- data.frame(
    Variable = descriptive_names,
    Importance = var_importance[, "IncNodePurity"]
  )
  
  var_importance_df <- var_importance_df %>%
    arrange(Importance)
  
  # Step 3: Create the plot
  p <- ggplot(var_importance_df, aes(x = Importance, y = reorder(Variable, Importance))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste(tech), 
         x = "Increase in Node Purity", 
         y = "") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 8, face = "bold"))
  
  # Add plot to the list
  plots_list[[tech]] <- p
}

# Step 4: Combine the plots into a grid
combined_plot <- plot_grid(plotlist = plots_list, ncol = 5, nrow = 2)

# Step 5: Save the combined plot
ggsave("combined_var_importance.jpeg", plot = combined_plot, units = "in", width = 15, height = 10, dpi = 300)


