library(data.table)    # package for reading the data  
library(tidyverse)     
library(ggplot2)       
library(scales)        # package for label formats
library(geofacet)      # package for state graphs


# set directory to folder where data are
setwd("C:/Users/ss1216/Box/NDACAN/Presentations/Summer Series 2023/S6 - Data Viz")


#### LOAD DATA ####
# load NCANDS data of number of substantiated/unsubstantiated reports
ncands = fread("CF_summerseries.csv") 
head(ncands)

ncands2 = ncands  %>% # filter out PR (b/c not in census data) and counts less than 10 (for data protection)
                      filter(staterr != "PR",
                            unsubst > 10,
                            subst > 10)

# load census data
census = fread("census_pop.csv") 
head(census)


## join census and ncands
# left join because some states not reported in ncands from 2010-2012
dat  = ncands2 %>% # rename variables to link with census
                      rename(year = subyr,
                            st = staterr,
                            sex = chsex) %>% 
                  # left join because ncands may not have all states all years like census
                  left_join(census) %>%
                  # reorder variables
                  dplyr::select(year, st, state, stfips, everything()) %>%
                  # sort by year, state, and race
                  arrange(year, stfips, raceEthn)
head(dat)


## Data cleaning
dat2 = dat %>% # add informative labels to race and sex
                mutate(raceEthn2 = case_when(raceEthn == 1 ~ "White NH",
                                             raceEthn == 2 ~ "Black NH",
                                             raceEthn == 3 ~ "Native Am NH",
                                             raceEthn %in% 4:5 ~ "AAPI NH",
                                             raceEthn == 6 ~ "Multiracial NH",
                                             raceEthn == 7 ~ "Hispanic"),
                               sex2 = ifelse(sex == 1, "Male", "Female"))
head(dat2)


#### Summarize data to national level
# totals in each year - grouped by race and sex
natdat = dat2 %>% group_by(year, raceEthn2, sex2) %>%
                  summarise(unsubst = sum(unsubst,na.rm = TRUE),
                            subst = sum(subst, na.rm = TRUE),
                            pop = sum(pop,na.rm = TRUE))
head(natdat)

# total in each year - total over everyone
natdat_tot = dat2 %>% group_by(year) %>%
                    summarise(unsubst = sum(unsubst,na.rm = TRUE),
                              subst = sum(subst, na.rm = TRUE),
                              pop = sum(pop,na.rm = TRUE)) 
head(natdat_tot)




# put natdat_tot data in long format
natdat_tot_long = natdat_tot %>% pivot_longer(cols = c(unsubst, subst),
                                              names_to = "rptoutcome",
                                              values_to = "rpts")

head(natdat_tot_long)


#### FIGURES ######
# basic scatter plot of substantiated reports, at national level
p = ggplot(natdat_tot_long %>% filter(rptoutcome == "subst"), 
           aes(x = year, y = rpts)) +
           geom_point()
p


# add unsubstantiated data, at national level
# make the lines different color based on substantiation/outcome
p2 = ggplot(natdat_tot_long, 
            aes(x = year, y = rpts, color = rptoutcome)) +
  geom_point() + 
  geom_line()
p2



# take previous figure but fix labels and some reformatting
p2 + 
  # change x axes lines to 2010-2020, incremented by 1 yr
  scale_x_continuous(breaks = 2010:2020) +
  
  # change y axes to start at 0 and go to 3,000,000, incremented by 500,000 - formatted with commas
  scale_y_continuous(limits = c(0,3e6),
                   breaks = seq(0,3e6, by = 5e5),
                   label = scales::comma) +  
  
  # relabel x and y axes, and title
  xlab("Year") + 
  ylab("Number children") +
  ggtitle("Number of children on reports of maltreatment, substantiated or unsubstantiated") +
  
  # remove the color legend title name 
  labs(color = "") +
  
  # relabel the values of "subst" and "unsubst" respectively, need to specify 'values'/colors for each one too
  scale_color_manual(values = c("red","blue"),
                     breaks =  c("subst", "unsubst"),
                     labels = c("Substantiated", "Unsubstantiated")) +
  
  # put the legend horizontally on the bottom
  theme(legend.position = "bottom")





### just look at substantiated cases
p + 
  geom_line() +
  scale_x_continuous(breaks = 2010:2020) +
  scale_y_continuous(label = scales::comma,
                     #limits = c(0,650000)
                     ) +
  xlab("Year") + 
  ylab("Number substantiated") +
  ggtitle("Number of children on reports of substantiated maltreatment") 









## look at national trends of race

# make national level data - totals by race/ethnicity
natdat_race = natdat %>% group_by(year, raceEthn2) %>% 
                         summarise(unsubst = sum(unsubst, na.rm = TRUE),
                                   subst = sum(subst, na.rm = TRUE),
                                   pop = sum(pop, na.rm = TRUE))


# Plot number substantiated by race
ggplot(natdat_race, aes(x = year, y = subst, color = raceEthn2)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2010:2020) +
  scale_y_continuous(label = scales::comma,
                     breaks = seq(0,300000, by = 50000)) +
  guides(color = guide_legend("Race"))+ 
  xlab("Year") + 
  ylab("Number of substantiated cases") +
  ggtitle("Number of children on reports of substantiated reports of maltreatment")






## Create rates to standardize comparison
# national level rates of substantiated reports per 100k children - by race
natdat_race3 = natdat_race %>% mutate(subst_rate = 100000*subst/pop)


# plot substantiated rate
ggplot(natdat_race3, 
       aes(x = year, y = subst_rate, color = raceEthn2)) + 
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 2010:2020) +
  scale_y_continuous(label = scales::comma,
                     limits = c(0,1700),
                     breaks = seq(0,1600, by = 400)) +
  guides(color = guide_legend("Race")) + 
  xlab("Year") + 
  ylab("Rate of substantiated cases (per 100k children)") +
  ggtitle("Rate of substantiated reports of maltreatment (per 100,000 children)") +
  theme(legend.position = "bottom")







### look at national trends of race and sex #####
# grouping by sex too now
natdat_race_sex = natdat %>% group_by(year, raceEthn2, sex2) %>% 
                            summarise(unsubst = sum(unsubst, na.rm = TRUE),
                                      subst = sum(subst, na.rm = TRUE),
                                      pop = sum(pop, na.rm = TRUE)) %>% 
                            mutate(subst_rate = 100000*subst/pop)



# facet by sex
ggplot(natdat_race_sex, aes(x = year, y = subst_rate, color = raceEthn2)) + 
  geom_point() +
  geom_line() +
  facet_grid(~sex2) +
  scale_x_continuous(breaks = 2010:2020) +
  scale_y_continuous(label = scales::comma,
                     limits = c(0,1700),
                     breaks = seq(0,1600, by = 400)) +
  guides(color = guide_legend("Race"))+ 
  xlab("Year") + 
  ylab("Rate of substantiated cases (per 100k children)") +
  ggtitle("Rate of substantiated reports of maltreatment (per 100,000 children)") +
  theme(legend.position = "bottom")




# facet by race instead
ggplot(natdat_race_sex, aes(x = year, y = subst_rate, color = sex2)) + 
  geom_point() +
  geom_line() +
  # using facet_wrap now, can easily specify 2 rows and free scales between figures
  facet_wrap(~raceEthn2,nrow = 2, scales = "free") +
  scale_x_continuous(breaks = 2010:2020) +
  scale_y_continuous(label = scales::comma) +
  guides(color = guide_legend(""))+ 
  xlab("Year") + 
  ylab("Rate of substantiated cases (per 100k children)") +
  ggtitle("Rate of substantiated reports of maltreatment (per 100,000 children)") +
  theme(legend.position = "bottom")





#### make figures by state #######
# collapse data over state
statedat = dat %>% group_by(year, st, state, stfips) %>% 
                  summarise(unsubst = sum(unsubst, na.rm = TRUE),
                            subst = sum(subst, na.rm = TRUE),
                            pop = sum(pop, na.rm = TRUE)) %>%
                arrange(year,stfips)



# plot substantiated by state
ggplot(statedat, aes(x = year, y = subst)) +
  geom_point() + 
  geom_line() + 
  facet_geo(~st, grid = "us_state_grid1"#, scales = "free_y" 
            ) + 
  scale_x_continuous(breaks = 2010:2020)+
  scale_y_continuous(label = scales::comma) +
  xlab("Year") + 
  ylab("Number children") +
  ggtitle("Number of children on reports of maltreatment, substantiated or unsubstantiated") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## make rates instead
statedat2 = statedat %>% mutate(subst_rate = 10000*subst/pop,
                                unsubst_rate = 10000*unsubst/pop)


# plot substantiated rate by state
ggplot(statedat2, aes(x = year, y = subst_rate)) +
  geom_point() + 
  facet_geo(~st, grid = "us_state_grid1") + 
  scale_x_continuous(breaks = 2010:2020) +
  xlab("Year") + 
  ylab("Rate per 10k Children ") +
  ggtitle("Rate of children on reports of maltreatment, substantiated or unsubstantiated") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




## plot subst and unsubt by color - make long first
statedat_long = statedat2 %>% dplyr::select(year,st,state,stfips,ends_with("rate")) %>%
                              pivot_longer(cols = subst_rate:unsubst_rate,
                                           names_to = "rptoutcome",
                                           values_to = "rate")

# plot substantiated and unsubstantiated rate by state
ggplot(statedat_long, aes(x = year, y = rate, color = rptoutcome)) +
  geom_point() +
  geom_line() + 
  facet_geo(~st, grid = "us_state_grid1") + 
  scale_x_continuous(breaks = 2010:2020) +
  xlab("Year") + 
  ylab("Rate per 10k Children ") +
  ggtitle("Rate of children on reports of maltreatment, substantiated or unsubstantiated") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "") +
  scale_color_manual(values = c("red","blue"),
                     breaks =  c("subst_rate", "unsubst_rate"),
                     labels = c("Substantiated", "Unsubstantiated")) +
  theme(legend.position = "bottom",
        # edit axis text to be a little smaller and vertical
        axis.text.x = element_text(angle = 90, vjust = 0, 
                                   size = 8))


