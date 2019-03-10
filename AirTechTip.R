# First Install R packages tidyverse and readxl
# In Rstudio --> go to Tools, then Install Packages.

library(tidyverse)
library(readxl)


file.list <- list.files(path = "RawData", pattern='*.xlsx', full.names = TRUE)
df_all <- do.call(bind_rows,lapply(file.list, read_excel))


new_sal = df_all %>% rowwise() %>% 
  # For each row, calculate a total number of full time 
  # at least 9 month contract faculty.
  mutate(total_faculty = sum(num_men,num_women, na.rm=TRUE))  %>%
  ungroup() %>%
  # Correct for 2012-2014
  # First faculty by rank equity distribution and then equity ratio
  mutate(avg_men = ifelse(year> 2011 & year <2017,avg_men*9,avg_men),
         avg_women=ifelse(year>2011 & year <2017,avg_women*9,avg_women)) %>%
  mutate(equity = round(avg_women/avg_men * 100, 2))%>% 
  #Remove if salary is below 15k per year
  filter(avg_men >=15000 &avg_women >=15000) %>% 
  # Remove if there are less than 30 faculty at an institution 
  # In a given rank.
  filter(num_men >=15 & num_women>=15) %>% 
  #This is meant to allow me to graph by year (as a category)
  mutate(year = as.character(year))



by_year = new_sal %>% group_by(year,rank) %>% 
  summarise(AvgEquity = weighted.mean(equity,total_faculty,na.rm=TRUE),
            sd_equity = sd(equity, na.rm=TRUE), N=length(unique(unitid))) %>%
  mutate(rank = 
           factor(rank, levels=c("Professor", "Associate professor", "Assistant professor", "Instructor","Lecturer","No academic rank")))


# A color pallete that works well for people with sight color deficiencies.

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



ggplot(by_year,aes(x=year, y=AvgEquity, color=rank, group=rank))+
  geom_point() + geom_line(size=2) + 
  theme_bw() + 
  ggtitle("2005 to 2017 Salary Equity by Rank in the U.S.") +
  xlab("Year") + 
  ylab("Ratio of Average Women to Men Salary x 100") + 
  geom_hline(yintercept =100, color="red") +
  scale_colour_manual(values=cbbPalette)



lect_2005 = new_sal %>% filter(year == 2005 & rank == "Lecturer")

#In RStudio, opens a table to 
View(lect_2005)


info =read_excel("primary/ipeds_info.xlsx")

# Look at Salary Equity by Control Type (not in Tech Tip)
new_sal2 = new_sal %>% left_join(info,by="unitid") %>%
  filter(!is.na(institution_name.y))


# Same as before.
sum_sal = new_sal2 %>% 
  group_by(year,institutional_control,rank) %>% 
  summarise(AvgEquity = weighted.mean(equity,total_faculty,na.rm=TRUE),
            sd_equity = sd(equity, na.rm=TRUE), N=length(unique(unitid)))%>%
  mutate(rank = 
           factor(rank, levels=c("Professor", "Associate professor", "Assistant professor", "Instructor","Lecturer","No academic rank")))


# Graph each by institutional control.
ggplot(sum_sal,aes(x=year, y=AvgEquity, color=rank, group=rank, shape=rank))+
  geom_line(size=2) + theme_bw() + 
  ggtitle("2005 to 2017 Salary Equity by Rank") +
  xlab("Year") + ylab("Ratio of Average Women to Men Salary x 100") + 
  geom_hline(yintercept =100, color="red") +
  scale_colour_manual(values=cbbPalette)+
  facet_wrap(~institutional_control, ncol=10)





