
install.packages("Rmisc")
library("Rmisc")
install.packages("ggplot2")
library("ggplot2")
install.packages("ggpubr")
library("ggpubr")
install.packages("dplyr")
library("dplyr")

# read in data
data <- read.csv("O2 Probe Offset.csv")

# create summaries for Winkler and Probe data
Winkler <- summarySE(data=data, measurevar="Winkler_O2", groupvars="Tank")
Winkler # look at summary
Probe <- summarySE(data=data, measurevar="Avg_Mod_O2", groupvars="Tank")
Probe # look at summary

# Graph Winkler vs. Probe raw O2 values
a <- ggplot()+
  geom_point(data=Winkler, aes(x=Tank, y=Winkler_O2, color="Winkler O2"), size=3)+
  geom_errorbar(data=Winkler, aes(x=Tank, y=Winkler_O2, ymin=Winkler_O2-sd, 
              ymax=Winkler_O2+sd), width=0.2, position=position_dodge(.9))+
  geom_point(data=Probe, aes(x=Tank, y=Avg_Mod_O2, color="Probe O2"), size=3)+
  geom_errorbar(data=Probe, aes(x=Tank, y=Avg_Mod_O2, ymin=Avg_Mod_O2-sd, 
              ymax=Avg_Mod_O2+sd), width=0.2, position=position_dodge(.9))+
  ylab(expression(paste('O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Winkler Sample vs. Probe (Before)")+
  ylim(c(180,300))+
  theme(legend.title = element_blank())
a 

# summarize offset for each probe
Probe_Offset <- summarySE(data=data, measurevar="Probe_Offset", groupvars="Tank")
Probe_Offset

# Graph offset
b <- ggplot()+
  geom_col(data=Probe_Offset, aes(x=Tank, y=Probe_Offset))+
  geom_errorbar(data=Probe_Offset, aes(x=Tank, y=Probe_Offset, 
               ymin=Probe_Offset-sd, ymax=Probe_Offset+sd), width=0.2)+
  ylab(expression(paste('Difference', ~'O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Offset (Before)")
b

# Facet into one image
Figure <- ggarrange(a, b)
Figure

## POST-OFFSET #1

# read in data file after offset applied
data2 <- read.csv("O2 Probe Offset_7.18.24.csv")

# subset data to show only data after offset applied
data2 <- data2 %>% filter(Sample_Date == "7/17/24" | Sample_Date == "7/18/24")

# create summaries for Winkler and Probe data
Winkler2 <- summarySE(data=data2, measurevar="Winkler_O2", groupvars="Tank")
Winkler2 # look at summary
Probe2 <- summarySE(data=data2, measurevar="Avg_Mod_O2", groupvars="Tank")
Probe2 # look at summary

# graph winkler vs. probe
c <- ggplot()+
  geom_point(data=data2, aes(x=Tank, y=Winkler_O2, color="Winkler O2"), size=3)+
  geom_errorbar(data=Winkler2, aes(x=Tank, y=Winkler_O2, ymin=Winkler_O2-sd, 
                                  ymax=Winkler_O2+sd), width=0.2, position=position_dodge(.9))+
  geom_point(data=data2, aes(x=Tank, y=Avg_Mod_O2, color="Probe O2"), size=3)+
  geom_errorbar(data=Probe2, aes(x=Tank, y=Avg_Mod_O2, ymin=Avg_Mod_O2-sd, 
                                ymax=Avg_Mod_O2+sd), width=0.2, position=position_dodge(.9))+
  ylab(expression(paste('O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Winkler vs. Probe (Offset1)")+
  ylim(c(180,300))+
  theme(legend.title = element_blank())
c 

# summarize offset for each probe
Probe_Offset2 <- summarySE(data=data2, measurevar="Probe_Offset", groupvars="Tank")
Probe_Offset2

# Graph offset
d <- ggplot()+
  geom_col(data=Probe_Offset2, aes(x=Tank, y=Probe_Offset))+
  geom_errorbar(data=Probe_Offset2, aes(x=Tank, y=Probe_Offset, 
                                       ymin=Probe_Offset-sd, ymax=Probe_Offset+sd), width=0.2)+
  ylab(expression(paste('Difference', ~'O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Offset1")+
  ylim(c(0,20))
d

# Facet into one image
Figure <- ggarrange(a, b, c, d)
Figure

# GRAPHS BY Tank

Tank9 <- data2 %>% filter(Tank == 9)
Tank10 <- data2 %>% filter(Tank == 10)
Tank11 <- data2 %>% filter(Tank == 11)
Tank12 <- data2 %>% filter(Tank == 12)

# POST OFFSET #2

# read in data file after offset applied
data3 <- read.csv("O2 Probe Offset_7.26.24.csv")

# subset data to show only data after offset applied
data3 <- data3 %>% filter(Sample_Date == "7/24/24" | Sample_Date == "7/26/24")

# create summaries for Winkler and Probe data
Winkler3 <- summarySE(data=data3, measurevar="Winkler_O2", groupvars="Tank")
Winkler3 # look at summary
Probe3 <- summarySE(data=data3, measurevar="Avg_Mod_O2", groupvars="Tank")
Probe3 # look at summary

# graph winkler vs. probe
e <- ggplot()+
  geom_point(data=data3, aes(x=Tank, y=Winkler_O2, color="Winkler O2"), size=3)+
  geom_errorbar(data=Winkler3, aes(x=Tank, y=Winkler_O2, ymin=Winkler_O2-sd, 
                                   ymax=Winkler_O2+sd), width=0.2, position=position_dodge(.9))+
  geom_point(data=data3, aes(x=Tank, y=Avg_Mod_O2, color="Probe O2"), size=3)+
  geom_errorbar(data=Probe3, aes(x=Tank, y=Avg_Mod_O2, ymin=Avg_Mod_O2-sd, 
                                 ymax=Avg_Mod_O2+sd), width=0.2, position=position_dodge(.9))+
  ylab(expression(paste('O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Winkler vs. Probe (Offset2)")+
  ylim(c(180,300))+
  theme(legend.title = element_blank())
e 

# summarize offset for each probe
Probe_Offset3 <- summarySE(data=data3, measurevar="Probe_Offset", groupvars="Tank")
Probe_Offset3

# Graph offset
f <- ggplot()+
  geom_col(data=Probe_Offset3, aes(x=Tank, y=Probe_Offset))+
  geom_errorbar(data=Probe_Offset3, aes(x=Tank, y=Probe_Offset, 
                                        ymin=Probe_Offset-sd, ymax=Probe_Offset+sd), width=0.2)+
  ylab(expression(paste('Difference', ~'O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Offset2")+
  ylim(c(0,20))
f

#arrange into one figure
Figure <- ggarrange(a, b, c, d, e, f)
Figure

Figure <- ggarrange(c, d, e, f)
Figure

# Checking second Offset, and tanks 13-16

# read in data file
data4 <- read.csv("O2 Probe Offset 8.26.csv")

# subset data to show only data after offset applied
data4 <- data4 %>% filter(Sample_Date == "8/20/24")

# graph winkler vs. probe
e <- ggplot()+
  geom_point(data=data4, aes(x=Tank, y=Winkler_O2, color="Winkler O2"), size=3)+
  geom_point(data=data4, aes(x=Tank, y=Avg_Mod_O2, color="Probe O2"), size=3)+
  ylab(expression(paste('O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Winkler vs. Probe 8/20/24")+
  ylim(c(190,250))+
  theme(legend.title = element_blank())
e 

# Graph difference
data4$Tank <- factor(data4$Tank, levels = 9:16)

g <- ggplot(data = data4, aes(x = Tank, y = Probe_Offset)) +
  geom_col() +
  ylab(expression(paste('Difference', ~'O'[2], ~mu, 'mol', ~'L'^-1))) +
  ggtitle("Difference per Tank 8/20/24") +
  scale_x_discrete(drop = FALSE)+# Ensures all factor levels are displayed
  ylim(c(-40, 20))
g
Figure <- ggarrange(e, f)
Figure

## PHASE 1 STEP 1: officially calculating offsets for tanks 1-16

# read in data 
data <- read.csv("O2 Probe Offset_10.3.24.csv")

# subset data to show only data for phase 1
data <- data %>% filter(Phase == "Phase 1 Step 1")

Sample_summary <- summarySE(data=data, measurevar="Winkler_O2", groupvars="Tank")
Sample_summary

# graph winkler vs. probe
e <- ggplot()+
  geom_point(data=data, aes(x=Tank, y=Winkler_O2, color="Winkler O2"), size=3)+
  geom_errorbar(data=Sample_summary, aes(x=Tank, y=Winkler_O2, ymin=Winkler_O2-sd, 
                                   ymax=Winkler_O2+sd), width=0.2, position=position_dodge(.9))+
  geom_point(data=data, aes(x=Tank, y=Avg_Mod_O2, color="Probe O2"), size=3)+
  ylab(expression(paste('O'[2], ~mu, 'mol', ~'L'^-1)))+
  ggtitle("Winkler vs. Probe 10/3/24")+
  theme(legend.title = element_blank())+
  scale_x_continuous(breaks = round(seq(min(data$Tank), max(data$Tank), by = 1),1))
e 

# graph offset
f <- ggplot(data = data, aes(x = Tank, y = Probe_Offset)) +
  geom_col() +
  ylab(expression(paste('Difference', ~'O'[2], ~mu, 'mol', ~'L'^-1))) +
  ggtitle("Difference per Tank 10/3/24") +
  scale_x_continuous(breaks = round(seq(min(data$Tank), max(data$Tank), by = 1),1))+
  ylim(c(-40,20))
f

Figure <- ggarrange(e,f)
Figure

Figure <- ggarrange(g,f)
Figure

# filter only tanks 9-16 for comparison
data <- data %>% filter(Tank > 8)
