# Ngay 23-08-2020
# Su phan bo nguyen lieu cua manh tuoc qua thoi gian

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
View(lop1)
str(lop1)
lop1$Mass <- as.numeric(lop1$Mass)
lop1$zone13t <- as.numeric(lop1$zone13t)
lop1$zone14t <- as.numeric(lop1$zone13t)
median(lop1$zone13t, na.rm = TRUE)
IQR(lop1$zone13t, na.rm = TRUE)
lop1$zone1t <- as.numeric(lop1$zone1t)
median(lop1$zone1t, na.rm = TRUE)
IQR(lop1$zone1t, na.rm = TRUE)
median(lop1$zone2t, na.rm = TRUE)
IQR(lop1$zone2t, na.rm = TRUE)
names(lop1)
lop1 %>% 
  group_by(`Material`) %>%
  tally()
lop1_Material_tally <-lop1 %>% 
  group_by(`Material`) %>% 
  tally() %>% 
  filter(`Material` != 'NA') %>% 
  arrange(desc(n))
ggplot(lop1_Material_tally,
       aes(x = reorder(`Material`, n), y = n)) +
  geom_bar(stat="identity", width = 0.8, size = 0.6, fill ="white", color = "black") +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Frequency") +
  ggtitle("Unit 1 and Unit 2") +
  coord_flip()


## Ngay 02-07-2024 -  LOI CODE- chua cahy
# Kiem tra su khac biet trong luong
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
View(lop1)
str(lop1)
attach(lop1)
lop1
str(lop1)
ggplot(lop1, aes(x=Material, y=Context)) +
  geom_boxplot(size = 0.6, width = 0.3, outlier.shape = NA) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(legend.position="top") +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Mass (g)") +
  ggtitle("Unit 1 and Unit 2") +
  coord_flip()
#
lop1$Mass <- as.numeric(lop1$Mass)
median(lop1$Mass, na.rm  = TRUE)
sd(lop1$Mass, na.rm  = TRUE)
IQR(lop1$Mass, na.rm  = TRUE)
t.test(lop1$Mass, na.rm  = TRUE)


###--------------------------------------------------------------------------------
## The percent correspoding to States in Units 1 and 2

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
str(lop1)
ggplot(lop1) +
  aes(x = Context, fill = State) +
  geom_bar(position = "fill", width=0.8, size = 0.9, color = "black") +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', size = 7, position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 9/10) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=0.3, color = "black"),
        axis.text.x = element_text(size = 25, angle= 0, vjust=0.5, color = "black")) +
  theme(legend.position="top") +
  xlab("Lớp văn hóa II (L10-4) năm 2017") +
  ylab("%") +
  ggtitle("") +
  labs(fill = "Tình trạng") # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"


## Hinh thai Dau
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
ggplot(lop1) +
  aes(x = Flake_initiation, fill = Material) +
  geom_bar(position = "fill", width=0.6, size = 0.9) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.3)) +
  theme(aspect.ratio = 12/9) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=0.3),
        axis.text.x = element_text(size = 25, angle= 0, vjust=0.5)) +
  theme(legend.position="top") +
  xlab("Excavation contexts") +
  ylab("Percentage") +
  ggtitle("Unit 1 (Contexts 3-6)") +
  labs(fill = "States") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous()


# Tinh trang vA LENGTH
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
attach(lop1)
lop1$Length <- as.numeric(lop1$Length)
ggplot(lop1, aes(x=reorder(State, Length),
                 y = Length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()

# Anlop2# Anova of States with Length - Lop 28-22ka
str(lop1)
attach(lop1)
av1 = aov(Length ~ State)
summary(av1)
av1
TukeyHSD(av1)
unit1 = TukeyHSD(av1)
plot(unit1)


# Tinh trang va Width
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
attach(lop1)
lop1$Width <- as.numeric(lop1$Width)
ggplot(lop1, aes(x=reorder(State, Width),
                 y = Width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Chiều rộng (mm)") +
  scale_y_log10() +
  ggtitle("Lớp văn hóa I (L4-L10)") +
  coord_flip()

# Anlop2# Anova of States with Length- Unit 1
str(lop1)
attach(lop1)
wa = aov(Width ~ State)
summary(wa)
wa
TukeyHSD(wa)
unit1 = TukeyHSD(wa)
plot(unit1)


# Tinh trang vA Thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
lop1$Thickness <- as.numeric(lop1$Thickness)
ggplot(lop1, aes(x=reorder(State, Thickness),
                 y = Thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()

# Anlop2# Anova of States with Length- Unit 1
str(lop1)
attach(lop1)
T1 = aov(Thickness ~ State)
summary(T1)
T1
TukeyHSD(T1)
unit1 = TukeyHSD(T1)
plot(unit1)


# Tinh trang vA Platform_width
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
attach(lop1)
lop1$Platform_width <- as.numeric(lop1$Platform_width)
ggplot(lop1, aes(x=reorder(State, Platform_width),
                 y = Platform_width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()

# Anlop2# Anova of States with Length- Unit 1
str(lop1)
attach(lop1)
T2 = aov(Platform_width ~ State)
summary(T2)
T2
TukeyHSD(T2)
unit2 = TukeyHSD(T2)
plot(unit2)


# Tinh trang vA Platform_thickness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
lop1 <- read_excel("B_Retouchedtools.xlsx", sheet = 'Tang28_22ka')
attach(lop1)
lop1
lop1$Platform_thickness <- as.numeric(lop1$Platform_thickness)
ggplot(lop1, aes(x=reorder(State, Platform_thickness),
                 y = Platform_thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()


### The percentage of each kind of raw_material of unit 1 + 2
## Chart is OK!
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake <- read_excel("flake.xlsx", sheet = 'flakes')
names(flake)
ggplot(flake) +
  aes(x = context, fill = raw_material) +
  geom_bar(position = "fill", width= 0.8, size = 0.1) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat = '', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 15/2) +
  theme_bw() +
  theme(text = element_text(size=15),
        axis.text.y = element_text(size = 15, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 15, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("Excavation units") +
  ylab("Percentage") +
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  labs(fill = "Raw material") + # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) 


### Computing "Median and Interquartile range" Lop muon
str(lop1)
attach(lop1)
median(lop1$Mass, na.rm = TRUE)
IQR(lop1$Mass, na.rm = TRUE)

median(lop1$Max_dimension, na.rm = TRUE)
IQR(lop1$Max_dimension, na.rm = TRUE)

median(lop1$Length, na.rm = TRUE)
IQR(lop1$Length, na.rm = TRUE)

median(lop1$Width, na.rm = TRUE)
IQR(lop1$Width, na.rm = TRUE)

median(lop1$Thickness, na.rm = TRUE)
IQR(lop1$Thickness, na.rm = TRUE)

median(lop1$Platform_width, na.rm = TRUE)
IQR(lop1$Platform_width, na.rm = TRUE)

median(lop1$Platform_thickness, na.rm = TRUE)
IQR(lop1$Platform_thickness, na.rm = TRUE)

median(lop1$External_platform_angle, na.rm = TRUE)
IQR(lop1$External_platform_angle, na.rm = TRUE)

median(lop1$Internal_platform_angle, na.rm = TRUE)
IQR(lop1$Internal_platform_angle, na.rm = TRUE)

median(lop1$Tylevocuoi, na.rm = TRUE)
IQR(lop1$Tylevocuoi, na.rm = TRUE)

median(lop1$Sovetghe_dienghe, na.rm = TRUE)
IQR(lop1$Sovetghe_dienghe, na.rm = TRUE)

median(lop1$Sovetghe_lung, na.rm = TRUE)
IQR(lop1$Sovetghe_lung, na.rm = TRUE)

median(lop1$Sovetghe_bung, na.rm = TRUE)
IQR(lop1$Sovetghe_bung, na.rm = TRUE)

median(lop1$zone1t, na.rm = TRUE)
IQR(lop1$zone1t, na.rm = TRUE)

median(lop1$zone2t, na.rm = TRUE)
IQR(lop1$zone2t, na.rm = TRUE)

median(lop1$zone3t, na.rm = TRUE)
IQR(lop1$zone3t, na.rm = TRUE)

median(lop1$zone4t, na.rm = TRUE)
IQR(lop1$zone4t, na.rm = TRUE)

median(lop1$zone5t, na.rm = TRUE)
IQR(lop1$zone5t, na.rm = TRUE)

median(lop1$zone6t, na.rm = TRUE)
IQR(lop1$zone6t, na.rm = TRUE)

median(lop1$zone7t, na.rm = TRUE)
IQR(lop1$zone7t, na.rm = TRUE)

median(lop1$zone8t, na.rm = TRUE)
IQR(lop1$zone8t, na.rm = TRUE)

median(lop1$zone9t, na.rm = TRUE)
IQR(lop1$zone9t, na.rm = TRUE)

median(lop1$zone10t, na.rm = TRUE)
IQR(lop1$zone10t, na.rm = TRUE)

median(lop1$zone11t, na.rm = TRUE)
IQR(lop1$zone11t, na.rm = TRUE)

median(lop1$zone12t, na.rm = TRUE)
IQR(lop1$zone12t, na.rm = TRUE)

median(lop1$zone13t, na.rm = TRUE)
IQR(lop1$zone13t, na.rm = TRUE)

median(lop1$zone14t, na.rm = TRUE)
IQR(lop1$zone14t, na.rm = TRUE)

median(lop1$zone15t, na.rm = TRUE)
IQR(lop1$zone15t, na.rm = TRUE)

median(lop1$zone16t, na.rm = TRUE)
IQR(lop1$zone16t, na.rm = TRUE)

## Lets check the "Median and Interquartile range" -U1_2 - Orthophyre
# U1_2 - Orthophyre
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1_2 <- read_excel("flake.xlsx", sheet = 'u1_2')
str(flake_u1_2)
attach(flake_u1_2)
View(flake_u1_2)
str(flake_u2)
attach(flake_u2)

# To convert metric variables to as.numeric
flake_u1_2$mass <- as.numeric(flake_u2$mass)
flake_u1_2$max_dimension <- as.numeric(flake_u1_2$max_dimension)
flake_u1_2$length <- as.numeric(flake_u1_2$length)
flake_u1_2$width <- as.numeric(flake_u1_2$width)
flake_u1_2$thickness <- as.numeric(flake_u1_2$thickness)
flake_u1_2$platform_width <- as.numeric(flake_u1_2$platform_width)
flake_u1_2$platform_thickness <- as.numeric(flake_u1_2$platform_thickness)
flake_u1_2$external_platform_angle <- as.numeric(flake_u1_2$external_platform_angle)
flake_u1_2$internal_platform_angle<- as.numeric(flake_u1_2$internal_platform_angle)
flake_u1_2$dorsal_cortex_percentage <- as.numeric(flake_u1_2$dorsal_cortex_percentage)
flake_u1_2$number_of_dorsal_scars<- as.numeric(flake_u1_2$number_of_dorsal_scars)
View(flake_u1_2)

### Computing "Median and Interquartile range" - U1_1
str(flake_u1_2)
attach(flake_u1_2)
median(flake_u1_2$mass, na.rm = TRUE)
IQR(flake_u1_2$mass, na.rm = TRUE)

median(flake_u1_2$max_dimension, na.rm = TRUE)
IQR(flake_u1_2$max_dimension, na.rm = TRUE)

median(flake_u1_2$length, na.rm = TRUE)
IQR(flake_u1_2$length, na.rm = TRUE)

median(flake_u1_2$width, na.rm = TRUE)
IQR(flake_u1_2$width, na.rm = TRUE)

median(flake_u1_2$thickness, na.rm = TRUE)
IQR(flake_u1_2$thickness, na.rm = TRUE)

median(flake_u1_2$number_of_dorsal_scars, na.rm = TRUE)
IQR(flake_u1_2$number_of_dorsal_scars, na.rm = TRUE)

median(flake_u1_2$platform_width, na.rm = TRUE)
IQR(flake_u1_2$platform_width, na.rm = TRUE)

median(flake_u1_2$platform_thickness, na.rm = TRUE)
IQR(flake_u1_2$platform_thickness, na.rm = TRUE)

median(flake_u1_2$external_platform_angle, na.rm = TRUE)
IQR(flake_u1_2$external_platform_angle, na.rm = TRUE)

median(flake_u1_2$internal_platform_angle, na.rm = TRUE)
IQR(flake_u1_2$internal_platform_angle, na.rm = TRUE)


## Lets check the "Median and Interquartile range" - U1_3 - Basalt
# U1_3 - Basalt
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1_3 <- read_excel("flake.xlsx", sheet = 'u1_3')
str(flake_u1_3)
attach(flake_u1_3)
View(flake_u1_3)

# To convert metric variables to as.numeric
flake_u1_3$mass <- as.numeric(flake_u1_3$mass)
flake_u1_3$max_dimension <- as.numeric(flake_u1_3$max_dimension)
flake_u1_3$length <- as.numeric(flake_u1_3$length)
flake_u1_3$width <- as.numeric(flake_u1_3$width)
flake_u1_3$thickness <- as.numeric(flake_u1_3$thickness)
flake_u1_3$platform_width <- as.numeric(flake_u1_3$platform_width)
flake_u1_3$platform_thickness <- as.numeric(flake_u1_3$platform_thickness)
flake_u1_3$external_platform_angle <- as.numeric(flake_u1_3$external_platform_angle)
flake_u1_3$internal_platform_angle<- as.numeric(flake_u1_3$internal_platform_angle)
flake_u1_3$dorsal_cortex_percentage <- as.numeric(flake_u1_3$dorsal_cortex_percentage)
flake_u1_3$number_of_dorsal_scars<- as.numeric(flake_u1_3$number_of_dorsal_scars)
View(flake_u1_3)

### Computing "Median and Interquartile range" - U1_1
str(flake_u1_3)
attach(flake_u1_3)
median(flake_u1_3$mass, na.rm = TRUE)
IQR(flake_u1_3$mass, na.rm = TRUE)

median(flake_u1_3$max_dimension, na.rm = TRUE)
IQR(flake_u1_3$max_dimension, na.rm = TRUE)

median(flake_u1_3$length, na.rm = TRUE)
IQR(flake_u1_3$length, na.rm = TRUE)

median(flake_u1_3$width, na.rm = TRUE)
IQR(flake_u1_3$width, na.rm = TRUE)

median(flake_u1_3$thickness, na.rm = TRUE)
IQR(flake_u1_3$thickness, na.rm = TRUE)

median(flake_u1_3$number_of_dorsal_scars, na.rm = TRUE)
IQR(flake_u1_3$number_of_dorsal_scars, na.rm = TRUE)

median(flake_u1_3$platform_width, na.rm = TRUE)
IQR(flake_u1_3$platform_width, na.rm = TRUE)

median(flake_u1_3$platform_thickness, na.rm = TRUE)
IQR(flake_u1_3$platform_thickness, na.rm = TRUE)

median(flake_u1_3$external_platform_angle, na.rm = TRUE)
IQR(flake_u1_3$external_platform_angle, na.rm = TRUE)

median(flake_u1_3$internal_platform_angle, na.rm = TRUE)
IQR(flake_u1_3$internal_platform_angle, na.rm = TRUE)



###---------------------------------------------------------------------
## Unit 2
# Phan loai theo nhom nguyen lieu
## To see the frequency of scar numbers by group of basalt/quartite/orthophyre ...etc
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
View(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
ggplot(flake_u2, aes(x=total_material, y=mass)) +
  geom_boxplot(size = 0.6, width = 0.3) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle=  0, hjust=.5)) +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Mass (g)") +
  scale_y_log10() +
  ggtitle("Unit 2") +
  coord_flip()


## Lets check the "Median and Interquartile range"
# U2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
View(flake_u2)

# To convert metric variables to as.numeric
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
flake_u2$length <- as.numeric(flake_u2$length)
flake_u2$width <- as.numeric(flake_u2$width)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
flake_u2$platform_width <- as.numeric(flake_u2$platform_width)
flake_u2$platform_thickness <- as.numeric(flake_u2$platform_thickness)
flake_u2$external_platform_angle <- as.numeric(flake_u2$external_platform_angle)
flake_u2$internal_platform_angle<- as.numeric(flake_u2$internal_platform_angle)
flake_u2$dorsal_cortex_percentage <- as.numeric(flake_u2$dorsal_cortex_percentage)
flake_u2$number_of_dorsal_scars<- as.numeric(flake_u2$number_of_dorsal_scars)
View(flake_u2)

### Computing "Median and Interquartile range"
str(flake_u2)
attach(flake_u2)
median(flake_u2$mass, na.rm = TRUE)
IQR(flake_u2$mass, na.rm = TRUE)

median(flake_u2$max_dimension, na.rm = TRUE)
IQR(flake_u2$max_dimension, na.rm = TRUE)

median(flake_u2$length, na.rm = TRUE)
IQR(flake_u2$length, na.rm = TRUE)

median(flake_u2$width, na.rm = TRUE)
IQR(flake_u2$width, na.rm = TRUE)

median(flake_u2$thickness, na.rm = TRUE)
IQR(flake_u2$thickness, na.rm = TRUE)

median(flake_u2$number_of_dorsal_scars, na.rm = TRUE)
IQR(flake_u2$number_of_dorsal_scars, na.rm = TRUE)

median(flake_u2$platform_width, na.rm = TRUE)
IQR(flake_u2$platform_width, na.rm = TRUE)

median(flake_u2$platform_thickness, na.rm = TRUE)
IQR(flake_u2$platform_thickness, na.rm = TRUE)

median(flake_u2$external_platform_angle, na.rm = TRUE)
IQR(flake_u2$external_platform_angle, na.rm = TRUE)

median(flake_u2$internal_platform_angle, na.rm = TRUE)
IQR(flake_u2$internal_platform_angle, na.rm = TRUE)

## Lets check the "Median and Interquartile range" -  U2_1 - Quartzite - group
# U2_1
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2_1 <- read_excel("flake.xlsx", sheet = 'u2_1')
str(flake_u2_1)
attach(flake_u2_1)
View(flake_u2_1)

# To convert metric variables to as.numeric
flake_u2_1$mass <- as.numeric(flake_u2_1$mass)
flake_u2_1$max_dimension <- as.numeric(flake_u2_1$max_dimension)
flake_u2_1$length <- as.numeric(flake_u2_1$length)
flake_u2_1$width <- as.numeric(flake_u2_1$width)
flake_u2_1$thickness <- as.numeric(flake_u2_1$thickness)
flake_u2_1$platform_width <- as.numeric(flake_u2_1$platform_width)
flake_u2_1$platform_thickness <- as.numeric(flake_u2_1$platform_thickness)
flake_u2_1$external_platform_angle <- as.numeric(flake_u2_1$external_platform_angle)
flake_u2_1$internal_platform_angle<- as.numeric(flake_u2_1$internal_platform_angle)
flake_u2_1$dorsal_cortex_percentage <- as.numeric(flake_u2_1$dorsal_cortex_percentage)
flake_u2_1$number_of_dorsal_scars<- as.numeric(flake_u2_1$number_of_dorsal_scars)
View(flake_u2_1)

### Computing "Median and Interquartile range" - U1_1
str(flake_u2_1)
attach(flake_u2_1)
median(flake_u2_1$mass, na.rm = TRUE)
IQR(flake_u2_1$mass, na.rm = TRUE)

median(flake_u2_1$max_dimension, na.rm = TRUE)
IQR(flake_u2_1$max_dimension, na.rm = TRUE)

median(flake_u2_1$length, na.rm = TRUE)
IQR(flake_u2_1$length, na.rm = TRUE)

median(flake_u2_1$width, na.rm = TRUE)
IQR(flake_u2_1$width, na.rm = TRUE)

median(flake_u2_1$thickness, na.rm = TRUE)
IQR(flake_u2_1$thickness, na.rm = TRUE)

median(flake_u2_1$number_of_dorsal_scars, na.rm = TRUE)
IQR(flake_u2_1$number_of_dorsal_scars, na.rm = TRUE)

median(flake_u2_1$platform_width, na.rm = TRUE)
IQR(flake_u2_1$platform_width, na.rm = TRUE)

median(flake_u2_1$platform_thickness, na.rm = TRUE)
IQR(flake_u2_1$platform_thickness, na.rm = TRUE)

median(flake_u2_1$external_platform_angle, na.rm = TRUE)
IQR(flake_u2_1$external_platform_angle, na.rm = TRUE)

median(flake_u2_1$internal_platform_angle, na.rm = TRUE)
IQR(flake_u2_1$internal_platform_angle, na.rm = TRUE)


## Lets check the "Median and Interquartile range" - U2_2 - Orthophyre group
# U2_2
library(ggplot2)
library(dplyr)



library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2_2 <- read_excel("flake.xlsx", sheet = 'u2_2')
str(flake_u2_2)
attach(flake_u2_2)
View(flake_u2_2)

# To convert metric variables to as.numeric
flake_u2_2$mass <- as.numeric(flake_u2_2$mass)
flake_u2_2$max_dimension <- as.numeric(flake_u2_2$max_dimension)
flake_u2_2$length <- as.numeric(flake_u2_2$length)
flake_u2_2$width <- as.numeric(flake_u2_2$width)
flake_u2_2$thickness <- as.numeric(flake_u2_2$thickness)
flake_u2_2$platform_width <- as.numeric(flake_u2_2$platform_width)
flake_u2_2$platform_thickness <- as.numeric(flake_u2_2$platform_thickness)
flake_u2_2$external_platform_angle <- as.numeric(flake_u2_2$external_platform_angle)
flake_u2_2$internal_platform_angle<- as.numeric(flake_u2_2$internal_platform_angle)
flake_u2_2$dorsal_cortex_percentage <- as.numeric(flake_u2_2$dorsal_cortex_percentage)
flake_u2_2$number_of_dorsal_scars<- as.numeric(flake_u2_2$number_of_dorsal_scars)
View(flake_u2_2)

### Computing "Median and Interquartile range" - U1_1
str(flake_u2_2)
attach(flake_u2_2)
median(flake_u2_2$mass, na.rm = TRUE)
IQR(flake_u2_2$mass, na.rm = TRUE)

median(flake_u2_2$max_dimension, na.rm = TRUE)
IQR(flake_u2_2$max_dimension, na.rm = TRUE)

median(flake_u2_2$length, na.rm = TRUE)
IQR(flake_u2_2$length, na.rm = TRUE)

median(flake_u2_2$width, na.rm = TRUE)
IQR(flake_u2_2$width, na.rm = TRUE)

median(flake_u2_2$thickness, na.rm = TRUE)
IQR(flake_u2_2$thickness, na.rm = TRUE)

median(flake_u2_2$number_of_dorsal_scars, na.rm = TRUE)
IQR(flake_u2_2$number_of_dorsal_scars, na.rm = TRUE)

median(flake_u2_2$platform_width, na.rm = TRUE)
IQR(flake_u2_2$platform_width, na.rm = TRUE)

median(flake_u2_2$platform_thickness, na.rm = TRUE)
IQR(flake_u2_2$platform_thickness, na.rm = TRUE)

median(flake_u2_2$external_platform_angle, na.rm = TRUE)
IQR(flake_u2_2$external_platform_angle, na.rm = TRUE)

median(flake_u2_2$internal_platform_angle, na.rm = TRUE)
IQR(flake_u2_2$internal_platform_angle, na.rm = TRUE)


## Lets check the "Median and Interquartile range" - U2_3 - Basalt group
# U2_3
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2_3 <- read_excel("flake.xlsx", sheet = 'u2_3')
str(flake_u2_3)
attach(flake_u2_3)
View(flake_u2_3)

# To convert metric variables to as.numeric
flake_u2_3$mass <- as.numeric(flake_u2_3$mass)
flake_u2_3$max_dimension <- as.numeric(flake_u2_3$max_dimension)
flake_u2_3$length <- as.numeric(flake_u2_3$length)
flake_u2_3$width <- as.numeric(flake_u2_3$width)
flake_u2_3$thickness <- as.numeric(flake_u2_3$thickness)
flake_u2_3$platform_width <- as.numeric(flake_u2_3$platform_width)
flake_u2_3$platform_thickness <- as.numeric(flake_u2_3$platform_thickness)
flake_u2_3$external_platform_angle <- as.numeric(flake_u2_3$external_platform_angle)
flake_u2_3$internal_platform_angle<- as.numeric(flake_u2_3$internal_platform_angle)
flake_u2_3$dorsal_cortex_percentage <- as.numeric(flake_u2_3$dorsal_cortex_percentage)
flake_u2_3$number_of_dorsal_scars<- as.numeric(flake_u2_3$number_of_dorsal_scars)
View(flake_u2_3)

### Computing "Median and Interquartile range" - U1_1
str(flake_u2_3)
attach(flake_u2_3)
median(flake_u2_3$mass, na.rm = TRUE)
IQR(flake_u2_3$mass, na.rm = TRUE)

median(flake_u2_3$max_dimension, na.rm = TRUE)
IQR(flake_u2_3$max_dimension, na.rm = TRUE)

median(flake_u2_3$length, na.rm = TRUE)
IQR(flake_u2_3$length, na.rm = TRUE)

median(flake_u2_3$width, na.rm = TRUE)
IQR(flake_u2_3$width, na.rm = TRUE)

median(flake_u2_3$thickness, na.rm = TRUE)
IQR(flake_u2_3$thickness, na.rm = TRUE)

median(flake_u2_3$number_of_dorsal_scars, na.rm = TRUE)
IQR(flake_u2_3$number_of_dorsal_scars, na.rm = TRUE)

median(flake_u2_3$platform_width, na.rm = TRUE)
IQR(flake_u2_3$platform_width, na.rm = TRUE)

median(flake_u2_3$platform_thickness, na.rm = TRUE)
IQR(flake_u2_3$platform_thickness, na.rm = TRUE)

median(flake_u2_3$external_platform_angle, na.rm = TRUE)
IQR(flake_u2_3$external_platform_angle, na.rm = TRUE)

median(flake_u2_3$internal_platform_angle, na.rm = TRUE)
IQR(flake_u2_3$internal_platform_angle, na.rm = TRUE)



####### Ngay 19-11-2019
###-------------------------------------DIMENSIONS----------------------------------------------
### Means/Dimensions (Mass/Max/Length/Width/Thickness)
### Chi-Square (Mass and Raw materials) Unit 1+2
# Trang 197
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake <- read_excel("flake.xlsx", sheet = 'flakes')
View(flake)
# Mass and State of preseravation over time (Unit 1+2), B1
library(ggplot2)
attach(flake)
str(flake)
flake$mass <- as.numeric(flake$mass)
ggplot(flake, aes(x = reorder(total_material, mass),
                  y = mass)) + 
  geom_boxplot(width = 0.6, size = 0.5, fatten = 0.5) +
  geom_jitter(alpha = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Mass (g)") + 
  xlab("") +
  ggtitle("Unit 1 and Unit 2") +
  scale_y_log10() +
  coord_flip()

## Chi-square to see wether or not it has a strong correlation?
attach(flake)
View(flake)
tabl1 = table(flake$mass, flake$total_material)
tabl1 
chisq.test(tabl1)


### Chi-Square (Max and Raw materials) Unit 1+2
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake <- read_excel("flake.xlsx", sheet = 'flakes')
# Mass and State of preseravation over time (Unit 1+2), B1
library(ggplot2)
attach(flake)
str(flake)
flake$max_dimension <- as.numeric(flake$max_dimension)
ggplot(flake, aes(x = reorder(total_material, max_dimension),
                  y = max_dimension)) + 
  geom_boxplot(width = 0.6, size = 0.8, fatten = 0.9) +
  geom_jitter(alpha = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Max dimension (mm)") + 
  xlab("") +
  ggtitle("Unit 1 and Unit 2") +
  scale_y_log10() +
  coord_flip()

## Chi-square to see wether or not it has a strong correlation?
attach(flake)
tabl1 = table(flake$max_dimension, flake$total_material)
tabl1 
chisq.test(tabl1)


### Chi-Square (Length and Raw materials) Unit 1+2
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake <- read_excel("flake.xlsx", sheet = 'flakes')
# Mass and State of preseravation over time (Unit 1+2), B1
library(ggplot2)
attach(flake)
str(flake)
flake$length <- as.numeric(flake$length)
ggplot(flake, aes(x = reorder(total_material, length),
                  y = length)) + 
  geom_boxplot(width = 0.6, size = 0.8, fatten = 0.9) +
  geom_jitter(alpha = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Length (mm)") + 
  xlab("") +
  ggtitle("Unit 1 and Unit 2") +
  scale_y_log10() +
  coord_flip()

## Chi-square to see wether or not it has a strong correlation?
attach(flake)
tabl1 = table(flake$length, flake$total_material)
tabl1 
chisq.test(tabl1)


### Chi-Square (Width and Raw materials) Unit 1+2
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake <- read_excel("flake.xlsx", sheet = 'flakes')
# Mass and State of preseravation over time (Unit 1+2), B1
library(ggplot2)
attach(flake)
str(flake)
flake$width <- as.numeric(flake$width)
ggplot(flake, aes(x = reorder(total_material, width),
                  y = width)) + 
  geom_boxplot(width = 0.6, size = 0.8, fatten = 0.9) +
  geom_jitter(alpha = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Width (mm)") + 
  xlab("") +
  ggtitle("Unit 1 and Unit 2") +
  scale_y_log10() +
  coord_flip()
## Chi-square to see wether or not it has a strong correlation?
attach(flake)
tabl1 = table(flake$width, flake$total_material)
tabl1 
chisq.test(tabl1)


### Chi-Square (Thickness and Raw materials) Unit 1+2
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake <- read_excel("flake.xlsx", sheet = 'flakes')

# Mass and State of preseravation over time (Unit 1+2), B1
library(ggplot2)
attach(flake)
str(flake)
flake$thickness <- as.numeric(flake$thickness)
ggplot(flake, aes(x = reorder(total_material, thickness),
                  y = thickness)) + 
  geom_boxplot(width = 0.6, size = 0.8, fatten = 0.9) +
  geom_jitter(alpha = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Thickness (mm)") + 
  xlab("") +
  ggtitle("Unit 1 and Unit 2") +
  scale_y_log10() +
  coord_flip()
## Chi-square to see wether or not it has a strong correlation?
attach(flake)
tabl1 = table(flake$thickness, flake$total_material)
tabl1 
chisq.test(tabl1)


###---------------------------------------------------------------------
## Mean Mass with three types of Raw materials in Uit 1
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
summary(flake_u1)
table(flake_u1$flake_type)
class(flake_u1$length)
# Mass and State of preseravation over time (Unit 1), B1
library(ggplot2)
attach(flake_u1)
str(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
ggplot(flake_u1, aes(x = reorder(total_material, mass),
                     y = mass)) + 
  geom_boxplot(width = 0.6, size = 0.6, fatten = 0.6) +
  geom_jitter(alpha = 0.25) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Mass (g)") + 
  xlab("") +
  ggtitle("Unit 1") +
  scale_y_log10()


## Mean Mass with three types of Raw materials in Uit 2
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
# Mass and State of preseravation over time (Unit 2), B1
library(ggplot2)
attach(flake_u1)
str(flake_u1)
flake_u1$max_dimension <- as.numeric(flake_u1$max_dimension)
ggplot(flake_u1, aes(x = reorder(total_material, max_dimension),
                     y = mass)) + 
  geom_boxplot(width = 0.7, size = 0.6, fatten = 0.6) +
  geom_jitter(alpha = 0.25) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Max Dimension (mm)") + 
  xlab("") +
  ggtitle("Unit 1") +
  scale_y_log10()
## Chi.square test for Mass/State of preservation
attach(flake_u1)
View(flake_u1)
tabl1 = table(flake_u1$max_dimension, flake_u1$total_material)
tabl1 
chisq.test(tabl1)


# Unit 1+2
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
library(tidyr)
library(microbenchmark)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
## Chi.square test for Mass/State of preservation
attach(flake_u1)
View(flake_u1)
tabl1 = table(flake_u1$mass, flake_u1$total_material)
tabl1 
chisq.test(tabl1)
## Anova to test there was a different or not between three main types of raw materials and mass in Unit 1
attach(flake_u1)
str(flake_u1)
av = aov(mass ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)



# Ngay 22-11-2019
### DIMENSIONS ---------------------------------------------------------
# Unit 1 - Mass
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
library(tidyr)
library(microbenchmark)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)

# Mass and Raw Material ------------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
flake_u1 %>% na.omit()
# inspect data
str(flake_u1)
names(flake_u1) <- make.names(names(flake_u1))
# Compute ANOVA
mass_total_material_aov <- aov(mass ~ total_material, data = flake_u1)
mass_total_material_aov_tidy <- tidy(mass_total_material_aov)
# Compute post-hoc test
mass_total_material_aov_posthoc <- tidy(TukeyHSD(mass_total_material_aov))
# Plot
mass_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Mass (g)") +
  ylab("Estimate") +
  ggtitle("Unit 1") +
  labs(colour = "Significant difference") 
### P_value of Mass
mass_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(mass ~ total_material, data = flake_u1)
summary(results)
## Chi.square test for Mass/State of preservation
attach(flake_u1)
View(flake_u1)
tabl1 = table(flake_u1$mass, flake_u1$total_material)
tabl1 
chisq.test(tabl1)
## Anova to test there was a different or not between three main types of raw materials and mass in Unit 1
attach(flake_u1)
str(flake_u1)
av = aov(mass ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# Max and Raw Material ------------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$max_dimension <- as.numeric(flake_u1$max_dimension)
flake_u1 %>% na.omit()
# inspect data
str(flake_u1)
names(flake_u1) <- make.names(names(flake_u1))
# Compute ANOVA
max_dimension_total_material_aov <- aov(max_dimension ~ total_material, data = flake_u1)
max_dimension_total_material_aov_tidy <- tidy(max_dimension_total_material_aov)
# Compute post-hoc test
max_dimension_total_material_aov_posthoc <- tidy(TukeyHSD(max_dimension_total_material_aov))
# Plot
max_dimension_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Max Dimension (g)") +
  ylab("Estimate") +
  ggtitle("Unit 1") +
  labs(colour = "Significant difference") 
### P_value of Mass
max_dimension_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(max_dimension ~ total_material, data = flake_u1)
summary(results)
## Chi.square test for Mass/State of preservation
attach(flake_u1)
View(flake_u1)
tabl1 = table(flake_u1$max_dimension, flake_u1$total_material)
tabl1 
chisq.test(tabl1)

## Anova to test there was a different or not between three main types of raw materials and mass in Unit 1
attach(flake_u1)
str(flake_u1)
av = aov(max_dimension ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)
library(ggplot2)
attach(flake_u1)
str(flake_u1)
# Ploting Bxoplot Max Dimension/Raw Materials
flake_u1$max_dimension <- as.numeric(flake_u1$max_dimension)
ggplot(flake_u1, aes(x = reorder(total_material, max_dimension),
                     y = max_dimension)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Max Dimension (mm)") + 
  xlab("") +
  ggtitle("Unit 1") +
  scale_y_log10()
## Anova for Lenght/Raw Material types
attach(flake_u1)
str(flake_u1)
av = aov(max_dimension ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# Length and Raw Material ----------------------------------------------
# Chi-square Length/Raw Material
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
library(psych)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
names(flake_u1)
attach(flake_u1)
require(psych)
describe(width, length)
require(beeswarm)
beeswarm(length ~ flake_inititation_type, data = flake_u1, color = 16, pch = 20, width = 3)
boxplot(length ~ flake_inititation_type, add = F, data = flake_u1, size = 9)
t.test(length ~ flake_inititation_type)


## Chi.square test for Mass/State of preservation
describe(context, width)
tabl1 = table(flake_u1$length, flake_u1$total_material)
tabl1 
t.test(tabl1)

## Plotting 
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$length <- as.numeric(flake_u1$length)
flake_u1 %>% na.omit()
# inspect data
str(flake_u1)
names(flake_u1) <- make.names(names(flake_u1))
# Compute ANOVA
length_total_material_aov <- aov(length ~ total_material, data = flake_u1)
length_total_material_aov_tidy <- tidy(length_total_material_aov)
# Compute post-hoc test
length_total_material_aov_posthoc <- tidy(TukeyHSD(length_total_material_aov))
# Plot
length_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Length (mm)") +
  ylab("Estimate") +
  ggtitle("Unit 1") +
  labs(colour = "Significant difference") 


### P_value of Mass
length_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(length ~ total_material, data = flake_u1)
summary(results)
## Boxplot of Length and Raw Materials
library(ggplot2)
attach(flake_u1)
str(flake_u1)
flake_u1$length <- as.numeric(flake_u1$length)
ggplot(flake_u1, aes(x = reorder(total_material, length),
                     y = length)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.1) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Legnth (mm)") + 
  xlab("") +
  ggtitle("Unit 1") +
  scale_y_log10()

## Anova for Lenght / Raw Material types
attach(flake_u1)
str(flake_u1)
av = aov(length ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


### Width and Raw_materials---------------------------------------------
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
## Chi.square test for Mass/State of preservation
attach(flake_u1)
str(flake_u1)
tabl1 = table(flake_u1$width, flake_u1$total_material)
tabl1 
chisq.test(tabl1)
## Anova for Width/Raw Material types
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
flake_u1 %>% na.omit()
# inspect data
str(flake_u1)
names(flake_u1) <- make.names(names(flake_u1))
# Compute ANOVA
width_total_material_aov <- aov(width ~ total_material, data = flake_u1)
width_total_material_aov_tidy <- tidy(width_total_material_aov)
# Compute post-hoc test
width_total_material_aov_posthoc <- tidy(TukeyHSD(width_total_material_aov))
# Plot
width_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/12) +
  theme(legend.position = "top") +
  xlab("Width/Raw Material") +
  ylab("Estimate") +
  ggtitle("Unit 1") +
  labs(colour = "Significant difference")
### P_value of Mass
width_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(width ~ total_material, data = flake_u1)
summary(results)
# Mass and Raw material of Unit 1
av = aov(width ~ total_material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)
## Boxplot for Width and Raw Materials
library(ggplot2)
attach(flake_u1)
str(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
ggplot(flake_u1, aes(x = reorder(total_material, width),
                     y = width)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Width (mm)") + 
  xlab("") +
  ggtitle("Unit 1") +
  scale_y_log10()



### Thickness and Raw_materials----------------------------------------
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
## Chi.square test for Mass/State of preservation
attach(flake_u1)
str(flake_u1)
tabl1 = table(flake_u1$thickness, flake_u1$total_material)
tabl1 
chisq.test(tabl1)
## A shortcut way
str(split_rivercobbles)
attach(split_rivercobbles)
av = aov(thickness ~ raw_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)
## Boxplot for Thickness and Raw Materials
library(ggplot2)
attach(flake_u1)
str(flake_u1)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
ggplot(flake_u1, aes(x = reorder(total_material, thickness),
                     y = thickness)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Thickness (mm)") + 
  xlab("") +
  ggtitle("Unit 1") +
  scale_y_log10()

# Anova for Thickness/Raw Materials
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
View(flake_u1)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
flake_u1 %>% na.omit()
# inspect data
str(flake_u1)
names(flake_u1) <- make.names(names(flake_u1))
# Compute ANOVA
thickness_total_material_aov <- aov(thickness ~ total_material, data = flake_u1)
thickness_total_material_aov_tidy <- tidy(thickness_total_material_aov)
# Compute post-hoc test
thickness_total_material_aov_posthoc <- tidy(TukeyHSD(thickness_total_material_aov))
# Plot
width_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/12) +
  theme(legend.position = "top") +
  xlab("Thickness/Raw Material") +
  ylab("Estimate") +
  ggtitle("Unit 1") +
  labs(colour = "Significant difference")
### P_value of Mass
thickness_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(thickness ~ total_material, data = flake_u1)
summary(results)
# Mass and Raw material of Unit 1
av = aov(thickness ~ total_material)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# STATES OF FLAKES - Unit1-------------------------------
# States with Mass---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
ggplot(flake_u1, aes(x=reorder(state_of_preservation, mass),
                     y = mass)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Mass (g)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()
# Anova of States with Mass- Unit 1
str(flake_u1)
attach(flake_u1)
av = aov(mass ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# States with Max---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$length <- as.numeric(flake_u1$length)
ggplot(flake_u1, aes(x=reorder(state_of_preservation, length),
                     y = length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 30, angle= 270, hjust = 1)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1")

# Anova of States with Max- Unit 1
str(flake_u1)
attach(flake_u1)
av = aov(max_dimension ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# States with Length---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$length <- as.numeric(flake_u1$length)
ggplot(flake_u1, aes(x=reorder(state_of_preservation, length),
                     y = length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()

# Anova of States with Length- Unit 1
str(flake_u1)
attach(flake_u1)
av = aov(length ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)



# States with Width---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
ggplot(flake_u1, aes(x=reorder(state_of_preservation, width),
                     y = length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Width (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()
# Anova of States with Width- Unit 1
str(flake_u1)
attach(flake_u1)
av = aov(width ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# States with Thickness---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
ggplot(flake_u1, aes(x=reorder(state_of_preservation, thickness),
                     y = thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Thickness (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()
# Anova of States with Thickness - Unit 1
str(flake_u1)
attach(flake_u1)
av = aov(thickness ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


# States with Platform Width---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$platform_width <- as.numeric(flake_u1$platform_width)
ggplot(flake_u1, aes(x=reorder(state_of_preservation, platform_width),
                     y = platform_width)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Platform Width (mm)") +
  scale_y_log10() +
  ggtitle("Unit 1") +
  coord_flip()


# Uunit 1- Correlations -------------------------------
# To test the "Correlation Coeffiection of Dimensions"
# Mass/Max
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
flake_u1$max_dimension <- as.numeric(flake_u1$max_dimension)
ggscatter(flake_u1 , x = "max_dimension", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 300, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Max dimension (mm)") +
  ylab("Mass (g)")

# Mass/Length
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
flake_u1$length <- as.numeric(flake_u1$length)
ggscatter(flake_u1 , x = "length", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 300, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Length (mm)") +
  ylab("Mass (g)")


# Mass / Width
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
flake_u1$width <- as.numeric(flake_u1$width)
ggscatter(flake_u1 , x = "width", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 300, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Width (mm)") +
  ylab("Mass (g)")


# Mass / Thickness
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$mass <- as.numeric(flake_u1$mass)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
ggscatter(flake_u1 , x = "thickness", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 300, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Thickness (mm)") +
  ylab("Mass (g)")


# Length / Max
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$max_dimension <- as.numeric(flake_u1$max_dimension)
flake_u1$length <- as.numeric(flake_u1$length)
ggscatter(flake_u1 , x = "length", y = "max_dimension",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 200, size=13) +
  geom_point(alpha = 1, size = 4, fill = "black" , color = "black", alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Length (mm)") +
  ylab("Max dimension (mm)")

# Length / Width
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
flake_u1$length <- as.numeric(flake_u1$length)
ggscatter(flake_u1 , x = "length", y = "width",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 150, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Length (mm)") +
  ylab("Width (mm)")


# Length / Thickness
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
flake_u1$length <- as.numeric(flake_u1$length)
ggscatter(flake_u1 , x = "length", y = "thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 60, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Length (mm)") +
  ylab("Thickness (mm)")


# Width / Mass
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
flake_u1$mass <- as.numeric(flake_u1$mass)
ggscatter(flake_u1 , x = "mass", y = "width",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 150, size=16) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Mass (g)") +
  ylab("Width (mm)")


# Width / Max
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
flake_u1$max_dimension <- as.numeric(flake_u1$max_dimension)
ggscatter(flake_u1 , x = "width", y = "max_dimension",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 160, size=15) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Width (mm)") +
  ylab("Max dimension (mm)")


# Width / Thickness
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$width <- as.numeric(flake_u1$width)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
ggscatter(flake_u1 , x = "width", y = "thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 60, size=14) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Width (mm)") +
  ylab("Thickness (mm)")


# Max / Thickness
flake_u1 <- read_excel("flake.xlsx", sheet = 'u1')
str(flake_u1)
attach(flake_u1)
flake_u1$max_dimension <- as.numeric(flake_u1$max_dimension)
flake_u1$thickness <- as.numeric(flake_u1$thickness)
ggscatter(flake_u1 , x = "max_dimension", y = "thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 60, size=14) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 1") +
  xlab("Thickness (mm)") +
  ylab("Max dimension (mm)")


#### Dimensions-----------------------------------------------------
#### Unit_2
## MASS - RAW MATERIALS - ok
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
ggplot(flake_u2, aes(x = reorder(state_of_preservation, mass),
                     y = mass, fill = total_material)) + 
  geom_boxplot(width = 0.8, size = 0.6, fatten = 0.3, outlier.shape = NA) +
  geom_point(alpha = 0.09) +
  theme_bw() +
  theme(text = element_text(size=25),
        axis.text.y = element_text(size = 16, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 16, angle = 0, hjust=.5)) +
  scale_y_log10() +
  ylab("Mass (g)") + 
  xlab("") +
  ggtitle("Unit 2") +
  labs(fill = "") +
  coord_flip()



# Ngay 22-11-2019
# DIMENSIONS ---------------------------------------------------------
# Unit 2 - Mass
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
library(tidyr)
library(microbenchmark)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u1)
flake_u2$mass <- as.numeric(flake_u2$mass)
# Mass and Raw Material ------------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
names(flake_u21) <- make.names(names(flake_u2))
# Compute ANOVA
mass_total_material_aov <- aov(mass ~ total_material, data = flake_u2)
mass_total_material_aov_tidy <- tidy(mass_total_material_aov)
# Compute post-hoc test
mass_total_material_aov_posthoc <- tidy(TukeyHSD(mass_total_material_aov))
# Plot
mass_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Mass (g)") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference") 
### P_value of Mass
mass_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(mass ~ total_material, data = flake_u2)
summary(results)
## Anova to test there was a different or not between three main types of raw materials and mass in Unit 1
attach(flake_u2)
str(flake_u2)
av = aov(mass ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# Boxploting for Mass / Raw Materials
flake_u2$mass <- as.numeric(flake_u2$mass)
ggplot(flake_u2, aes(x = reorder(total_material, mass),
                     y = max_dimension)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 1) +
  geom_jitter(alpha = 0.12) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Mass (g)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()
## Chi.square test for Mass/State of preservation
attach(flake_u2)
View(flake_u2)
tabl1 = table(flake_u2$mass, flake_u2$total_material)
tabl1 
cor(tabl1)



# Max and Raw Material ------------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
names(flake_u2) <- make.names(names(flake_u2))
# Compute ANOVA
max_dimension_total_material_aov <- aov(max_dimension ~ total_material, data = flake_u2)
max_dimension_total_material_aov_tidy <- tidy(max_dimension_total_material_aov)
# Compute post-hoc test
max_dimension_total_material_aov_posthoc <- tidy(TukeyHSD(max_dimension_total_material_aov))
# Plot
max_dimension_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Max Dimension (mm)") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference") 
### P_value of Mass
max_dimension_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(max_dimension ~ total_material, data = flake_u2)
summary(results)
## Chi.square test for Mass/State of preservation
attach(flake_u2)
View(flake_u2)
tabl1 = table(flake_u2$max_dimension, flake_u2$total_material)
tabl1 
cor(tabl1)


# Ploting Boxplot Max Dimension/Raw Materials
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
ggplot(flake_u2, aes(x = reorder(total_material, max_dimension),
                     y = max_dimension)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Max Dimension (mm)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()
## Anova for Lenght/Raw Material types
attach(flake_u2)
str(flake_u2)
av = aov(max_dimension ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


# Length and Raw Material ----------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$length <- as.numeric(flake_u2$length)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
names(flake_u2) <- make.names(names(flake_u2))
# Compute ANOVA
length_total_material_aov <- aov(length ~ total_material, data = flake_u2)
length_total_material_aov_tidy <- tidy(length_total_material_aov)
# Compute post-hoc test
length_total_material_aov_posthoc <- tidy(TukeyHSD(length_total_material_aov))
# Plot
length_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/10) +
  theme(legend.position = "top") +
  xlab("Length (mm)") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference") 
### P_value of Mass
length_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(length ~ total_material, data = flake_u2)
summary(results)
## Anova for Lenght / Raw Material types
attach(flake_u2)
str(flake_u2)
av = aov(length ~ total_material)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)

## Boxplot of Length and Raw Materials
library(ggplot2)
attach(flake_u2)
str(flake_u2)
flake_u2$length <- as.numeric(flake_u2$length)
ggplot(flake_u2, aes(x = reorder(total_material, length),
                     y = length)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.1) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Legnth (mm)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()

# Chi-square Length/Raw Material
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$length <- as.numeric(flake_u2$length)
## Chi.square test for Mass/State of preservation
attach(flake_u2)
tabl1 = table(flake_u2$length, flake_u2$total_material)
tabl1 
cor(tabl1)


### Width and Raw_materials---------------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
library("readxl")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$width <- as.numeric(flake_u2$width)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
attach(flake_u2)
names(flake_u2) <- make.names(names(flake_u2))
# Compute ANOVA
width_total_material_aov <- aov(width ~ total_material, data = flake_u2)
width_total_material_aov_tidy <- tidy(width_total_material_aov)
# Compute post-hoc test
width_total_material_aov_posthoc <- tidy(TukeyHSD(width_total_material_aov))
# Plot
width_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/12) +
  theme(legend.position = "top") +
  xlab("Width/Raw Material") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference")
### P_value of Mass
width_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(width ~ total_material, data = flake_u2)
summary(results)
# Mass and Raw material of Unit 2
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
attach(flake_u2)
av = aov(width ~ total_material)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)

## Boxplot for Width and Raw Materials
library(ggplot2)
attach(flake_u2)
str(flake_u2)
flake_u2$width <- as.numeric(flake_u2$width)
ggplot(flake_u2, aes(x = reorder(total_material, width),
                     y = width)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Width (mm)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()
## Chi.square test for Mass/State of preservation
attach(flake_u2)
str(flake_u2)
tabl2 = table(flake_u2$width, flake_u2$total_material)
tabl2
cor(tabl2)


### Thickness and Raw_materials----------------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
library("readxl")
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
## Chi.square test for Mass/State of preservation
attach(flake_u2)
str(flake_u2)
tabl1 = table(flake_u2$thickness, flake_u2$total_material)
tabl1 
cor(tabl1)


## Boxplot for Thickness and Raw Materials
library(ggplot2)
attach(flake_u2)
str(flake_u2)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
ggplot(flake_u2, aes(x = reorder(total_material, thickness),
                     y = thickness)) + 
  geom_boxplot(width = 0.75, size = 1, fatten = 0.9) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 35, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Thickness (mm)") + 
  xlab("") +
  ggtitle("Unit 2") +
  scale_y_log10()

# Anova for Thickness/Raw Materials
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
flake_u2 %>% na.omit()
# inspect data
str(flake_u2)
names(flake_u2) <- make.names(names(flake_u2))
# Compute ANOVA
thickness_total_material_aov <- aov(thickness ~ total_material, data = flake_u2)
thickness_total_material_aov_tidy <- tidy(thickness_total_material_aov)
# Compute post-hoc test
thickness_total_material_aov_posthoc <- tidy(TukeyHSD(thickness_total_material_aov))
# Plot
width_total_material_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 25, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 6/12) +
  theme(legend.position = "top") +
  xlab("Thickness/Raw Material") +
  ylab("Estimate") +
  ggtitle("Unit 2") +
  labs(colour = "Significant difference")
### P_value of Mass
thickness_total_material_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(thickness ~ total_material, data = flake_u2)
summary(results)
# Mass and Raw material of Unit 1
av = aov(thickness ~ total_material)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)

# Cor.test for Length/Width
library("dplyr")
library("broom")
library("tidyr")
# read in data
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
View(flake_u2)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
flake_u2$length <- as.numeric(flake_u2$length)
flake_u2$width <- as.numeric(flake_u2$width)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
flake_u2 %>% na.omit()
plot(length, width, las = 2, main = "Scatter plot")
cor.test(width, length, method = "pearson")
pairs(flake_u2[, 15:17])
cor(flake_u2[, 15:17], method = "spearman")



# STATES OF FLAKES - Unit2-------------------------------
# States with Mass---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
ggplot(flake_u2, aes(x=reorder(state_of_preservation, mass),
                     y = mass)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Mass (g)") +
  scale_y_log10() +
  ggtitle("Unit 2") +
  coord_flip()
# Anova of States with Mass- Unit 2
str(flake_u2)
attach(flake_u2)
av = aov(mass ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


# States with Max---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
ggplot(flake_u2, aes(x=reorder(state_of_preservation, max_dimension),
                     y = max_dimension)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Max Dimension (mm)") +
  scale_y_log10() +
  ggtitle("Unit 2") +
  coord_flip()
# Anova of States with Max- Unit 2
str(flake_u2)
attach(flake_u2)
av = aov(max_dimension ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


# States with Length---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$length <- as.numeric(flake_u2$length)
ggplot(flake_u2, aes(x=reorder(state_of_preservation, length),
                     y = length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Length (mm)") +
  scale_y_log10() +
  ggtitle("Unit 2") +
  coord_flip()
# Anova of States with Length- Unit 2
str(flake_u2)
attach(flake_u2)
av = aov(length ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


# States with Width---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$width <- as.numeric(flake_u2$width)
ggplot(flake_u2, aes(x=reorder(state_of_preservation, width),
                     y = length)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Width (mm)") +
  scale_y_log10() +
  ggtitle("Unit 2") +
  coord_flip()


# Anova of States with Width- Unit 1
str(flake_u2)
attach(flake_u2)
av = aov(width ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


# States with Thickness---------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
ggplot(flake_u2, aes(x=reorder(state_of_preservation, thickness),
                     y = thickness)) +
  geom_boxplot(size = 1, width = 0.8, flatten = 0.4) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(legend.position="top") +
  xlab("") +
  ylab("Thickness (mm)") +
  scale_y_log10() +
  ggtitle("Unit 2") +
  coord_flip()
# Anova of States with Thickness - Unit 2
str(flake_u2)
attach(flake_u2)
av = aov(thickness ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)

install.packages("devtools")

-------------------------------
  # To test the "Correlation Coeffiection of Dimensions"
  # Mass/Max
  
  library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
ggscatter(flake_u2 , x = "max_dimension", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 200, size=13) +
  geom_point(alpha = 1, size = 5, fill = "white" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Max dimension (mm)") +
  ylab("Mass (g)")


# Mass / Length
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2$length <- as.numeric(flake_u2$length)
ggscatter(flake_u2 , x = "length", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 200, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Length (mm)") +
  ylab("Mass (g)")


# Mass / Width
library(dplyr)
library(readxl)
library(tidyverse)
library(ggforce)
library("broom")
library(here)
library(ggplot2)
library(ggpubr)
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2$width <- as.numeric(flake_u2$width)
ggscatter(flake_u2 , x = "width", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 200, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Width (mm)") +
  ylab("Mass (g)")


# Mass / Thickness
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$mass <- as.numeric(flake_u2$mass)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
ggscatter(flake_u2 , x = "thickness", y = "mass",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 200, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Thickness (mm)") +
  ylab("Mass (g)")


# Length / Max
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
flake_u2$length <- as.numeric(flake_u2$length)
ggscatter(flake_u2 , x = "length", y = "max_dimension",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 100, size=13) +
  geom_point(alpha = 1, size = 4, fill = "black" , color = "black", alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Length (mm)") +
  ylab("Max dimension (mm)")


# Length / Width
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$width <- as.numeric(flake_u2$width)
flake_u2$length <- as.numeric(flake_u2$length)
ggscatter(flake_u2 , x = "length", y = "width",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 100, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Length (mm)") +
  ylab("Width (mm)")


# Length / Thickness
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
flake_u2$length <- as.numeric(flake_u2$length)
ggscatter(flake_u2 , x = "length", y = "thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 60, size=13) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Length (mm)") +
  ylab("Thickness (mm)")


# Width / Mass
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$width <- as.numeric(flake_u2$width)
flake_u2$mass <- as.numeric(flake_u2$mass)
ggscatter(flake_u2 , x = "mass", y = "width",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 100, size=16) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Mass (g)") +
  ylab("Width (mm)")



# Width / Max
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$width <- as.numeric(flake_u2$width)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
ggscatter(flake_u2 , x = "width", y = "max_dimension",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 100, size=15) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Width (mm)") +
  ylab("Max dimension (mm)")


# Width / Thickness
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$width <- as.numeric(flake_u2$width)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
ggscatter(flake_u2 , x = "width", y = "thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 35, size=14) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Width (mm)") +
  ylab("Thickness (mm)")


# Max / Thickness
flake_u2 <- read_excel("flake.xlsx", sheet = 'u2')
str(flake_u2)
attach(flake_u2)
flake_u2$max_dimension <- as.numeric(flake_u2$max_dimension)
flake_u2$thickness <- as.numeric(flake_u2$thickness)
ggscatter(flake_u2 , x = "max_dimension", y = "thickness",
          add = "reg.line",                                 
          conf.int = TRUE,                                
          add.params = list(color = "blue",
                            fill = "black"))+
  stat_cor(method = "pearson", label.x = 5, label.y = 35, size=14) +
  geom_point(alpha = 1, size = 5, fill = "black" , color = "black") +
  geom_smooth(method = "lm") +
  theme_bw() +
  theme_bw(base_size = 30) +
  theme(aspect.ratio = 4/8) +
  ggtitle("Unit 2") +
  xlab("Thickness (mm)") +
  ylab("Max dimension (mm)")



# Methods of Flaking ========================================================
# Ngay 26-11-2019
# Types of Striking platforms

library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_u12 <- read_excel("flake.xlsx", sheet = 'flake1+2')
View(flake_u12)
str(flake_u12)
flake_u12 %>% 
  group_by(`type_of_striking_platforms`) %>%
  tally()
flake_u12_type_of_striking_platforms_tally <- flake_u12 %>% 
  group_by(`type_of_striking_platforms`) %>% 
  tally() %>% 
  filter(`type_of_striking_platforms` != 'NA') %>%  # REMOVE NAs 
  arrange(desc(n))
ggplot(flake_u12_type_of_striking_platforms_tally,
       aes(x = reorder(`type_of_striking_platforms`, n), y = n)) +
  geom_bar(stat="identity", size = 1, 
           fill = "white", color = "black", 
           width = 0.5) +
  theme_bw() +
  theme(text = element_text(size = 30)) +
  xlab("") +
  ylab("Frequency")


# Distribution of Platform Width/Types of Striking Platforms
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_u12 <- read_excel("flake.xlsx", sheet = 'flake1+2')
View(flake_u12)
str(flake_u12)
ggplot(flake_u12 , aes(x = reorder(type_of_striking_platforms, platform_width),
                       y = platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width(mm)") +
  ylim(0,30) +
  ggtitle("Unit 1 and Unit 2") +
  labs(fill = "Flakes")
# Anova of States with Thickness - Unit 1
str(flake_u12)
attach(flake_u12)
flake_u12$platform_width <- as.numeric(flake_u12$platform_width)
av = aov(platform_width ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


# Distribution of Platform Thickness/Types of Striking Platforms
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_u12 <- read_excel("flake.xlsx", sheet = 'flake1+2')
View(flake_u12)
str(flake_u12)
ggplot(flake_u12 , aes(x = reorder(type_of_striking_platforms, platform_thickness),
                       y = platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thikcness (mm)") +
  ylim(0,30) +
  ggtitle("Unit 1 and Unit 2") +
  labs(fill = "Flakes") +
  
  # Anova of States with Thickness - Unit 1
  str(flake_u12)
attach(flake_u12)
flake_u12$platform_thickness <- as.numeric(flake_u12$platform_thickness)
av = aov(platform_thickness ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


#===============================================================================
# Anova for differences of Platform Width/Types of Platforms in Unit 1
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake1 <- read_excel("flake.xlsx", sheet = 'flake1')
View(flake1)
str(flake1)
ggplot(flake1 , aes(x = reorder(type_of_striking_platforms, platform_width),
                    y = platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width (mm)") +
  ylim(0,30) +
  ggtitle("Unit 1") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 1
  str(flake1)
attach(flake1)
flake1$platform_thickness <- as.numeric(flake1$platform_thickness)
av = aov(platform_width ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)

# Anova for differences of Platform Thickness/Types of Platforms in Unit 1
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake1 <- read_excel("flake.xlsx", sheet = 'flake1')
View(flake1)
str(flake1)
ggplot(flake1 , aes(x = reorder(type_of_striking_platforms, platform_thickness),
                    y = platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thikcness (mm)") +
  ylim(0,30) +
  ggtitle("Unit 1") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 1
  str(flake1)
attach(flake1)
flake1$platform_thickness <- as.numeric(flake1$platform_thickness)
av = aov(platform_thickness ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit1 = TukeyHSD(av)
plot(unit1)


#===============================================================================
# Anova for differences of Platform Width/Types of Platforms in Unit 2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake2 <- read_excel("flake.xlsx", sheet = 'flake2')
str(flake2)
ggplot(flake2 , aes(x = reorder(type_of_striking_platforms, platform_width),
                    y = platform_width)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width (mm)") +
  ylim(0,30) +
  ggtitle("Unit 2") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 2
  str(flake2)
attach(flake2)
flake1$platform_width <- as.numeric(flake1$platform_width)
av = aov(platform_width ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)

# Anova for differences of Platform Width/Types of Platforms in Unit 2
ggplot(flake2 , aes(x = reorder(type_of_striking_platforms, platform_thickness),
                    y = platform_thickness)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.8) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 2) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thickness (mm)") +
  ylim(0,25) +
  ggtitle("Unit 2") +
  labs(fill = "Flakes") +
  # Anova of States with Thickness - Unit 2
  str(flake2)
attach(flake2)
flake1$platform_thickness <- as.numeric(flake1$platform_thickness)
av = aov(platform_thickness ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



# Plotting Geom_Point for Platform Width/Platform Thikcness
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_u12 <- read_excel("flake.xlsx", sheet = 'flake1+2')
attach(flake_u12)
str(flake_u12)
ggplot(flake_u12, aes(x = platform_thickness, y = platform_width,
                      fill = type_of_striking_platforms)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_smooth(size = 1, method = "lm", formula = y ~ x+I(x^2)) +
  theme_bw() +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  theme(legend.position="right") +
  xlim(0, 30) +
  ylab("Platform Width (mm)") +
  xlab("Platform Thickness (mm)") +
  labs(fill = "Platforms")
## Chi.square test for Mass/State of preservation
attach(flake_u12)
cor.test(platform_thickness, platform_width, method = "pearson")




#====================================================================
# Comparing Platform Width between Flakes/Split River Cobbles Unit 1+2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
attach(flake_src)
str(flake_src)
ggplot(flake_src , aes(x = reorder(type_of_striking_platforms, platform_width),
                       y = platform_width, fill = flake_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.6, faltten = 0.7, size = 0.6) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Width (mm)") +
  ylim(0,110) +
  ggtitle("") +
  labs(fill = "") 
# Anova of Platform Width/Flake/Split River cobbles - Unit 1+2
str(flake_src)
attach(flake_src)
flake_src$platform_width <- as.numeric(flake_src$platform_width)
av = aov(platform_width ~ flake_type)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



# Comparing Platform Thickness between Flakes/Split River Cobbles Unit 1+2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
View(flake_src)
str(flake_src)
ggplot(flake_src , aes(x = reorder(type_of_striking_platforms, platform_thickness),
                       y = platform_thickness, fill = flake_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.6, faltten = 0.7, size = 0.6) +
  facet_wrap( ~ unit) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 25, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Platform Thikcness (mm)") +
  ylim(0,35) +
  ggtitle("") +
  labs(fill = "") 
# Anova of States with Thickness - Unit 1
str(flake_src)
attach(flake_src)
flake_src$platform_thickness <- as.numeric(flake_src$platform_thickness)
av = aov(platform_thickness ~ flake_type)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



# Platform Angles ==========================================================
## Distribution of "Internal Platform Angle"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
View(flake_src)
str(flake_src)
ggplot(flake_src, aes(x = reorder(type_of_striking_platforms, internal_platform_angle),
                      y = internal_platform_angle, fill = unit)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("Internal Platform Angle") +
  xlab("") +
  ylab("Degree") +
  ylim(0, 150) +
  labs(fill = "") 



# Correlations between Internal/External Platform Angles
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
View(flake_src)
str(flake_src)
plot(internal_platform_angle, external_platform_angle, las = 2, main = "Scatter plot")
cor.test(internal_platform_angle, external_platform_angle, method = "pearson")
pairs(flake_u2[, 15:17])
cor(flake_u2[, 15:17], method = "spearman")


# Correlation test for Internal Platform Angles
View(flake_src)
attach(flake_src)
tabl1 = table(flake_u12$internal_platform_angle, flake_u12$internal_platform_angle)
tabl1 
cor.test(internal_platform_angle, external_platform_angle, method = "pearson")


# Anova of States with Thickness - Unit 1
str(flake_u12)
attach(flake_u12)
flake_u12$platform_thickness <- as.numeric(flake_u12$platform_thickness)
flake_u12$platform_width <- as.numeric(flake_u12$platform_width)
av = aov(internal_platform_angle ~ type_of_striking_platforms)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


## Distribution of "Internal Platform Angles - Types of Flakes"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
flake_src <- read_excel("flake.xlsx", sheet = 'flake_src')
View(flake_src)
str(flake_src)
ggplot(flake_src, aes(x = reorder(type_of_striking_platforms,dorsal_cortex_percentage),
                      y = dorsal_cortex_percentage, fill = unit)) + 
  geom_boxplot(outlier.shape = NA, width = 0.4, faltten = 0.7, size = 0.6) +
  theme_bw() +
  theme(legend.position = "top") +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust= 0.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("External Platform Angle") +
  xlab("") +
  ylab("Degree ") +
  ylim(0, 100) +
  labs(fill = "") 


# Chi-Square for Internal Platform Angles
attach(flake_u12)
tabl1 = table(flake_u12$internal_platform_angle, flake_u12$internal_platform_angle)
tabl1 
chisq.test(tabl1)
# Anova of States with Thickness - Unit 1
str(flake_u12)
attach(flake_u12)
flake_u12$platform_thickness <- as.numeric(flake_u12$platform_thickness)
flake_u12$platform_width <- as.numeric(flake_u12$platform_width)
av = aov(internal_platform_angle ~ state_of_preservation)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



# Ngay 29-11-2019
## Distribution of "Dorsal Cortex Percentage- Types of Flakes"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
View(cortex)
ggplot(cortex, aes(x = reorder(state_of_preservation, dorsal_cortex_percentage),
                   y = dorsal_cortex_percentage, fill = flake_inititation_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5, faltten = 0.7, size = 0.6) +
  facet_wrap(~ unit) +
  theme_bw() +
  theme(legend.position = "right") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 25, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 25, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("Dorsal Cortex Percentage (%)") +
  labs(fill = "") +
  coord_flip()
# Chi-Square for Internal Platform Angles
attach(flake_u12)
tabl1 = table(flake_u12$external_platform_angle, flake_u12$external_platform_angle)
tabl1 
chisq.test(tabl1)
# Anova of States with Thickness - Unit 1
str(flake_u12)
attach(flake_u12)
flake_u12$external_platform_angle <- as.numeric(flake_u12$external_platform_angle)
av = aov(external_platform_angle ~ total_material)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)



## Distribution of "Dorsal Cortex Percentage- Termination of Flakes"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
ggplot(cortex, aes(x = reorder(total_material, cortexpercentage),
                   y = cortexpercentage, fill = flake_inititation_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5, faltten = 0.7, size = 0.6) +
  facet_wrap(~ unit) +
  theme_bw() +
  theme(legend.position = "right") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 20, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("Dorsal Cortex Percentage (%)") +
  labs(fill = "")


## Distribution of "Dorsal Cortex Percentage- Initiation of Flakes"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
ggplot(cortex, aes(x = reorder(total_material, cortexpercentage),
                   y = cortexpercentage, fill = flake_inititation_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5, faltten = 0.7, size = 0.6) +
  facet_wrap(~ unit) +
  theme_bw() +
  theme(legend.position = "right") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 20, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("Dorsal Cortex Percentage (%)") +
  labs(fill = "")



## Distribution of "Dorsal Cortex Percentage- Termination of Flakes"
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
ggplot(cortex, aes(x = reorder(total_material, cortexpercentage),
                   y = cortexpercentage, fill = flake_termination_type)) + 
  geom_boxplot(outlier.shape = NA, width = 0.5, faltten = 0.7, size = 0.6) +
  facet_wrap(~ unit) +
  theme_bw() +
  theme(legend.position = "right") +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 20, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 20, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  ggtitle("") +
  xlab("") +
  ylab("Dorsal Cortex Percentage (%)") +
  labs(fill = "")


# ======================================================================
# Mean of Dorsal Cortex Percentage - Unit 1
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
cortex_u1 <- read_excel("flake.xlsx", sheet = 'cortex_u1')
attach(cortex_u1)
str(cortex_u1) 
cortex_u1 %>% 
  group_by(`localization_of_dosral_cortex`) %>%
  tally()
cortex_u1_localization_of_dosral_cortex_tally <- cortex_u1 %>% 
  group_by(`localization_of_dosral_cortex`) %>% 
  tally() %>% 
  filter(`localization_of_dosral_cortex` != 'NA') %>% 
  arrange(desc(n))
ggplot(cortex_u1_localization_of_dosral_cortex_tally,
       aes(x = reorder(`localization_of_dosral_cortex`, n), y = n)) +
  geom_bar(stat="identity", width = 1, size = 0.6, fill ="white", color = "black") +
  theme_bw()  +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 30, angle = 0, hjust = 0.5)) + # Cach thay doi SIZE cua phong chu o chu thich X-axis
  xlab("") +
  ylab("Frequency") +
  ggtitle("Unit 1") +
  coord_flip()
# Mean of Cortex Percentage
mean(dorsal_cortex_percentage)
sd(dorsal_cortex_percentage)


# ==================================================================
# Mean of Dorsal Cortex Percentage - Unit 2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(readxl)
cortex_u2 <- read_excel("flake.xlsx", sheet = 'cortex_u2')
attach(cortex_u2)
str(cortex_u2) 
cortex_u2 %>% 
  group_by(`localization_of_dosral_cortex`) %>%
  tally()
cortex_u2_localization_of_dosral_cortex_tally <- cortex_u2 %>% 
  group_by(`localization_of_dosral_cortex`) %>% 
  tally() %>% 
  filter(`localization_of_dosral_cortex` != 'NA') %>% 
  arrange(desc(n))
ggplot(cortex_u2_localization_of_dosral_cortex_tally,
       aes(x = reorder(`localization_of_dosral_cortex`, n), y = n)) +
  geom_bar(stat="identity", width = 1, size = 0.6, fill ="white", color = "black") +
  theme_bw()  +
  theme(text = element_text(size=35),
        axis.text.y = element_text(size = 30, angle = 0, hjust = 0),
        axis.text.x = element_text(size = 30, angle = 0, hjust = 0.5)) +
  xlab("") +
  ylab("Frequency") +
  ggtitle("Unit 2") +
  coord_flip()
# Mean of Cortex Percentage
attach(cortex_u2)
str(cortex_u2) 
mean(dorsal_cortex_percentage)
sd(dorsal_cortex_percentage)



#===========================================================
# Dorsal Cortex Locations
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
View(cortex)
str(cortex)
ggplot(cortex) +
  aes(x = context, fill = cortexlocalization) +
  geom_bar(position = "fill", width= 0.9, size = 1, na.rm = TRUE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle= 90, hjust=.5)) +
  theme(legend.position="right") +
  xlab("Excavation contexts") +
  ylab("Percentage (%)") +
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  labs(fill = "Cortex location") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent)



# Termination of Flakes Unit 1+2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
View(cortex)
str(cortex)
ggplot(cortex) +
  aes(x = context, fill = flake_termination_type) +
  geom_bar(position = "fill", width= 0.9, size = 1, na.rm = TRUE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle= 90, hjust=.5)) +
  theme(legend.position="right") +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  labs(fill = "Termination") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent)


# Termination of Flakes Unit 1+2  - Bending
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
bending <- read_excel("flake.xlsx", sheet = 'bending')
attach(bending)
View(bending)
str(bending)
ggplot(bending) +
  aes(x = total_material, fill = flake_termination_type) +
  geom_bar(position = "fill", width= 0.9, size = 1, na.rm = TRUE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='', position=position_fill(vjust=0.5)) +
  facet_wrap(~ unit) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(legend.position="right") +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Bending Initiation") +
  labs(fill = "Termination") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) +
  coord_flip()


# Termination of Flakes Unit 1+2  - Hertzian
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
hertzian <- read_excel("flake.xlsx", sheet = 'hertzian')
attach(hertzian)
View(hertzian)
str(hertzian)
ggplot(hertzian) +
  aes(x = total_material, fill = flake_termination_type) +
  geom_bar(position = "fill", width= 0.9, size = 1, na.rm = TRUE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='', position=position_fill(vjust=0.5)) +
  facet_wrap(~ unit) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(legend.position="right") +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Hertzian Initiation") +
  labs(fill = "Termination") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) +
  coord_flip()


# Initiation of Flakes Unit 1+2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
View(cortex)
str(cortex)
ggplot(cortex) +
  aes(x = context, fill = flake_inititation_type) +
  geom_bar(position = "fill", width= 0.8, size = 1, na.rm = FALSE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle= 90, hjust=.5)) +
  theme(legend.position="right") +
  xlab("") +
  ylab("Percentage") +
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  labs(fill = "Initiation") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) 



# Termination of flakes in unit 1 + 2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u12 <- read_excel("flake.xlsx", sheet = 'flake1+2')
attach(flake_u12)
str(flake_u12)
ggplot(flake_u12) +
  aes(x = context, fill = flake_termination_type) +
  geom_bar(position = "fill", width= 0.8, size = 1, na.rm = FALSE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.5)) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle= 90, hjust=.5)) +
  theme(legend.position="right") +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  labs(fill = "Termination") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) 


# Termination of flakes  according to manin types of Raw Materials in unit 1 + 2
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
flake_u12 <- read_excel("flake.xlsx", sheet = 'flake1+2')
attach(flake_u12)
str(flake_u12)
ggplot(flake_u12) +
  aes(x = total_material, fill = flake_termination_type) +
  geom_bar(position = "fill", width= 0.8, size = 1, na.rm = FALSE) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count', position=position_fill(vjust=0.5)) +
  facet_wrap(~ unit) +
  theme(aspect.ratio = 5/5) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust=.5)) +
  theme(legend.position="right") +
  xlab("") +
  ylab("Percentage (%)") +
  ggtitle("Unit 1 (3-6) and Unit 2 (7-13)") +
  labs(fill = "Termination") +  # Thay cho cach ky hieu trong bang vi du: "Raw_materal" thanh "Raw matrial" o phan "Legend"
  scale_y_continuous(labels = percent) 


# Dorsal cortex pecentage ----------------------------------------------------
# Mean of Dorsal Cortex Percentage per Each Flake
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
str(cortex)
cortex$number_of_dorsal_scars <- as.numeric(cortex$number_of_dorsal_scars)
cortex$dorsal_cortex_percentage <- as.numeric(cortex$dorsal_cortex_percentage)
ggplot(cortex, aes(x = reorder(localization_of_dosral_cortex,
                               dorsal_cortex_percentage,
                               FUN = mean), y = dorsal_cortex_percentage)) + 
  geom_boxplot(colour="lightgrey", alpha=0.5, outlier.shape = NA) +
  geom_point(stat="summary", fun.y="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  geom_jitter(colour="black", alpha=0.0, width=0.1)  +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 90, hjust=.5)) +
  ylim(0, 100) +
  xlab("") +
  ylab("Mean of Dorsal Cortex Percentage (%)")  +
  coord_flip()
# Chi-Square for Internal Platform Angles
attach(cortex)
tabl1 = table(cortex$dorsal_cortex_percentage, cortex$dorsal_cortex_percentage)
tabl1 
chisq.test(tabl1)
# Anova of Dorsal Cortex Percentage
str(cortex)
attach(cortex)
cortex$dorsal_cortex_percentage <- as.numeric(cortex$dorsal_cortex_percentage)
av = aov(dorsal_cortex_percentage ~ localization_of_dosral_cortex)
summary(av)
av
TukeyHSD(av)
unit2 = TukeyHSD(av)
plot(unit2)


#================================================================
# Ngay 02-12-2019
# To see differences between Percetages of Cortexs of Flakes in both Units of 1 and 2
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
library(tidyr)
library(microbenchmark)
cortex_u1 <- read_excel("flake.xlsx", sheet = 'cortex_u1')
attach(cortex_u1)
str(cortex_u1) 
cortex_u1$mass <- as.numeric(cortex_u1$mass)


# Dorsal Cortex Percentage and Cortex Localizations----------------------------------
library("ggplot2")
library("dplyr")
library("broom")
library("tidyr")
# read in data
cortex_u1 <- read_excel("flake.xlsx", sheet = 'cortex_u1')
attach(cortex_u1)
str(cortex_u1)
View(cortex_u1)
cortex_u1 %>% na.omit()
# inspect data
str(cortex_u1)
names(cortex_u1) <- make.names(names(cortex_u1))
# Compute ANOVA
cortexpercentage_cortexlocalization_aov <- aov(cortexpercentage ~ cortexlocalization, data = cortex_u1)
cortexpercentage_cortexlocalization_aov_tidy <- tidy(cortexpercentage_cortexlocalization_aov)
# Compute post-hoc test
cortexpercentage_cortexlocalization_aov_posthoc <- tidy(TukeyHSD(cortexpercentage_cortexlocalization_aov))
# Plot
cortexpercentage_cortexlocalization_aov_posthoc %>% 
  dplyr::select(-term) %>% 
  mutate(`significant difference` = !data.table::between(0, 
                                                         conf.low, 
                                                         conf.high)) %>% 
  ggplot(aes(comparison,
             estimate,
             colour = `significant difference`)) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) +
  theme_bw() +
  theme(text = element_text(size=10),
        axis.text.y = element_text(size = 15, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 10, angle = 90, hjust=.5)) +
  theme(aspect.ratio = 30/100) +
  theme(legend.position = "top") +
  xlab("") +
  ylab("Estimate") +
  ggtitle("") +
  labs(colour = "Significant difference") 
### P_value of Mass
cortexpercentage_cortex_aov_tidy$p.value[1]
### The P_value of Mass
results = aov(cortexpercentage ~ cortexlocalization, data = cortex_u1)
summary(results)
## Chi.square test for Mass/State of preservation
attach(cortex_u1)
tabl1 = table(cortex_u1$cortexpercentage, cortex_u1$cortexlocalization)
tabl1 
chisq.test(tabl1)
## Anova to test there was a different or not between three main types of raw materials and mass in Unit 1
attach(cortex_u1)
str(cortex_u1)
av = aov(cortexpercentage ~ cortexlocalization)
summary(av)
av
TukeyHSD(av)
tk = TukeyHSD(av)
plot(tk)


#=========================================================================================
# Mean of Dorsal Scars per Each Flake
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggforce)
library(scales)
library(readxl)
cortex <- read_excel("flake.xlsx", sheet = 'cortex_dorsal')
attach(cortex)
View(cortex)
str(cortex)
cortex$number_of_dorsal_scars <- as.numeric(cortex$number_of_dorsal_scars)
cortex$cortexpercentage <- as.numeric(cortex$cortexpercentage)
ggplot(cortex, aes(x = reorder(cortexlocalization,
                               number_of_dorsal_scars,
                               FUN = mean), y = number_of_dorsal_scars)) + 
  geom_point(stat="summary", fun.y="mean") + 
  geom_errorbar(stat="summary", fun.data="mean_se", fun.args = list(mult = 1.96), width=0) +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.y = element_text(size = 20, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 20, angle = 0, hjust= .5)) +
  ggtitle("The cortical flakes according to the mean of dorsal scar number") +
  ylim(0,7) +
  xlab("") +
  ylab("Mean of Dorsal Scar Number") +
  coord_flip()

# Kiem tra he so tuong quan tren moi manh tuoc
attach(cortex)
tabl1 = table(cortex$number_of_dorsal_scars, cortex$dorsal_cortex_percentage)
tabl1 
chisq.test(tabl1)
#

library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake <- read_excel("flake.xlsx", sheet = 'flakes')
View(flake)
# Mass and State of preservation over time (Unit 1+2), B1
library(ggplot2)
attach(flake)
str(flake)
flake$mass <- as.numeric(flake$mass)
ggplot(flake, aes(x = unit, y = mass, colors = "material"),
       y = mass) + 
  geom_boxplot(width = 0.6, size = 0.5, fatten = 0.5) +
  geom_jitter(alpha = 0.15) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle= 0, hjust=.5)) +
  theme(aspect.ratio = 6/8) +
  ylab("Mass (g)") + 
  xlab("") +
  ggtitle("Unit 1 and Unit 2") +
  scale_y_log10() +
  coord_flip()

# Subsetting data

library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flake1 <- read_excel("flake.xlsx", sheet = 'Platforms')
View(flake1)
str(flake1)
attach(flake1)
id = c(material)
col1 = c(unit)
names = c("length", "width", "thickness")
x = c(length, width, thickness)
dat2= data.frame(id, col1, names, x)
dat2
attach(dat2)
ggplot(dat2, aes(x = col1, y = x),
       y = x, fill = id) + 
  geom_boxplot(width = 0.35, size = 1, fatten = 0.4, color = "black") +
  geom_jitter(alpha = 0.1, width = 0.22) +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 13/13) +
  theme(legend.position = "top") +
  stat_summary(fun.y="mean", colour="yellow", geom="point", 
               shape=18, size=6,show_guide = FALSE, lable = "mean") +
  
  facet_wrap(~ names) +
  ylab("K?ch thu???c (mm)") + 
  ylim(0, 170) +
  xlab("") +
  ggtitle("") +
  labs(fill = "")



# So s?nh quy m? m???nh tu???c theo : D???U TI?N/C?N V??? CU???I/KH?NG V??? CU???I
library(ggpubr)
library(ggsignif)
library(tidyverse)
library(ggplot2) 
library(dplyr)
library(readxl)
flakes <- read_excel("flake.xlsx", sheet = 'Cortex_Noncortex')
View(flakes)
str(flakes)
attach(flakes)
id = c(flake_type)
col1 = c(unit)
names = c("D?i", "R???ng", "D?y")
x = c(D?i, R???ng, D?y)
dat2= data.frame(id, col1, names, x)
dat2
attach(dat2)
ggplot(dat2, aes(x = id, y = x),
       y = x, fill = names) + 
  geom_boxplot(width = 0.35, size = 1, fatten = 0.4, color = "black") +
  geom_jitter(alpha = 0.1, width = 0.22) +
  stat_summary(fun.y="mean", geom="point") +
  theme_bw() +
  theme(text = element_text(size=30),
        axis.text.y = element_text(size = 30, angle = 0, hjust=.5),
        axis.text.x = element_text(size = 30, angle = 0, hjust=.5)) +
  theme(aspect.ratio = 5/8) +
  theme(legend.position = "top") +
  stat_summary(fun.y="mean", colour="yellow", geom="point", 
               shape=18, size=6,show_guide = FALSE, lable = "mean") +
  
  facet_wrap(~ names) +
  ylab("K?ch thu???c (mm)") + 
  ylim(0, 100) +
  xlab("") +
  ggtitle("") +
  labs(fill = "") +
  coord_flip()
#

