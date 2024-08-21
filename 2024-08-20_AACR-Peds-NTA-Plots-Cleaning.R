library(tidyverse)

setwd(file.path("~","Documents","ALSF"))
NTA_data <- read_csv(file.path(
  "NTA_concentrations_cleaner.csv"
  )
)

head(NTA_data)
names(NTA_data)

my_groups <- c("OSA","HC")

#Making a summary of statistics
NTA_summary <- NTA_data |> 
  filter(Group %in% my_groups) |> 
  group_by(Group) |> 
  summarize(Mean_Size = mean(Size),
            Std_Dev_Size = sd(Size),
            Mean_Concentration = mean(Concentration),
            Std_Dev_Concentration = sd(Concentration)
  )
 view(NTA_summary)
 
 
 #Making size table for plot
 NTA_size <- NTA_data |> 
   filter(Group %in% my_groups) |> 
   group_by(Group) |> 
   select(Size)

 NTA_size   

 #Making concentration table for plot
 NTA_conc <- NTA_data |> 
   filter(Group %in% my_groups) |> 
   group_by(Group) |> 
   select(Concentration)
 
NTA_conc   

 
 #Graphing Size
 NTA_size_bp <- ggplot(
   NTA_size,
   aes(
     x = Group,
     y = Size,
     fill = Group,
     )
 )+ geom_boxplot(lwd = 1, fatten=1) +
   theme_bw() +
   theme(plot.title = element_text(size = 14, face = "bold", hjust=0.5)) +
   scale_fill_manual(values=c("#8a0a25", "#f7ba4d")) +
   labs(
     x = "Condition",
     y = "Size",
     title = "Exosome Size in Healthy Control vs. Osteosarcoma"
   )
 
 NTA_size_bp
 
 #Graphing Concentration
 NTA_conc_bp <- ggplot(
   NTA_conc,
   aes(
     x = Group,
     y = Concentration,
     fill = Group
   )
 )+ geom_boxplot(lwd = 1, fatten=1) +
   theme_bw() +
   theme(plot.title = element_text(size = 14, face = "bold", hjust=0.5)) +
   scale_fill_manual(values=c("#8a0a25", "#f7ba4d")) +
   labs(
     x = "Condition",
     y = "Concentration",
     title = "Exosome Concentration in Healthy Control vs. Osteosarcoma"
   )
 
 NTA_conc_bp
 
 #Saving Plots
 ggsave( #Saving size plot
   plot = NTA_size_bp,
   filename = file.path("NTA_size_bp.png"),
   width = 6,
   height = 6
 )
 
 ggsave( #Saving concentration plot
   plot = NTA_conc_bp,
   filename = file.path("NTA_conc_bp.png"),
   width = 6,
   height = 6
 )
 