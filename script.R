library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(viridis)
library(ggrepel)

folder_for_plots <- "C:/Users/lanzoan1/Downloads/pubmed_omics/plots/"
folder_for_data <- "C:/Users/lanzoan1/Downloads/pubmed_omics/data/"

setwd(folder_for_data)
list_files <- list.files(pattern = "PubMed_Timeline*")
data_long <- data.frame()

for (file in list_files) {
  temp_data_long <- fread(file)
  term <- unlist(strsplit(file,split = "_"))
  term <- term[[length(term)]]
  term <- gsub("[.]csv","",term)
  term <- stringr::str_to_title(term)
  temp_data_long$Term <- term
  data_long <- rbind(data_long,temp_data_long)
}
data_long <- data_long  %>%
  dplyr::filter(Year != 2023 & Year >= 1980 & !(Term %in% c("Glycomics","Phenomics")))%>%
  dplyr::arrange(Year) %>% 
  dplyr::group_by(Term) %>% 
  dplyr::mutate(YoY = Count/lag(Count,1)*100) %>%
  dplyr::group_by(Year) %>% 
  dplyr::mutate(Count_normalized_per_year = Count/sum(Count)*100)


setwd(folder_for_plots)

ggplot(data = data_long,
       mapping = aes(x = Year,
                     y = Count,
                     fill = Term)) +
  geom_col() +
  labs(y = "Number of PubMed results",
       fill = "Search term") +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        panel.grid.minor = element_blank()) +
  scale_fill_viridis_d() + 
  guides(fill = guide_legend(nrow = 2))
ggsave("absolute.png",height = 7, width = 10)

ggplot(data = data_long,
       mapping = aes(x = Year,
                     y = Count_normalized_per_year,
                     color = Term)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(axis.text = element_text(color = "black"),
        legend.position = "top",
        panel.grid.minor = element_blank()) +
  labs(y = "Percentage of PubMed results per Year",
       color = "Search term") +
  facet_wrap("Term",scales = "free_y") +
  scale_color_viridis_d() + 
  guides(color = guide_legend(nrow = 2))
ggsave("percentage.png",height = 7, width = 10)
