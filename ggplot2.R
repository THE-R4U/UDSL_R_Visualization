train <- read.csv("train.csv")

library(ggplot2)          # ggplot2 is an R library for visualizations train.
ggplot(train, aes(Item_Visibility, Item_MRP)) + 
  geom_point() + 
  scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05)) + 
  scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30)) + 
  theme_bw() 

ggplot(train, aes(Item_Visibility, Item_MRP)) + geom_point(aes(color = Item_Type)) + 
  scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+
  scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+
  theme_bw() + labs(title="Scatterplot")

ggplot(train, aes(Item_Visibility, Item_MRP)) + geom_point(aes(color = Item_Type)) + 
  scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+
  scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+ 
  theme_bw() + labs(title="Scatterplot") + facet_wrap( ~ Item_Type)


ggplot(train, aes(Item_MRP)) + geom_histogram(binwidth = 2)+
  scale_x_continuous("Item MRP", breaks = seq(0,270,by = 30))+
  scale_y_continuous("Count", breaks = seq(0,200,by = 20))+
  labs(title = "Histogram")


ggplot(train, aes(Outlet_Establishment_Year)) + geom_bar(fill = "red")+theme_bw()+
  scale_x_continuous("Establishment Year", breaks = seq(1985,2010)) + 
  scale_y_continuous("Count", breaks = seq(0,1500,150)) +
  coord_flip()+ labs(title = "Bar Chart") + theme_gray()

ggplot(train, aes(Item_Type, Item_Weight)) + 
  geom_bar(stat = "identity", fill = "darkblue") + 
  scale_x_discrete("Outlet Type") + 
  scale_y_continuous("Item Weight", breaks = seq(0,15000, by = 500)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(title = "Bar Chart")


ggplot(train, aes(Outlet_Location_Type, fill = Outlet_Type)) + geom_bar()+
  labs(title = "Stacked Bar Chart", x = "Outlet Location Type", y = "Count of Outlets")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
  geom_boxplot(fill = "red")+
  scale_y_continuous("Item Outlet Sales", breaks= seq(0,15000, by=500))+
  labs(title = "Box Plot", x = "Outlet Identifier")

ggplot(train, aes(Item_Outlet_Sales)) + 
  geom_area(stat = "bin", bins = 30, fill = "steelblue") + 
  scale_x_continuous(breaks = seq(0,11000,1000)) + 
  labs(title = "Area Chart", x = "Item Outlet Sales", y = "Count") 

ggplot(train, aes(Outlet_Identifier, Item_Type)) +
  geom_raster(aes(fill = Item_MRP)) +
  labs(title ="Heat Map", x = "Outlet Identifier", y = "Item Type")+
  scale_fill_continuous(name = "Item MRP") 

#install.packages("corrgram")
library(corrgram)

corrgram(train, order=NULL, panel=panel.shade, text.panel=panel.txt,
         main="Correlogram") 

