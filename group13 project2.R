library(tidyverse)
library(dplyr)
library(highcharter)
library(DT)
library(leaflet)
###Prepare
#load cleaned data
load("../Downloads/clean_sales.Rdata")
#load master data customer
mastercustomer_data <- read_csv(
  "../Downloads/Masterdata_Customer.csv",
  col_types = cols(
    CustomerID = col_character(),
    CustomerName = col_character(),
    Channel = col_character(),
    SubChannel = col_character(),
    Account = col_character(),
    Latitude = col_double(),
    Longitude = col_double(),
    City = col_character(),
    Province = col_character(),
    Province_map = col_character(),
    Region = col_character(),
    GroupIsland = col_character()
  )
)
#load master data product
masterproduct_data <-  read_csv(
  "../Downloads/Masterdata_Product.csv",
  col_types = cols(
    ProductID = col_character(),
    ProductName = col_character(),
    Category = col_character(),
    Brand = col_character(),
    TradeUnit = col_character(),
    StorageCondition = col_character(),
    ProductType = col_character(),
    NetKgPerEach = col_double(), 
    EachPerCarton = col_double(), 
    CartonsPerPallet = col_double(), 
    KgPerCarton = col_double(),
    Source = col_character(),
    COGSPerKg = col_double(),
    ShelfLifeInDays = col_integer(),
    ItemStatus = col_character()
  )
)
str(mastercustomer_data)
str(masterproduct_data)
#Clean
#~Check duplicate because in mastercustomer one cusid just be only one
mastercustomer_data %>%
  count(CustomerID) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  print()
mastercustomer_data %>%
  count(CustomerName) %>%
  filter(n > 1) %>%
  arrange(desc(n)) %>%
  print()
#~find duplicate with different information
mastercustomer_dupid <- mastercustomer_data %>% 
  group_by(CustomerID) %>%
  filter(n() > 1) %>%
  filter(n_distinct(Channel) > 1 
         |n_distinct(City) > 1
         |n_distinct(Region) > 1) %>% 
  ungroup() %>% arrange(CustomerID)
mastercustomer_dupname <- mastercustomer_data %>% 
  group_by(CustomerName) %>%
  filter(n() > 1) %>%
  filter(n_distinct(Channel) > 1 
         |n_distinct(City) > 1
         |n_distinct(Region) > 1) %>% 
  ungroup() %>% arrange(CustomerName)
#_not has N/A so it's not be problem, we can still use distinct without afraid from clean dup name but has dif i4
#~clean dup
cmastercustomer_data <- mastercustomer_data %>% distinct(CustomerID,.keep_all = TRUE)
cmastercustomer_data %>% count(CustomerID) %>% filter(n>1) %>% print()
clean_dupname <- cmastercustomer_data %>% distinct(CustomerName, .keep_all = TRUE) %>%
  select(-CustomerID)
###Project2.
##0.Join data
#Check data to known which does each variable has N/A before join?
colSums(is.na(cmastercustomer_data))
colSums(is.na(masterproduct_data))
colSums(is.na(clean_sales))
#Let join master customer into clean sale first
#_because of N/a value at CustomerID in clean data but we wanna still keep them for another information so
#~Double join: by customer id and join by customer name with the observation has N/a value
sales_with_id <- clean_sales %>% filter(!is.na(CustomerID))
sales_without_id_but_by_name <- clean_sales %>% filter(is.na(CustomerID))
#~~Join by customer Id
joined_with_id <- sales_with_id %>%
  left_join(cmastercustomer_data, by = "CustomerID")
nrow(clean_sales) == nrow(joined_with_id)
#~~Join by customername
joined_without_id <- sales_without_id_but_by_name %>%
  left_join(clean_dupname, by = "CustomerName")
#~~Combine them 
sale_join_mastercus <- bind_rows(joined_with_id, joined_without_id)
nrow(clean_sales) == nrow(sale_join_mastercus)
#Because of Product ID didn't have N/A so we cain join without clean
final_sale_data <- sale_join_mastercus %>% left_join(masterproduct_data)
nrow(clean_sales) == nrow(final_sale_data)
colSums(is.na(final_sale_data))#*
###############################addone-dumaaaa
checkconsistency <- final_sale_data %>% mutate(
  Discrepacy=OrderQuantityInCarton-(abs(CancelQuantityInCarton)+ ConfirmQuantityInCarton))
checkconsistency <- checkconsistency %>% filter(Discrepacy!=0)#I see outliner has been problen here maybe leftover 0
checkcost <- final_sale_data %>% mutate(Costpercarton=OrderValue/OrderQuantityInCarton,
                                        COGSpercarton=KgPerCarton*COGSPerKg)#i think has sth confused at COGS=0 
final_sale_data <- final_sale_data %>% mutate(
  fixConfirmQuantityInCarton=OrderQuantityInCarton-abs(CancelQuantityInCarton))
checkconsistency2 <-final_sale_data %>%
  mutate(
    Discrepacy=OrderQuantityInCarton-(abs(CancelQuantityInCarton)+ fixConfirmQuantityInCarton)) %>% 
  filter(Discrepacy!=0)#al need be equal zero
  

#_after check i see alot of problem that is in cfqtyincarton and the COGS
##Q1
#~Prepare
select_month <- final_sale_data %>% select(PONumber:fixConfirmQuantityInCarton)
select_month <- select_month %>%  mutate(Month = format(DeliveryDate, "%Y-%m"),
                                         filConfirmValue= ifelse(is.na(ConfirmValue), 0, ConfirmValue),
                                         #fil n/a in confirm value=0 to have view about financial loss
                                         Caculatecost=fixConfirmQuantityInCarton* KgPerCarton* COGSPerKg,
                                         Profit= filConfirmValue- Caculatecost)
select_month %>% count(is.na(KgPerCarton))
select_month %>% count(is.na(COGSPerKg))
select_month %>% count(is.na(ConfirmValue))
monthly_sale <- select_month %>% group_by(Month) %>% 
  summarise(Totalvaluedemand= round(sum(OrderValue, na.rm = TRUE),1),
            Totalvaluerevenue= round(sum(filConfirmValue, na.rm = TRUE),1),
            Totalvolume= round(sum(fixConfirmQuantityInCarton, na.rm = TRUE),0),
            Totalcost=round(sum(Caculatecost, na.rm = TRUE),1),
            Totalprofit=round(sum(Profit, na.rm = TRUE),1)) %>% 
  ungroup()
#~Draw again more visualize
colors_vector <- monthly_sale %>%
  mutate(color = case_when(
    Totalvolume < quantile(Totalvolume, 0.1) ~ "#0F9D58", 
    Totalvolume < quantile(Totalvolume, 0.6) ~ "#f4B400",  
    TRUE ~ "#DB4437" 
  ))%>% 
  pull(color)#Creat vector color for volume

monthly_sale %>% hchart(type = "column",hcaes(x=Month, y=Totalvolume),
                        name="Unit cartons",
                        colorByPoint = TRUE,
                        yAxis=0) %>%
  hc_colors(colors_vector) %>%
  hc_add_series(monthly_sale,type = "line",
                hcaes(x=Month, y=Totalvaluedemand),
                name="Demand value",
                color="#4285F4",
                showInLegend = TRUE,
                yAxis=1) %>% 
  hc_add_series(monthly_sale,type = "line",
                hcaes(x=Month, y=Totalvaluerevenue),
                name="Revenue value",
                color="#6967CE",
                marker = list(symbol = "diamond"),
                dashStyle = "Dot",
                showInLegend = TRUE,
                yAxis=1) %>% 
  hc_add_series(monthly_sale,type = "line",
                hcaes(x=Month, y=Totalcost),
                name="Cost value",
                color="#7c3a2d",
                marker = list(symbol = "triangle"),
                dashStyle = "ShortDash",
                showInLegend = TRUE,
                yAxis=1) %>% 
  hc_add_series(monthly_sale,type = "line",
                hcaes(x=Month, y=Totalprofit),
                name="Profit value",
                color="#455A64",
                marker = list(symbol = "square"),
                dashStyle = "Dash",
                showInLegend = TRUE,
                yAxis=1) %>% 
  hc_xAxis(title = list(text = "Time")) %>%
  hc_yAxis_multiples(
    list(title=list(text="Volume Cartons"),opposite = TRUE),
    list(title=list(text="Currency Value"),opposite = FALSE)) %>% 
  hc_title(text = "Overview Monthly Sales Performance 2020") %>% 
  hc_subtitle(text = "Company B") %>% 
  hc_chart(zoomType = "xy")%>% # Allow zoom both horizontally and vertically
  hc_tooltip(table = TRUE,
             sort = TRUE,
             crosshairs = TRUE) %>% # Try each element to check the output
  hc_exporting(enabled = TRUE)%>%
  hc_add_series(name = "Low volum", color = "#0F9D58", type = "column",
                showInLegend = TRUE, data = list(),grouping = FALSE) %>%
  hc_add_series(name = "Medium volum", color = "#f4B400", type = "column",
                showInLegend = TRUE, data = list(),grouping = FALSE) %>%
  hc_add_series(name = "High volum", color = "#DB4437", type = "column",
                showInLegend = TRUE, data = list(),grouping = FALSE) %>%
  hc_legend(enabled = TRUE)
  
#~~Accuracy of data insight
accuracy_data1<- select_month%>% summarise(
  ColumnName="ConfirmValue",
  Accuracy=round((1-(sum(is.na(ConfirmValue))/n()))*100,1))
accuracy_data2<- select_month%>% summarise(
  ColumnName="KgPerCarton",
  Accuracy=round((1-(sum(is.na(KgPerCarton))/n()))*100,1))
accuracy_data3<- select_month%>% summarise(
  ColumnName="COGSPerKg",
  Accuracy=round((1-(sum(is.na(COGSPerKg))/n()))*100,1))
accuracy_data <- bind_rows(accuracy_data1,accuracy_data2,accuracy_data3) 
datatable(accuracy_data,
          caption = " Accuracy in data ",
          options = list(pageLength = 10,
                         dom = 'Bfrtip',
                         scrollX = TRUE,
                         class = 'stripe hover order-column',
                         columnDefs = list(
                           list(className = 'dt-center', targets = "_all"))),
          rownames = FALSE)

##Q2
#2a
#~Prepare
select_month %>% count(Channel) %>% arrange(desc(n))
select_month %>% count(SubChannel) %>% arrange(desc(n))
channel_share <- select_month %>% group_by(Month,Channel) %>% 
  summarise(Channelrevenue=sum(filConfirmValue,na.rm = TRUE)) %>% ungroup()
sub_share <- select_month %>% group_by(Month,SubChannel) %>% 
  summarise(Subrevenue=sum(filConfirmValue,na.rm = TRUE)) %>% ungroup()
sub_share <- sub_share %>%
  left_join(monthly_sale %>% select(Month, Totalvaluerevenue), by = "Month") %>%
  mutate(Share = round(Subrevenue / Totalvaluerevenue * 100,2))
channel_share <- channel_share %>% 
  left_join(monthly_sale %>% select(Month,Totalvaluerevenue),by="Month") %>% 
  mutate(Share=round(Channelrevenue/Totalvaluerevenue*100,2))
spread_channel <- channel_share %>% 
  select(Month, Channel, Share) %>% spread(key = Channel, value = Share )
spread_subchannel <- sub_share %>% select(Month,SubChannel,Share) %>% 
  spread(key = SubChannel, value = Share)
#~Draw
#~~channel share
spread_channel %>%
  hchart(type = "area", hcaes(x = Month, y = CaH), name = "CaH") %>%
  hc_add_series(spread_channel, type = "area", hcaes(x = Month, y = FS), name = "FS") %>%
  hc_add_series(spread_channel, type = "area", hcaes(x = Month, y = MT), name = "MT") %>%
  hc_add_series(spread_channel, type = "area", hcaes(x = Month, y = Others), name = "Others") %>%
  hc_plotOptions(area = list(stacking = "percent")) %>%
  hc_yAxis(title = list(text = "Percent(%)"), max = 100) %>%
  hc_xAxis(title = list(text = "Time")) %>%
  hc_title(text = "Channel Share Revenue") %>%
  hc_subtitle(text = "Company B") %>%
  hc_chart(zoomType = "xy") %>%
  hc_tooltip(shared = TRUE,
             sort = FALSE,
             crosshairs = TRUE,
             useHTML=TRUE,
             headerFormat="<b>{point.key}</b><br/>",
             pointFormat = "<span style='color:{series.color}'>●</span> 
             <b style='color:{series.color}'>{series.name}</b>: {point.percentage:.1f}%<br/>"
  )%>%
  hc_colors(c("#38A86F", "#BAEBAD", "#F3B735", "#FFD966")) %>%
  hc_exporting(enabled = TRUE)
#~~subchannel share
spread_subchannel %>%
  hchart(type = "area", hcaes(x = Month, y =spread_subchannel$`FS - Distributor` ), 
         name = "FS-ditribtor") %>%
  hc_add_series(spread_subchannel, type = "area", hcaes(x = Month, y = spread_subchannel$`FS - Key Account`), 
                name = "FS-key account") %>%
  hc_add_series(spread_subchannel, type = "area", hcaes(x = Month, y = GT), name = "GT") %>%
  hc_add_series(spread_subchannel, type = "area", hcaes(x = Month, y = spread_subchannel$`MT - Key Accounts`), 
                name = "MT-key account") %>%
  hc_add_series(spread_subchannel, type = "area", hcaes(x = Month, y = NKA), name = "NKA") %>%
  hc_add_series(spread_subchannel, type = "area", hcaes(x = Month, y = Others), name = "Other") %>%
  hc_plotOptions(area = list(stacking = "percent")) %>%
  hc_yAxis(title = list(text = "Percent(%)"), max = 100) %>%
  hc_xAxis(title = list(text = "Time")) %>%
  hc_title(text = "SubChannel Share Revenue") %>%
  hc_subtitle(text = "Company B") %>%
  hc_chart(zoomType = "xy") %>%
  hc_tooltip(shared = TRUE,
             sort = FALSE,
             crosshairs = TRUE,
             useHTML=TRUE,
             headerFormat="<b>{point.key}</b><br/>",
             pointFormat = "<span style='color:{series.color}'>●</span> 
             <b style='color:{series.color}'>{series.name}</b>: {point.percentage:.1f}%<br/>"
  )%>%
  hc_colors(c("#FFF2CC", "#F3B735" ,"#38A86F", "#7CCBA2", "#D2F2E1","#FFD966")) %>%
  hc_exporting(enabled = TRUE)
#2b Top 10% customer
colSums(is.na(select_month))
select_month %>% count(OrderLineID)
#~prepare data which is one customer orders maximum once per day
#__Fillout the data which has N/a at both variables Pon & cusID
onecusperday <- select_month %>% 
  filter(
    !( (is.na(PONumber) | PONumber == "" | PONumber == "-") &
         is.na(CustomerID) )
  )
check <-onecusperday %>%  filter(
  (is.na(PONumber) | PONumber == "" | PONumber == "-") &
    is.na(CustomerID))
#__creatvariable
onecusperday <- onecusperday %>%
  mutate(OderID = if_else(
    !is.na(PONumber) & PONumber != "" & PONumber != "-",
    paste0("PO", PONumber),
    paste0("O", CustomerID, "_", DeliveryDate)
  ))
#~Apply 4 criteria 
#~~Criteria1:Avaverage order value per each oderid
onecusperday <- onecusperday %>%
  mutate(filcusid = if_else(is.na(CustomerID), "Bearer", CustomerID),
         filcusname = if_else(is.na(CustomerName.x), "Bearer", CustomerName.x))
#__don't wanna fil out N/a at cusid
aov1 <- onecusperday %>% 
  group_by(OderID, filcusid, filcusname) %>%
  summarise(
    Odervalueperoder = sum(OrderValue, na.rm = TRUE)
  ) %>% ungroup() 
aov2 <- aov1 %>% group_by(filcusid,filcusname) %>% 
  summarise(Avgodervalue=mean(Odervalueperoder, na.rm=TRUE)) %>% ungroup() %>% 
  arrange(desc(Avgodervalue))
#~~Criteria2: Total value contribution
totalcfv <- onecusperday %>% group_by(filcusid,filcusname) %>% 
  summarise(Totalvalue=sum(filConfirmValue, na.rm = TRUE)) %>% ungroup() %>% 
  arrange(desc(Totalvalue))
#~~Criteria3: Frequency of purchase
cuspurchase_frequency <- onecusperday %>% group_by(filcusid,filcusname) %>% 
  summarise(Frequency=n_distinct(OderID)) %>% ungroup() %>% arrange(desc(Frequency))
#~~Criteria4: Cancel rate
cancel_rate <- onecusperday %>% 
  group_by(filcusid,filcusname) %>% 
  summarise(Cancel=sum(abs(CancelQuantityInCarton),na.rm = TRUE),
            Confirm=sum(fixConfirmQuantityInCarton,na.rm = TRUE)) %>% ungroup() %>% 
  mutate(Cancelrate=Cancel/(Cancel+Confirm)) %>% arrange(Cancelrate)
#~Top 10% each variable
lowestcancel_rate <- cancel_rate %>% filter(
  Cancelrate==0) %>% filter(Confirm>=quantile(Confirm,0.9))
top_aov <- aov2 %>% filter(Avgodervalue>=quantile(Avgodervalue,0.9))
top_total <- totalcfv %>% filter(Totalvalue>=quantile(Totalvalue,0.9))
top_frequen <- cuspurchase_frequency %>% filter(Frequency>=quantile(Frequency,0.9))
elitcus <- top_aov %>% inner_join(top_total, by="filcusid") %>%
  inner_join(top_frequen, by="filcusid") %>% inner_join(lowestcancel_rate,by="filcusid")
elitcusdone <- elitcus %>% select(Cusname=filcusname.x,
                                  Valuecontribution=Totalvalue,Avgodervalue,
                                  Frequency,Cancelrate,Confirmquantity=Confirm)
#~draw
datatable(elitcusdone,
          caption = " Elite Customers ",
          options = list(pageLength = 10,
                         dom = 'Bfrtip',
                         scrollX = TRUE,
                         class = 'stripe hover order-column',
                         columnDefs = list(
                           list(className = 'dt-center', targets = "_all"))),
          rownames = FALSE) %>%
  formatRound(columns = c("Valuecontribution", "Avgodervalue", 
                          "Frequency", "Cancelrate", "Confirmquantity"), digits = 0,
              mark = ",") %>% 
  formatCurrency(columns = c("Valuecontribution", "Avgodervalue"), currency = "$", digits = 0) %>%
  formatPercentage(columns = "Cancelrate", digits = 1) 

#2c map
colSums(is.na(onecusperday))
cusmap <- onecusperday %>% group_by(filcusid,filcusname,City,Province,Region,
                                 Latitude,Longitude) %>% 
  summarise(Totalcontribution=sum(filConfirmValue)) %>% ungroup()
colSums(is.na(cusmap))
#draw
summary(cusmap$Latitude)
summary(cusmap$Longitude)
str(cusmap)
str(cusmap$Latitude)
str(cusmap$Longitude)
head(cusmap)
sessionInfo()
cusmap_test <- cusmap %>% slice(1:5)
#
cusmap <- cusmap %>%
  mutate(
    popup = paste0(
      "<strong>", filcusname, "</strong><br/>",
      City, ", ", Province, "<br/>",
      "Region: ", Region, "<br/>",
      "Total Value: ", formatC(Totalcontribution, format = "f", big.mark = ",", digits = 0)
    )
  )

# leaflet map
leaflet(data = cusmap) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # Nền sáng đẹp
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    radius = ~sqrt(Totalcontribution) / 200,  # Điều chỉnh size bong bóng
    color = "#2c7bb6",
    fillOpacity = 0.6,
    stroke = FALSE,
    label = ~filcusname,
    popup = ~popup
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Customer Value (scaled circle)",
    colors = "#2c7bb6",
    labels = "Based on Confirmed Value",
    opacity = 0.6
  )
##Q3.
#3a
#~drilldown contribute of each product in each brand into revenue
#~~Prepare
#~~Add level 
tree_data <- onecusperday %>%
  mutate(
    filCategory = ifelse(is.na(Category) | Category == "", "Unknown_Product", Category),
    filBrand = ifelse(is.na(Brand) | Brand == "", "Unknown_brand", Brand)
  ) %>%
  group_by(filBrand, filCategory) %>%
  summarise(valuecontribute = sum(filConfirmValue, na.rm = TRUE)) %>% ungroup()
#~~~lvl1: Brand
level1 <- tree_data %>%
  group_by(filBrand) %>%
  summarise(value = sum(valuecontribute), .groups = "drop") %>%
  mutate(
    id = filBrand,
    name = filBrand,
    drilldown = filBrand
  )

#~~~lvl2: Category in Brand 
level2_list <- tree_data %>%
  mutate(drill_id = filBrand) %>%
  group_by(drill_id) %>%
  group_split() %>%
  map(~list(
    id = unique(.x$drill_id),
    name = unique(.x$drill_id),
    data = map2(.x$filCategory, .x$valuecontribute,
                ~ list(name = .x, y = .y))
  ))
#~~~Draw Performance of each product in each brand contribution into Sale revenue
highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Sale Performance") %>%
  hc_add_series(
    type = "column",
    name = "Brand",
    data = list_parse(level1 %>% ##
                        transmute(
                          name = filBrand,  
                          y = value,        
                          drilldown = filBrand
                        ))
  ) %>%
  hc_drilldown(
    series = level2_list
  ) %>%
  hc_tooltip(pointFormat = "<b>{point.name}</b><br>Value contribution: {point.y:,.0f}") %>%
  hc_exporting(enabled = TRUE)%>% 
  hc_colorAxis(stops = color_stops(
    n = 3,
    colors = c("#00cc66", "#ffff66", "#ff3300")
  ))%>% 
  hc_plotOptions(
    series = list(
      dataLabels = list(
        enabled = TRUE,
        format = "{point.y:,.0f}",
        style = list(fontSize = "12px",
                     textDecoration = "none"
                     )))) %>% 
  hc_xAxis(
    type = "category",
    title = list(text = "Brand"))

#~Trend pattern over months by category(use oder because wanna predict demand of customers and compare with supply)
#~~Prepare
trendcat <- onecusperday %>% filter(!is.na(Category)) %>% group_by(Month,Category) %>% 
  summarise(
    Demandvalue=sum(OrderValue),
    Supplyvalue=sum(filConfirmValue)) %>% ungroup()
trendcat_long <- trendcat %>% pivot_longer(cols = c(Demandvalue,Supplyvalue),
                                           names_to = "Type",values_to = "Value")
#~~Draw chart compared between demand and supply to known which is the most category has been canceled
trendcat_long <- trendcat_long %>% mutate(Group= interaction(Category,Type))
color_map <- trendcat_long %>%
  distinct(Group, Type) %>%
  mutate(color = if_else(Type == "Demandvalue", "#F3B735", "#38A86F")) %>%
  arrange(Group)
trendcat_long <- trendcat_long %>%
  mutate(Group = factor(Group, levels = color_map$Group)) %>%
  arrange(Group)
color_vector2 <- color_map$color
trendcat_long %>% hchart(
  type = "line", hcaes(x=Month,y=Value,group=Group)) %>% 
  hc_title(text = "Trend Pattern Over Time by Category") %>%
  hc_subtitle(text = "Visualize demand vs fulfillment gap") %>%
  hc_yAxis(title = list(text = "USD")) %>%
  hc_xAxis(title = list(text = "Month")) %>%
  hc_tooltip(shared = FALSE, crosshairs = TRUE) %>%
  hc_colors(colors = color_vector2) %>%
  hc_plotOptions(
    line = list(
      marker = list(
        enabled = TRUE,
        symbol = "circle"))) %>% 
  hc_exporting(enabled = TRUE)

#~Trend Favorite Brand through month
#~~Prepare
trendbrand <- onecusperday %>% filter(!is.na(Brand)) %>% group_by(Brand,Month) %>% 
  summarise(Value=sum(filConfirmValue)) %>% ungroup()
#~~Draw
trendbrand %>% hchart(type = "line", hcaes(x=Month, y=Value, group=Brand)) %>% 
  hc_title(text = "Brand Performance Over Time") %>%
  hc_xAxis(title = list(text = "Month")) %>%
  hc_yAxis(title = list(text = "Value")) %>%
  hc_tooltip(shared = TRUE) %>%
  hc_exporting(enabled = TRUE)

#3b single product level
colSums(is.na(onecusperday))
#~Sale revenue
separatesalevalue <-onecusperday %>%
  filter(ProductName!="Missing") %>% 
  group_by(ProductID,ProductName) %>% 
  summarise(
    Totalsalevalue=sum(filConfirmValue)) %>% ungroup() %>% arrange(desc(Totalsalevalue))
top5salevalue <- head(separatesalevalue,5)
bot5salevalue <- tail(separatesalevalue,5)
#~Profit
seperateprofit <- onecusperday %>% filter(!is.na(Profit)) %>% group_by(ProductID,ProductName) %>% 
  summarise(
    Totalprofit=sum(Profit)) %>% ungroup() %>% arrange(desc(Totalprofit))
top5profit <- head(seperateprofit,5)
bot5profit <- tail(seperateprofit,5)
consume3b <- bind_rows(
  mutate(top5salevalue,name="TopSaleValue"),
  mutate(bot5salevalue, name="BottomSaleValue"),
  mutate(top5profit, name="TopProfit"),
  mutate(bot5profit, name="BottomProfit")
)
#__will see N/A as 0 which mean that this is has value but didn't in top or bottom of this variables
consume3b_clean <- consume3b %>%
  group_by(ProductID, ProductName) %>%
  summarise(
    Totalsalevalue = sum(ifelse(is.na(Totalsalevalue), 0, Totalsalevalue)),
    Totalprofit = sum(ifelse(is.na(Totalprofit), 0, Totalprofit)),
    groups = paste(unique(name), collapse = " and ")
  ) %>% ungroup()
#~Draw
consume3b_clean %>%
  hchart("heatmap", hcaes(x = ProductName, y = groups, value = Totalsalevalue)) %>%
  hc_colorAxis(stops = color_stops(n = 5, colors = c("#f7fbff", "#6baed6", "#2171b5"))) %>%
  hc_title(text = "Performance of Key Products for Stockout Prevention") %>%
  hc_tooltip(pointFormat = "<b>{point.x}</b><br>Group: {point.y}<br>Sales: {point.value:,.0f}") %>%
  hc_xAxis(title = list(text = "Product Name")) %>%
  hc_yAxis(title = list(text = "Group")) %>%
  hc_exporting(enabled = TRUE)
##Or
consume3b_long <- consume3b_clean %>%
  pivot_longer(cols = c(Totalsalevalue, Totalprofit),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = recode(Metric,
                         "Totalsalevalue" = "Revenue",
                         "Totalprofit" = "Earning"))
consume3b_long %>%
  hchart("heatmap", hcaes(x = ProductName,
                          y = paste(groups, "-", Metric),
                          value = Value)) %>%
  hc_colorAxis(stops = color_stops(n = 5, colors = c("#f7fbff", "#6baed6", "#08306b"))) %>%
  hc_title(text = "Performance of Key Products for Stockout Prevention") %>%
  hc_subtitle(text = "Analyze to guide inventory decisions") %>%
  hc_tooltip(useHTML = TRUE,
             headerFormat = "",
             pointFormat = "<b>Product:</b> {point.x}<br><b>Group & Metric:</b> {point.y}<br><b>Value:</b> {point.value:,.0f}")%>%
  hc_xAxis(opposite = TRUE,title = list(text = "Product Name")) %>%
  hc_yAxis(title = list(text = "Group & Metric")) %>%
  hc_exporting(enabled = TRUE)

##Q4.
colSums(is.na(onecusperday))
#4a. Estimate the average daily volume in cartons and pallets from each facility
#~Volume carton
facilitydailyvolume <- onecusperday %>% group_by(FacilityID,DeliveryDate) %>% 
  summarise(
    Volumecarton= sum(ConfirmQuantityInCarton)) %>% ungroup()
avgvolumecartonoutbound <- facilitydailyvolume %>% group_by(FacilityID) %>% 
  summarise(
    Avgdailycarton=mean(Volumecarton),
    Maxdailycarton=max(Volumecarton)) %>% ungroup()
#~Volume pallet
palletdailyperfacility <- onecusperday %>% 
  filter(!is.na(CartonsPerPallet)&CartonsPerPallet>0) %>% 
  mutate(Confirmpallet=ConfirmQuantityInCarton/CartonsPerPallet) %>% 
  group_by(FacilityID,DeliveryDate) %>% summarise(
    Dailyconfirmpallet=sum(Confirmpallet)) %>% ungroup()
avgpalletoutbound <- palletdailyperfacility %>% group_by(FacilityID) %>% 
  summarise(Avgdailypallet=mean(Dailyconfirmpallet),
            Maxpalletdaily=max(Dailyconfirmpallet)) %>% ungroup()
#4b
truck_capacity_tons <- 0.5 
soifc01_data <- onecusperday %>%
  filter(FacilityID == "SOIFC01") %>%
  filter(!is.na(KgPerCarton) & KgPerCarton > 0) %>% 
  filter(!is.na(StorageCondition)) %>% 
  mutate(TotalKg = ConfirmQuantityInCarton * KgPerCarton,
         TotalTons = TotalKg / 1000)
#~Estimate volume in each conditon at SOIFC01
max_daily_volume_soifc01_by_storage <- soifc01_data %>%
  group_by(DeliveryDate, StorageCondition) %>%
  summarise(DailyTotalTons = sum(TotalTons, na.rm = TRUE)) %>%ungroup()
max_daily_volume_soifc01_by_storage <-max_daily_volume_soifc01_by_storage %>% 
  group_by(StorageCondition) %>%
  summarise(MaxDailyTons = max(DailyTotalTons, na.rm = TRUE)) %>% ungroup()
#~Estimate truck for each storagecondition
truckneed <- max_daily_volume_soifc01_by_storage %>% mutate(
  Targetvolumetruck= MaxDailyTons*0.8,
  Truckrequire= ceiling(Targetvolumetruck/truck_capacity_tons))
#~total truck require
totaltruck <- sum(truckneed$Truckrequire)