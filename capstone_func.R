# amazon trends------------------------------------------------------------------------
get_materials <- function(df, dt_range){
  df <- if(dt_range == "Past 30 Days"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-30))
  } else if(dt_range == "Past 90 Days"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-90))
  } else if(dt_range == "Past Year"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-365))
  } else if(dt_range == "This Year"){
    subset(df, (year(Listed_Date) == year(today())))
  }
  materials_ct <- df$Materials %>% str_split(',') %>% unlist() %>%
    table(exclude = "") %>% as.data.frame() %>% subset(Freq>=20) %>% arrange(-Freq)
  return(materials_ct)
}

get_brands <- function(df){
  brands_ct <- df$brands %>% table() %>% as.data.frame() %>% arrange(-Freq)
  return(brands_ct)
}

get_material_trend <- function(df, dt_range){
  trend_df <- if(dt_range == "Past 30 Days"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-30))
  } else if(dt_range == "Past 90 Days"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-90))
  } else if(dt_range == "Past Year"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-365))
  } else if(dt_range == "This Year"){
    subset(df, (year(Listed_Date) == year(today())))
  }
  material_trend <- trend_df %>% select(c(Listed_Date, Materials)) %>% separate_rows(Materials, sep = ',') %>% 
    subset(Materials!="") %>% group_by(Materials, Listed_Date) %>% summarise(count = n())
  return(material_trend)
}

get_trending <- function(df){
  df$Listed_Month <- strftime(df$Listed_Date, format = "%y-%m")
  df %>% subset(Listed_Month >= unique(sort(Listed_Month, decreasing = T))[2]) %>% group_by(Materials, Listed_Month) %>% 
    summarise(products = sum(count)) %>% mutate(`Growth(%)` = round(((products/lag(products)-1)*100), 2)) %>% select(-2) %>% drop_na() %>% arrange(-`Growth(%)`)
}

cat_materials_features <- function(df){
  df$Materials <- with(df, paste(Material_1, Material_2, Material_3, Material_4, Material_5, sep = ','))
  df$Features <- with(df, paste(Feature_1, Feature_2, Feature_3, Feature_4, Feature_5, Feature_6, Feature_7, Feature_8, Feature_9, Feature_10, Feature_11, sep = ','))
  df <- df %>% select(-8:-23)
  return(df)
}

get_features <- function(df, dt_range, dict){
  features_df <- 
  if(dt_range == "Past 30 Days"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-30))
  } else if(dt_range == "Past 90 Days"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-90))
  } else if(dt_range == "Past Year"){
    subset(df, (Listed_Date <= today())&(Listed_Date >= today()-365))
  } else if(dt_range == "This Year"){
    subset(df, (Listed_Date == year(today())))
  }
  features_df %>% separate_rows(Features, sep = ',') %>% subset(Features!="") %>% 
    mutate(Features_cat = hash::values(dict, Features)) %>% select(Features_cat) %>% 
    table(exclude = "") %>% as.data.frame() %>% arrange(-Freq) %>% 
    mutate(pct = Freq/sum(Freq), ymax = cumsum(pct), ymin = c(0, head(ymax, n=-1)))
}

materials_features <- function(df, dt_range, sel_features, dict){
  mf_df <- 
    if(dt_range == "Past 30 Days"){
      subset(df, (Listed_Date <= today())&(Listed_Date >= today()-30))
    } else if(dt_range == "Past 90 Days"){
      subset(df, (Listed_Date <= today())&(Listed_Date >= today()-90))
    } else if(dt_range == "Past Year"){
      subset(df, (Listed_Date <= today())&(Listed_Date >= today()-365))
    } else if(dt_range == "This Year"){
      subset(df, (Listed_Date == year(today())))
    }
  mf_df %>% separate_rows(Materials, sep = ',') %>% 
    subset(Materials!="") %>% separate_rows(Features, sep = ',') %>% subset(Features!="") %>%
    mutate(Features_cat = hash::values(dict, Features)) %>% subset(Features_cat %in% sel_features) %>% 
    select(Materials) %>% table(exclude = "") %>% as.data.frame() %>% arrange(-Freq)
}

plot_supply_bar <- function(category){
  supply_data %>% subset(Category==category) %>%
    ggplot(aes(x=reorder(Product, Capacity, function(x){sum(x)}), y=Capacity, group = Product, fill=reorder(Company, Capacity),
               text = paste('Company: ', Company, '<br>Capacity', Capacity, '<br>Capacity Utilization', CU, '<br>Unit: ', Unit))) +
    geom_bar(stat='identity') + coord_flip() + labs(x = NULL, y = 'Capacity') + scale_y_continuous(label = unit_format(unit = "K", scale = 0.001)) + theme_minimal() + 
    theme(plot.margin = unit(c(0,0,2,0), "mm"), legend.position = 'none', axis.title = element_text(size = 8), axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 8)) +
    scale_fill_manual(values = c(get_palette("jco", length(unique(supply_data$Company)))))
}

