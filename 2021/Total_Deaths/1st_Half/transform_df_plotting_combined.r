df$country=df$group
df$group=as.character(df$group)
df$group[df$group=="India" & df$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df$group[df$group=="India" & df$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df$group[df$group=="India" & df$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df$group[df$group=="India" & df$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df$group[df$group=="India" & df$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df$group[df$group=="India" & df$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df$group[df$group=="India" & df$colour=="Scenario 7"] = "Rel: Pak -> Ind"
df$group[df$group=="India" & df$colour=="Scenario 8"] = "Rel: Nep -> Ind"
df$group[df$group=="India" & df$colour=="Original"] = "Fit Ind"

df$group[df$group=="Pakistan" & df$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df$group[df$group=="Pakistan" & df$colour=="Original"] = "Fit Pak"

df$group[df$group=="Bangladesh" & df$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df$group[df$group=="Bangladesh" & df$colour=="Original"] = "Fit Bang"

df$group[df$group=="Nepal" & df$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df$group[df$group=="Nepal" & df$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df$group[df$group=="Nepal" & df$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df$group[df$group=="Nepal" & df$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df$group[df$group=="Nepal" & df$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df$group[df$group=="Nepal" & df$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df$group[df$group=="Nepal" & df$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df$group[df$group=="Nepal" & df$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df$group[df$group=="Nepal" & df$colour=="Original"] = "Fit Nep"

df$group[df$group=="Sri Lanka" & df$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df$group[df$group=="Sri Lanka" & df$colour=="Original"] = "Fit Sri"

ordering<-c( 
  "Abs: Bang -> Ind",
  "Rel: Bang -> Ind" , 
  "Abs: Sri -> Ind",
  "Rel: Sri -> Ind" ,
  "Fit Ind",  
  "Abs: Pak -> Ind",  
  "Rel: Pak -> Ind",
  "Abs: Nep -> Ind",  
  "Rel: Nep -> Ind",
  
  "Abs: Nep -> Pak",
  "Rel: Nep -> Pak",
  "Abs: Bang -> Pak",
  "Rel: Bang -> Pak",
  "Fit Pak",
  "Abs: Sri -> Pak",
  "Rel: Sri -> Pak",
  "Abs: Ind -> Pak",
  "Rel: Ind -> Pak",
  
  "Abs: Pak -> Bang",
  "Rel: Pak -> Bang",
  "Abs: Ind -> Bang",
  "Rel: Ind -> Bang",
  "Fit Bang",
  "Abs: Nep -> Bang",
  "Rel: Nep -> Bang",
  "Abs: Sri -> Bang",
  "Rel: Sri -> Bang",
  
  "Abs: Sri -> Nep",
  "Rel: Sri -> Nep",
  "Abs: Pak -> Nep",
  "Rel: Pak -> Nep",
  "Fit Nep",
  "Abs: Ind -> Nep",
  "Rel: Ind -> Nep",
  "Abs: Bang -> Nep",
  "Rel: Bang -> Nep",
   
  "Abs: Ind -> Sri",
  "Rel: Ind -> Sri",
  "Abs: Nep -> Sri",
  "Rel: Nep -> Sri",
  "Fit Sri",
  "Abs: Bang -> Sri",
  "Rel: Bang -> Sri",
  "Abs: Pak -> Sri",
  "Rel: Pak -> Sri"
)
df$group=factor(df$group,levels=ordering)
df_cf1$country=df_cf1$group
df_cf1$group=as.character(df_cf1$group)

df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 7"] = "Rel: Pak -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Scenario 8"] = "Rel: Nep -> Ind"
df_cf1$group[df_cf1$group=="India" & df_cf1$colour=="Original"] = "Fit Ind"

df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf1$group[df_cf1$group=="Pakistan" & df_cf1$colour=="Original"] = "Fit Pak"

df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf1$group[df_cf1$group=="Bangladesh" & df_cf1$colour=="Original"] = "Fit Bang"

df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf1$group[df_cf1$group=="Nepal" & df_cf1$colour=="Original"] = "Fit Nep"

df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf1$group[df_cf1$group=="Sri Lanka" & df_cf1$colour=="Original"] = "Fit Sri"


df_cf1$group=factor(df_cf1$group,levels=ordering)

df_cf2$country=df_cf2$group
df_cf2$group=as.character(df_cf2$group)

df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 7"] = "Rel: Pak -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Scenario 8"] = "Rel: Nep -> Ind"
df_cf2$group[df_cf2$group=="India" & df_cf2$colour=="Original"] = "Fit Ind"

df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf2$group[df_cf2$group=="Pakistan" & df_cf2$colour=="Original"] = "Fit Pak"

df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf2$group[df_cf2$group=="Bangladesh" & df_cf2$colour=="Original"] = "Fit Bang"

df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf2$group[df_cf2$group=="Nepal" & df_cf2$colour=="Original"] = "Fit Nep"

df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf2$group[df_cf2$group=="Sri Lanka" & df_cf2$colour=="Original"] = "Fit Sri"

df_cf2$group=factor(df_cf2$group,levels=ordering)


df_cf3$country=df_cf3$group
df_cf3$group=as.character(df_cf3$group)
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 7"] = "Rel: Pak -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Scenario 8"] = "Rel: Nep -> Ind"
df_cf3$group[df_cf3$group=="India" & df_cf3$colour=="Original"] = "Fit Ind"

df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf3$group[df_cf3$group=="Pakistan" & df_cf3$colour=="Original"] = "Fit Pak"

df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf3$group[df_cf3$group=="Bangladesh" & df_cf3$colour=="Original"] = "Fit Bang"

df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf3$group[df_cf3$group=="Nepal" & df_cf3$colour=="Original"] = "Fit Nep"

df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf3$group[df_cf3$group=="Sri Lanka" & df_cf3$colour=="Original"] = "Fit Sri"

df_cf3$group=factor(df_cf3$group,levels=ordering)

df_cf4$country=df_cf4$group
df_cf4$group=as.character(df_cf4$group)
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 7"] = "Rel: Pak -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Scenario 8"] = "Rel: Nep -> Ind"
df_cf4$group[df_cf4$group=="India" & df_cf4$colour=="Original"] = "Fit Ind"

df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf4$group[df_cf4$group=="Pakistan" & df_cf4$colour=="Original"] = "Fit Pak"

df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf4$group[df_cf4$group=="Bangladesh" & df_cf4$colour=="Original"] = "Fit Bang"

df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf4$group[df_cf4$group=="Nepal" & df_cf4$colour=="Original"] = "Fit Nep"

df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf4$group[df_cf4$group=="Sri Lanka" & df_cf4$colour=="Original"] = "Fit Sri"


df_cf4$group=factor(df_cf4$group,levels=ordering)


df_cf5$country=df_cf5$group
df_cf5$group=as.character(df_cf5$group)
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 7"] = "Rel: Nep -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Scenario 8"] = "Rel: Pak -> Ind"
df_cf5$group[df_cf5$group=="India" & df_cf5$colour=="Original"] = "Fit Ind"

df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf5$group[df_cf5$group=="Pakistan" & df_cf5$colour=="Original"] = "Fit Pak"

df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf5$group[df_cf5$group=="Bangladesh" & df_cf5$colour=="Original"] = "Fit Bang"

df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf5$group[df_cf5$group=="Nepal" & df_cf5$colour=="Original"] = "Fit Nep"

df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf5$group[df_cf5$group=="Sri Lanka" & df_cf5$colour=="Original"] = "Fit Sri"

df_cf5$group=factor(df_cf5$group,levels=ordering)

df_cf6$country=df_cf6$group
df_cf6$group=as.character(df_cf6$group)
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 7"] = "Rel: Nep -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Scenario 8"] = "Rel: Pak -> Ind"
df_cf6$group[df_cf6$group=="India" & df_cf6$colour=="Original"] = "Fit Ind"

df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf6$group[df_cf6$group=="Pakistan" & df_cf6$colour=="Original"] = "Fit Pak"

df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf6$group[df_cf6$group=="Bangladesh" & df_cf6$colour=="Original"] = "Fit Bang"

df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf6$group[df_cf6$group=="Nepal" & df_cf6$colour=="Original"] = "Fit Nep"

df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf6$group[df_cf6$group=="Sri Lanka" & df_cf6$colour=="Original"] = "Fit Sri"


df_cf6$group=factor(df_cf6$group,levels=ordering)

df_cf7$country=df_cf7$group
df_cf7$group=as.character(df_cf7$group)
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 7"] = "Rel: Pak -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Scenario 8"] = "Rel: Nep -> Ind"
df_cf7$group[df_cf7$group=="India" & df_cf7$colour=="Original"] = "Fit Ind"

df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf7$group[df_cf7$group=="Pakistan" & df_cf7$colour=="Original"] = "Fit Pak"

df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf7$group[df_cf7$group=="Bangladesh" & df_cf7$colour=="Original"] = "Fit Bang"

df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf7$group[df_cf7$group=="Nepal" & df_cf7$colour=="Original"] = "Fit Nep"

df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf7$group[df_cf7$group=="Sri Lanka" & df_cf7$colour=="Original"] = "Fit Sri"


df_cf7$group=factor(df_cf7$group,levels=ordering)


df_cf8$country=df_cf8$group
df_cf8$group=as.character(df_cf8$group)
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 1"] = "Abs: Bang -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 2"] = "Abs: Sri -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 3"] = "Abs: Pak -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 4"] = "Abs: Nep -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 5"] = "Rel: Bang -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 6"] = "Rel: Sri -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 7"] = "Rel: Pak -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Scenario 8"] = "Rel: Nep -> Ind"
df_cf8$group[df_cf8$group=="India" & df_cf8$colour=="Original"] = "Fit Ind"

df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 1"] = "Abs: Nep -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 2"] = "Abs: Bang -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 3"] = "Abs: Sri -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 4"] = "Abs: Ind -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 5"] = "Rel: Nep -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 6"] = "Rel: Bang -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 7"] = "Rel: Sri -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Scenario 8"] = "Rel: Ind -> Pak"
df_cf8$group[df_cf8$group=="Pakistan" & df_cf8$colour=="Original"] = "Fit Pak"

df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 1"] = "Abs: Pak -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 2"] = "Abs: Ind -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 3"] = "Abs: Nep -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 4"] = "Abs: Sri -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 5"] = "Rel: Pak -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 6"] = "Rel: Ind -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 7"] = "Rel: Nep -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Scenario 8"] = "Rel: Sri -> Bang"
df_cf8$group[df_cf8$group=="Bangladesh" & df_cf8$colour=="Original"] = "Fit Bang"

df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 1"] = "Abs: Sri -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 2"] = "Abs: Pak -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 3"] = "Abs: Ind -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 4"] = "Abs: Bang -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 5"] = "Rel: Sri -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 6"] = "Rel: Pak -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 7"] = "Rel: Ind -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Scenario 8"] = "Rel: Bang -> Nep"
df_cf8$group[df_cf8$group=="Nepal" & df_cf8$colour=="Original"] = "Fit Nep"

df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 1"] = "Abs: Ind -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 2"] = "Abs: Nep -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 3"] = "Abs: Bang -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 4"] = "Abs: Pak -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 5"] = "Rel: Ind -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 6"] = "Rel: Nep -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 7"] = "Rel: Bang -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Scenario 8"] = "Rel: Pak -> Sri"
df_cf8$group[df_cf8$group=="Sri Lanka" & df_cf8$colour=="Original"] = "Fit Sri"

df_cf8$group=factor(df_cf8$group,levels=ordering)






# dataframe for additional lines
df_originals=df
df_originals$group=as.character(df_originals$group)
un=unique(df_originals$group)
df_originals$median[grep("-> Ind",df_originals$group)] = df$median[grep("^Fit Ind$",df$group)]
df_originals$median[grep("-> Pak",df_originals$group)] = df$median[grep("^Fit Pak$",df$group)]
df_originals$median[grep("-> Bang",df_originals$group)] = df$median[grep("^Fit Bang$",df$group)]
df_originals$median[grep("-> Nep",df_originals$group)] = df$median[grep("^Fit Nep$",df$group)]
df_originals$median[grep("-> Sri",df_originals$group)] = df$median[grep("^Fit Sri$",df$group)]
df_originals$group=as.factor(df_originals$group)



mxs=c(
  max(df$deaths_ui[df$group==ordering[1]],df$deaths_ui[df$group==ordering[2]]),
  max(df$deaths_ui[df$group==ordering[1]],df$deaths_ui[df$group==ordering[2]]),
  max(df$deaths_ui[df$group==ordering[3]],df$deaths_ui[df$group==ordering[4]]),
  max(df$deaths_ui[df$group==ordering[3]],df$deaths_ui[df$group==ordering[4]]),
  max(df$deaths_ui[df$group==ordering[5]]),
  max(df$deaths_ui[df$group==ordering[6]],df$deaths_ui[df$group==ordering[7]]),
  max(df$deaths_ui[df$group==ordering[6]],df$deaths_ui[df$group==ordering[7]]),
  max(df$deaths_ui[df$group==ordering[8]],df$deaths_ui[df$group==ordering[9]]),
  max(df$deaths_ui[df$group==ordering[8]],df$deaths_ui[df$group==ordering[9]]),
  
  max(df$deaths_ui[df$group==ordering[10]],df$deaths_ui[df$group==ordering[11]]),
  max(df$deaths_ui[df$group==ordering[10]],df$deaths_ui[df$group==ordering[11]]),
  max(df$deaths_ui[df$group==ordering[12]],df$deaths_ui[df$group==ordering[13]]),
  max(df$deaths_ui[df$group==ordering[12]],df$deaths_ui[df$group==ordering[13]]),
  max(df$deaths_ui[df$group==ordering[14]]),
  max(df$deaths_ui[df$group==ordering[15]],df$deaths_ui[df$group==ordering[16]]),
  max(df$deaths_ui[df$group==ordering[15]],df$deaths_ui[df$group==ordering[16]]),
  max(df$deaths_ui[df$group==ordering[17]],df$deaths_ui[df$group==ordering[18]]),
  max(df$deaths_ui[df$group==ordering[17]],df$deaths_ui[df$group==ordering[18]]), 
  
  max(df$deaths_ui[df$group==ordering[19]],df$deaths_ui[df$group==ordering[20]]),
  max(df$deaths_ui[df$group==ordering[19]],df$deaths_ui[df$group==ordering[20]]),
  max(df$deaths_ui[df$group==ordering[21]],df$deaths_ui[df$group==ordering[22]]),
  max(df$deaths_ui[df$group==ordering[21]],df$deaths_ui[df$group==ordering[22]]),
  max(df$deaths_ui[df$group==ordering[23]]),
  max(df$deaths_ui[df$group==ordering[24]],df$deaths_ui[df$group==ordering[25]]),
  max(df$deaths_ui[df$group==ordering[24]],df$deaths_ui[df$group==ordering[25]]),
  max(df$deaths_ui[df$group==ordering[26]],df$deaths_ui[df$group==ordering[27]]),
  max(df$deaths_ui[df$group==ordering[26]],df$deaths_ui[df$group==ordering[27]]), 
  
  max(df$deaths_ui[df$group==ordering[28]],df$deaths_ui[df$group==ordering[29]]),
  max(df$deaths_ui[df$group==ordering[28]],df$deaths_ui[df$group==ordering[29]]),
  max(df$deaths_ui[df$group==ordering[30]],df$deaths_ui[df$group==ordering[31]]),
  max(df$deaths_ui[df$group==ordering[30]],df$deaths_ui[df$group==ordering[31]]),
  max(df$deaths_ui[df$group==ordering[32]]),
  max(df$deaths_ui[df$group==ordering[33]],df$deaths_ui[df$group==ordering[34]]),
  max(df$deaths_ui[df$group==ordering[33]],df$deaths_ui[df$group==ordering[34]]),
  max(df$deaths_ui[df$group==ordering[35]],df$deaths_ui[df$group==ordering[36]]),
  max(df$deaths_ui[df$group==ordering[35]],df$deaths_ui[df$group==ordering[36]]), 
  
  max(df$deaths_ui[df$group==ordering[37]],df$deaths_ui[df$group==ordering[38]]),
  max(df$deaths_ui[df$group==ordering[37]],df$deaths_ui[df$group==ordering[38]]),
  max(df$deaths_ui[df$group==ordering[39]],df$deaths_ui[df$group==ordering[40]]),
  max(df$deaths_ui[df$group==ordering[39]],df$deaths_ui[df$group==ordering[40]]),
  max(df$deaths_ui[df$group==ordering[41]]),
  max(df$deaths_ui[df$group==ordering[42]],df$deaths_ui[df$group==ordering[43]]),
  max(df$deaths_ui[df$group==ordering[42]],df$deaths_ui[df$group==ordering[43]]),
  max(df$deaths_ui[df$group==ordering[44]],df$deaths_ui[df$group==ordering[45]]),
  max(df$deaths_ui[df$group==ordering[44]],df$deaths_ui[df$group==ordering[45]]) 
)
df_aux <- data.frame(date = rep(as.Date("01/01/2021",format="%d/%m/%Y"),length(ordering)),
                     max=mxs,
                     group=ordering,
                     country=c(rep("India",9),rep("Pakistan",9),rep("Bangladesh",9),
                               rep("Nepal",9),rep("Sri Lanka",9)))


