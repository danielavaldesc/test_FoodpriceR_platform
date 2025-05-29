
library(rsconnect)

rsconnect::setAccountInfo(name='danielavaldesc',
                          token='65FD45D764B4D7CD39BF1088663FC201',
                          secret='4vsBxEUHQx7n8TWbuWW0bjz9h+HXdVfIfYwq0iuY')


rsconnect::deployApp('C:\\Users\\danie\\OneDrive\\Escritorio\\test_FoodpriceR_platform\\', 
                     appFiles = c("ui.R", "server.R", "shape_files/departamentos.shp", "shape_files/DANE_geodata/MGN_ANM_MPIOS_WGS84.shp"),
                     appName = "CoCAApp",
                     forceUpdate = TRUE)
