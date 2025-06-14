panel_inicio = tabPanel("Inicio",
             fluidPage(
               fluidRow(
                 column(12,
                        h1("Bienvenido a la Plataforma FoodPrice"),
                        p("El acceso económico a los alimentos es condición necesaria, aunque no suficiente, para garantizar la seguridad alimentaria de una población determinada."),
                        p("FoodPrice es una plataforma creada con el objetivo de proporcionar una herramienta accesible que permita evaluar la dimensión económica de la seguridad alimentaria, utilizando métricas basadas en dietas de costo mínimo. Estas métricas permiten monitorear este componente dentro de los sistemas alimentarios locales."),
                        h3("¿Qué mide FoodPrice?"),
                        p("Específicamente, la plataforma FoodPrice permite estimar tres métricas de dietas de costo mínimo:"),
                        tags$ol(
                          tags$li(strong("CoCA (Costo mínimo de una dieta suficiente en energía): "),
                                  "proporciona los niveles de calorías necesarias para cubrir los requerimientos energéticos de un individuo según su edad, sexo y nivel de actividad física."),
                          tags$li(strong("CoNA (Costo mínimo de una dieta adecuada en nutrientes): "),
                                  "además de satisfacer los requerimientos energéticos, proporciona niveles adecuados de macro y micronutrientes dentro de límites seguros para prevenir deficiencias o toxicidad."),
                          tags$li(strong("CoRD (Costo mínimo de una dieta saludable o recomendada): "),
                                  "se adhiere a las guías alimentarias nacionales promoviendo una dieta variada y saludable a largo plazo.")
                        ),
                        p("Estas métricas permiten conocer el costo mínimo necesario para acceder económicamente a alimentos disponibles localmente que satisfacen requerimientos nutricionales. Combinadas con datos sobre gasto en alimentos, permiten calcular indicadores como la proporción de personas que pueden (o no) costear dietas de diferente calidad."),
                        
                        h3("¿Qué había antes?"),
                        p("La plataforma FoodPrice se basa en el paquete en R ", code("FoodpriceR"), ", que permite realizar estos cálculos directamente desde R."),
                        p("Puedes instalar el paquete con el siguiente código:"),
                        tags$pre("remotes::install_github(\"lea-puj/FoodpriceR\")"),
                        tags$pre("library(FoodpriceR)"),
                        p("La documentación completa del paquete está disponible en: ",
                          tags$a(href = "https://hdl.handle.net/10568/155125", target = "_blank", "https://hdl.handle.net/10568/155125")),
                        
                        h3("Documentos de interés"),
                        p("(Publicaciones clave)"),
                        
                        h3("Equipo"),
                        p("(Nombres, correos o enlaces del equipo)"),
                        
                        h3("Aliados"),
                        p("(Organizaciones o entidades que colaboran con el proyecto)"),
                        
                        br(),
                        tags$img(src = "logo_FoodPriceR.png", height = "150px")
                 )
               )
             )
)
