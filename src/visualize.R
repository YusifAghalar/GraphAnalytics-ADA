
temp<-delete_vertices(egraph,V(egraph)[degree(egraph)==0])
temp<-V(egraph)[order(V(egraph)$degree,decreasing=TRUE)[1:5]] 
g<-neighborhood(egraph,nodes=temp)


extractName <- function(x) strsplit(x, "@")[[c(1, 1)]]

width <- 1000
height <- 600
padding <- 60
# Take only 75 nodes from the graph
df <- igraph::get.data.frame(x = temp)
g <- graph_from_data_frame(df)
g <- set.vertex.attribute(g, "name", value= sapply(V(g)$name,extractName))
print(g)
# VISUALIZATIONS...
## Setting the coordinates and adding a title label
V(g)$weight <- degree(g)*3

plot(g, edge.arrow.size=.0005,  edge.width = 2,
     vertex.color="gold", vertex.size=V(g)$weight,
     vertex.frame.color="gray", vertex.label.color="black",
     vertex.label.cex=0.5, vertex.label.dist=0, edge.curved=0.2,  vertex.shape="circle",
     layout=norm_coords(layout.fruchterman.reingold(g),20000,50000,0,2000))

## METHOD 1: plot function

## METHOD 2: tcltk library
width <- 1000
height <- 600

# E(g)$width <- sample(1:75, ecount(egraph), replace=TRUE)
id <- tkplot (g, canvas.width = width, canvas.height = height,
              labels = NULL, label.color = "darkblue", label.font = NULL, 
              label.degree = -pi/4, label.dist = 0, vertex.color = "SkyBlue2", 
              vertex.size = 15, edge.color = "royalblue", edge.width = 0.5,
              edge.labels=NA)

padding <- 60
coords <- norm_coords(layout = layout.auto(g), 0+padding, width-padding,
                      50+padding, height-padding)
tk_set_coords(id, coords)
canvas <- tk_canvas(id)

width <- as.numeric(tkcget(canvas, "-width"))
height <- as.numeric(tkcget(canvas, "-height"))
tkcreate(canvas, "text", width / 2, 25, text = "Graph by S. Akhund & Y. Aghalarli",
         justify="center", font=tcltk::tkfont.create(family="helvetica",
                                                     size=20,weight="bold"))


