
# Take only 75 nodes from the graph
df <- igraph::get.data.frame(x = egraph)
df <- sample_n(df, size = 75)
g <- graph_from_data_frame(df)
g <- set.vertex.attribute(g, "name", value=paste("v",1:vcount(g),sep=""))
print(g)
# VISUALIZATIONS...
## Setting the coordinates and adding a title label
# plot(g, edge.arrow.size=.5, vertex.color="gold", vertex.size=c(10, 10, 10) * 0.54,
#      vertex.frame.color="gray", vertex.label.color="black",
#      vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2, )

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


