# fractal generator.r
# by John Darcy, 1 MAY 2017

# These are the parameters for the fractal. 
# Edit them to make different fractals.

n_vertices <- 6      # attractor vertices - changes symmetry. 3 makes triangular symmetry. 5 makes pentagonal symmetry.
dist_frac <- 3/5     # attraction distance - changes the shape of the fractal. x/2 works well with odd vertices, x/5 works well with even vertices.
scaling <- 1         # zoom in or zoom out on the fractal
dotcol <- "orange"   # color of the fractal
bgcol <- "brown"     # color of the background
dotscale <- 0.4      # size of dots that are used to draw the fractal. 
iters <- 20000       # number of dots to draw. more dots take more time.
ncores <- 4          # number of cores for parallelization. doesn't actually speed anything up since drawing the fractal is rate-limiting.

point_between <- function(x0, y0, x1, y1, frac){
	dx <- x1 - x0
	dy <- y1 - y0
	x2 <- x0 + frac * dx
	y2 <- y0 + frac * dy
	return(c(x2, y2))
}

newpoint <- function(point0, vertices, frac){
	x0 <- point0[1]
	y0 <- point0[2]
	target <- vertices[sample(1: nrow(vertices), 1),]
	output <- point_between(x0, y0, target[1], target[2], frac)
	return(output)
}

# generate starting coordinates for a regular polygon
# done using a fixed radius and equal angles
circle_diam_frac <- scaling
vertices <- mat.or.vec(n_vertices,2)
colnames(vertices) <- c("x", "y")
theta <- 0
thetafrac <- (2 * pi)/n_vertices
centerx <- centery <- 100000 / 2
h <- (circle_diam_frac * 100000)/2
for(i in 1:n_vertices){
	dy <- sin(theta) * h
	dx <- cos(theta) * h
	xpoint <- centerx + dx
	ypoint <- centery + dy
	vertices[i,] <- c(xpoint, ypoint)
	theta <- theta + thetafrac
}	

if (ncores == 1){
	# start at vertex1 for simplicity
	cursorx <- vertices[1,1]
	cursory <- vertices[1,2]
	
	# make empty plot
	par(bg = bgcol)
	plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, 100000) ,ylim=c(0, 100000))
	
	# iterate and draw
	for(i in 1:iters){
		coords_i <- newpoint(c(cursorx, cursory), vertices, dist_frac)
		points(coords_i[1], coords_i[2], pch=19, cex=dotscale, col=dotcol)
		cursorx <- coords_i[1]
		cursory <- coords_i[2]
	}
	
} else if (ncores > 1){
	library(parallel)
	# create initial list of cursor points
	cursors <- rep(list(vertices[1,]), ncores)
	
	# make empty plot
	par(bg = bgcol)
	plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, 100000) ,ylim=c(0, 100000))

	# fewer iterations required for parallel
	iters <- round(iters / ncores)
	
	# iterate and draw
	for(i in 1:iters){
		
		coords_i <- (mclapply(cursors, newpoint, vertices=vertices, frac=dist_frac))
		coords_i_a <- simplify2array(coords_i)

		
		points(coords_i_a[1,], coords_i_a[2,], pch=19, cex=dotscale, col=dotcol)
		cursors <- coords_i
	}


} else {
	print("invalid number of cores, try again.")
}
