require("data.table")
require("magrittr")
require("dplyr")
require("ggplot2")

xy_circle <- function(r, centre = c(0, 0), from = 0, to = 2*pi, colour = "red", npoints = 200){

	angle <- seq(from = from, to = to, length.out = npoints)
	xy_coordinates <- t(sapply(angle, function(x){r * c(cos(x), sin(x))}))
	xy_coordinates <- t(apply(xy_coordinates, 1, function(x){c(x + centre)}))

	xy_coordinates <- data.table::as.data.table(xy_coordinates) %>%
						.[, a := angle] %>%
						.[, c := colour]

	names(xy_coordinates)[1:2] <- c("x", "y")
	return(xy_coordinates)

}





xy_rectangle <- function(side_lengths, centre = c(0, 0), colour = "red", npoints = 200){

	proportions <- (1/2) * side_lengths / sum(side_lengths)	
	offset <- side_lengths/2

	sides <- list()
	sides$bottom <- seq(from = 0, to = side_lengths[1], length.out = floor(proportions[1] * npoints)) - offset[1]
	sides$bottom <- data.table::as.data.table(sides$bottom) %>%
						.[, y := -offset[2]] %>%
						.[, s := "bottom"]
	
	names(sides$bottom)[1] <- "x"

	sides$top <- data.table::copy(sides$bottom) %>%
					.[, y := y + side_lengths[2]] %>%
					.[, s := "top"]


	sides$left<- seq(from = 0, to = side_lengths[2], length.out = ceiling(proportions[2] * npoints)) - offset[2]
	sides$left <- data.table::as.data.table(sides$left) %>%
						.[, x := -offset[1]] %>%
						.[, s := "left"]

	names(sides$left)[1] <- "y"

	sides$right <- data.table::copy(sides$left) %>%
					.[, x := x + side_lengths[1]] %>%
					.[, s := "right"]


	sides <- list(sides$left, sides$top, sides$right[order(-y)], sides$bottom[order(-x)])

	xy_coordinates <- data.table::copy(dplyr::bind_rows(sides)) %>%
						.[, x := x + centre[1]] %>%
						.[, y := y + centre[2]] %>%
						.[, c := colour]

 	return(xy_coordinates)

}


xy_line <- function(r, angle, centre = c(0, 0), colour = "red", npoints = 200){

	r_seq <- seq(from = 0, to = r, length.out = npoints)
	xy_coordinates <- t(sapply(r_seq, function(x){x * c(cos(angle), sin(angle))}))

	xy_coordinates <- data.table::as.data.table(xy_coordinates) %>%
						.[, c := colour]

	names(xy_coordinates)[1:2] <- c("x", "y")

	if(angle == 0 | angle == pi | angle == -pi){

		xy_coordinates[, y := 0]

	} else if(angle == pi/2 | angle == -pi/2 | angle == 3*pi/2 | angle == -3*pi/2){

		xy_coordinates[, x := 0]

	} 

	return(xy_coordinates)

}


xy_corner_ellipse <- function(xy_from, xy_to, colour = "red", npoints = 200){

	r <- abs(xy_to - xy_from)
	signs <- sign(xy_to - xy_from)

	if(signs[1] == 1 & signs[2] == 1){

		angle <- seq(from = pi, to = pi/2, length.out = npoints)

	} else if(signs[1] == 1 & signs[2] == -1){

		angle <- seq(from = pi/2, to = 0, length.out = npoints)

	} else if(signs[1] == -1 & signs[2] == -1){

		angle <- seq(from = 0, to = -pi/2, length.out = npoints)

	} else {

		angle <- seq(from = -pi/2, to = -pi, length.out = npoints)

	}

	xy_coordinates <- t(sapply(angle, function(x){r * c(cos(x), sin(x))}))
	xy_coordinates <- data.table::as.data.table(xy_coordinates) %>%
						.[, c := colour]

	names(xy_coordinates)[1:2] <- c("x", "y")	
	offset <- xy_from - unlist(xy_coordinates[1, c("x", "y"), with = FALSE])
	xy_coordinates <- xy_coordinates[, x := x + offset[1]] %>%
						.[, y := y + offset[2]]


	return(xy_coordinates)

}


xy_rectangle_ellipse_corner <- function(side_lengths, p_side, centre = c(0, 0), colour = "red", npoints = 200){

	rectangle <- xy_rectangle(side_lengths, centre, colour, npoints = 200)

	length_cut <- (1 - p_side) * side_lengths[2]
	p_side <- c(1 - length_cut/side_lengths[1], p_side)


	limits <- lapply(as.list(c(1:2)), function(i){centre[i] + c(-1, 1) * side_lengths[i]/2 * p_side[i]})	
	names(limits) <- c("x", "y")

	out_of_bounds <- list()
	out_of_bounds$top_left <- rectangle[x <= limits$x[1] & y >= limits$y[2], which = TRUE]
	out_of_bounds$top_right <- rectangle[x >= limits$x[2] & y >= limits$y[2], which = TRUE]
	out_of_bounds$bottom_right <- rectangle[x >= limits$x[2] & y <= limits$y[1], which = TRUE]
	out_of_bounds$bottom_left <- rectangle[x <= limits$x[1] & y <= limits$y[1], which = TRUE]

	from_to <- lapply(out_of_bounds, function(i){

			out <- list()

			if(length(unique(diff(i))) == 1){

				out$from <- unlist(rectangle[i[1], c("x", "y"), with = FALSE])
				out$to <- unlist(rectangle[i[length(i)], c("x", "y"), with = FALSE])
				return(out)

			} else {

				cutpoint <- which(diff(i) != 1)
				out$from <- unlist(rectangle[i[cutpoint + 1], c("x", "y"), with = FALSE])
				out$to <- unlist(rectangle[i[cutpoint], c("x", "y"), with = FALSE])

			}
			return(out)

		})

	npoints_fill <- lapply(out_of_bounds, length)

	for(i in 1:length(out_of_bounds)){

		replace_with <- xy_corner_ellipse(from_to[[i]]$from, from_to[[i]]$to, colour = colour, npoints = npoints_fill[[i]])

		rectangle[out_of_bounds[[i]], x := replace_with$x] %>%
					.[out_of_bounds[[i]], y := replace_with$y] %>%
					.[out_of_bounds[[i]], s := paste("ellipse", names(out_of_bounds)[i], sep = "_")]

	}

	index <- rectangle[s == "ellipse_bottom_left", which = TRUE]
	rectangle <- rbind(rectangle[index], rectangle[-index])
	rectangle <- rbind(rectangle, rectangle[1])	

	return(rectangle)

}


#Main function
draw_hockey_rink <- function(filename = "hockey_rink"){

	#Outer section
	border_xy <- xy_rectangle_ellipse_corner(side_lengths = c(60, 30), p_side = 0.58, colour = "black")

    #blue lines
    blue_lines <- xy_rectangle(side_lengths = c(18, 30), colour = "blue")
    rink <- ggplot(border_xy, aes(x = x, y = y)) + 
    		geom_path(data = blue_lines[s == "left"], aes(x = x, y = y), color = "blue", size = 1.5) +
    		geom_path(data = blue_lines[s == "right"], aes(x = x, y = y), color = "blue", size = 1.5)

    #red lines, side
    index <- order(abs(border_xy$x - 26))[1:2]
    height <- abs(diff(border_xy[index]$y))

    red_lines <- xy_rectangle(side_lengths = c(26*2, height), colour = "red")
    rink <- rink + 
    		geom_path(data = red_lines[s == "left"], aes(x = x, y = y), color = "red", size = 1) +
    		geom_path(data = red_lines[s == "right"], aes(x = x, y = y), color = "red", size = 1)

	#semicircle, middle
	rink <- rink + geom_path(data = xy_circle(r = 3, centre = c(0, -15), from = 0, to = pi), color = "red")


    #red line, centre
    rink <- rink + 
    		geom_path(data = xy_line(15, pi/2), aes(x = x, y = y), color = "red", size = 1.5) +
    		geom_path(data = xy_line(15, -pi/2), aes(x = x, y = y), color = "red", size = 1.5) + 
			geom_path(colour = "black", size = 1) +
			coord_fixed()


	#red circles
	centers <- list()
	centers$top_right <- c(-20, 7)
	centers$top_left <- c(20, 7)
	centers$bottom_left <- c(20, -7)
	centers$bottom_right <- c(-20, -7)

	for(i in 1:4){

		rink <- rink + geom_path(data = xy_circle(4.5, centers[[i]]), aes(x = x, y = y), color = "red")

	}

	#red dots, outer
	centers <- lapply(centers, function(x){

		out <- data.table::as.data.table(t(as.matrix(x)))
		names(out) <- c("x", "y")
		return(out)

	})


	for(i in 1:4){

		rink <- rink + geom_point(data = centers[[i]], aes(x = x, y = y), color = "red")

	}

	#red dots, inner
	centers$top_right <- c(-7, 7)
	centers$top_left <- c(7, 7)
	centers$bottom_left <- c(7, -7)
	centers$bottom_right <- c(-7, -7)
	centers <- lapply(centers, function(x){

		out <- data.table::as.data.table(t(as.matrix(x)))
		names(out) <- c("x", "y")
		return(out)

	})

	for(i in 1:4){

		rink <- rink + geom_point(data = centers[[i]], aes(x = x, y = y), color = "red")

	}

	#midpoint
	midpoint <- data.table::as.data.table(t(c(0, 0)))
	names(midpoint) <- c("x", "y")
	rink <- rink + geom_point(data = midpoint, aes(x = x, y = y), color = "blue", size = 0.5)

	#mid circle
	rink <- rink + geom_path(data = xy_circle(r = 4.5), color = "blue")

	#goals
	rink <- rink + 
			geom_path(data = xy_circle(r = 2, centre = c(-26, 0), from = pi/2, to = -pi/2), color = "blue") +
			geom_path(data = xy_circle(r = 2, centre = c(26, 0), from = pi/2, to = 3*pi/2), color = "blue")


	#remove axis labels
	rink <- rink + xlab("") + ylab("")


	ggsave(file = paste(filename, ".png", sep = ""))
	print(paste("File saved at: ", getwd(), "/", filename, sep = ""))
	print(rink)

}


