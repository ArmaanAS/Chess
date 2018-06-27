##install.packages('package-name', repos="http://cran.r-project.org")
if(require("png")){
    print("png has loaded correctly")
} else {
    print("Trying to install png...")
    install.packages('png', repos="http://cran.r-project.org")
    if(require("png")){
        print("png installed and loaded!")
    } else {
        stop("Could not install png!")
    }
}
library("grid")
library('png')

board	 	<<- rep(list(rep(0, 8)), 8) # 8x8 Board
ob			<<- board
selected	<<- NULL
running		<<- TRUE	
selected	<<- NULL
possible	<<- matrix(nrow = 2)
turn		<<- 'w'
dead		<<- NULL
moved		<<- FALSE
wkngchk		<<- FALSE
bkngchk		<<- FALSE
wkngmvd		<<- FALSE	# White king moved (castling)
bkngmvd		<<- FALSE
wlrkmvd		<<- FALSE	# Rooks moved? Left / right && black / white
wrrkmvd		<<- FALSE	# WhiteRightRooKMoVeD -> wrrkmvd
blrkmvd		<<- FALSE
brrkmvd		<<- FALSE

setwd(dirname(parent.frame(2)$ofile))

pieces = list(
	wrook = list(image = readPNG('wrook.png')), 
	wknight = list(image = readPNG('wknight.png')),
	wpawn = list(image = readPNG('wpawn.png')),
	wbishop = list(image = readPNG('wbishop.png')),
	wqueen = list(image = readPNG('wqueen.png')),
	wking = list(image = readPNG('wking.png')),
	brook = list(image = readPNG('brook.png')), 
	bknight = list(image = readPNG('bknight.png')),
	bpawn = list(image = readPNG('bpawn.png')),
	bbishop = list(image = readPNG('bbishop.png')),
	bqueen = list(image = readPNG('bqueen.png')),
	bking = list(image = readPNG('bking.png'))
)

hl <<- readPNG('highlight.png')
hvr <<- readPNG('hover.png')
chk <<- readPNG('check.png')

drawPiece <- function(p, x, y) {
	if (p == 0) return()
	p = pieces[[p]][['image']] # Get the image file
	grid.raster(p, 	# Draw the image
		(x * 2 - 1)/16, 
		(y * 2 - 1)/16, 
		1/8, 
		1/8, 
		interpolate = T
	)
}

highlight <- function(x, y) {
	if (is.null(selected) || selected[1] != x || selected[2] != y) return()
	if (getBoard(x, y) == 0) return()
	
	grid.raster(hl, # Draw highlight image
		(x * 2 - 1) / 16, 
		(y * 2 - 1) / 16, 
		1 / 8, 
		1 / 8, 
		interpolate = T
	)
}

redrawTile <- function(x, y) {
	grid.rect( # Redraw background tile colour
		(x - 1) / 8 + 1/16, 
		(y - 1) / 8 + 1/16, 
		1/8, 
		1/8,
		gp = gpar(
			col = "#F0F0F0",
			fill = ifelse(
				(x + y) %% 2 == 0, 
				"#D18B47", 
				"#FFCE9E"
			)
		)
	)
	
	# Redraw piece
	drawPiece(getBoard(x, y), x, y)
	
	# If it is selected, highlight it
	highlight(x, y)
}

setBoard <- function(x, y, p) {
	board[[x]][[y]] <<- p
	redrawTile(x, y)
}

getBoard <- function(x, y) {
	if (board[[x]][[y]] == 0) return(0)
	return(board[[x]][[y]])
}

# Board initial setup
{
setBoard(1, 1, 'wrook')	# White 1st layer
setBoard(2, 1, 'wknight')
setBoard(3, 1, 'wbishop')
setBoard(4, 1, 'wqueen')
setBoard(5, 1, 'wking')
setBoard(6, 1, 'wbishop')
setBoard(7, 1, 'wknight')
setBoard(8, 1, 'wrook')

setBoard(1, 8, 'brook') # Black 1st layer
setBoard(2, 8, 'bknight')
setBoard(3, 8, 'bbishop')
setBoard(4, 8, 'bqueen')
setBoard(5, 8, 'bking')
setBoard(6, 8, 'bbishop')
setBoard(7, 8, 'bknight')
setBoard(8, 8, 'brook')

setBoard(1, 2, 'wpawn')	# White pawn layer
setBoard(2, 2, 'wpawn')
setBoard(3, 2, 'wpawn')
setBoard(4, 2, 'wpawn')
setBoard(5, 2, 'wpawn')
setBoard(6, 2, 'wpawn')
setBoard(7, 2, 'wpawn')
setBoard(8, 2, 'wpawn')

setBoard(1, 7, 'bpawn')	# Black pawn layer
setBoard(2, 7, 'bpawn')
setBoard(3, 7, 'bpawn')
setBoard(4, 7, 'bpawn')
setBoard(5, 7, 'bpawn')
setBoard(6, 7, 'bpawn')
setBoard(7, 7, 'bpawn')
setBoard(8, 7, 'bpawn')
}

drawBoard <- function() {
	grid.rect(0.5, 0.5, 1, 1, # Dark grid slots
		gp = gpar(
			col = "#F0F0F0", # Outline
			fill = "#D18B47" # Filling
		)
	)
	
	for (y in 0:7 / 8) {	# Light grid slots
		for (x in 0:3 * 2 + 1) {
			grid.rect(
				(x - (y * 8) %% 2) / 8 + 1/16, 
				y + 1/16, 
				1/8, 
				1/8,
				gp = gpar(
					col = "#F0F0F0", # Outline
					fill = "#FFCE9E" # Fill colour
				)
			)
		}
	}
}

newScreen <- function() {
	grid.newpage() # Empty screen
	pushViewport(viewport(width = 0.9, height = 0.9)) 
}

redraw <- function() {
	newScreen()
	drawBoard()
	
	# Drawing each piece to the board
	for (x in 1:8) {	
		for (y in 1:8) {
			# If slot is empty, go to next slot
			if (getBoard(x, y) == 0) next 
			
			drawPiece(getBoard(x, y), x, y) # Draw the piece
		}
	}
	
	select() # Draw anything selected
}

hover <- function(x, y) {
	grid.raster(hvr, # Draw highlight image
		(x - 1) / 8 + 1 / 16, 
		(y - 1) / 8 + 1 / 16, 
		1 / 8, 
		1 / 8, 
		interpolate = T
	)
}

warn <- function(x, y) {
	grid.raster(chk, # Draw highlight image
		(x - 1) / 8 + 1 / 16, 
		(y - 1) / 8 + 1 / 16, 
		1 / 8, 
		1 / 8, 
		interpolate = T
	)
}

select <- function() {
	if (is.null(selected)) return() 
	
	x = selected[1]
	y = selected[2]
	p = getBoard(x, y)
	
	if (p == 0) return()
	
	highlight(x, y)
}

drawPossible <- function() {
	for (i in 2:ncol(possible)) {
		hover(possible[1, i], possible[2, i])
	}
}

removePossible <- function() {
	if (length(possible) == 2) return()
	for (i in 2:ncol(possible)) {
		redrawTile(possible[1, i], possible[2, i])
	}
	possible <<- matrix(nrow = 2)
}

inPossible <- function(x, y) {
	for (i in 2:ncol(possible)) {
		if (x == possible[1, i] && y == possible[2, i]) return(TRUE)
	}
	return(FALSE)
}

kingCheck <- function() {
	wkngchk <<- FALSE
	bkngchk <<- FALSE
	for (x in 1:8) {
		for (y in 1:8) {
			if (getBoard(x, y) == 'wking') {
				if (whiteCheck(x, y)) {
					wkngchk <<- TRUE
				}
			} else if (getBoard(x, y) == 'bking') {
				if (blackCheck(x, y)) {
					bkngchk <<- TRUE
				}
			}
		}
	}
}

whiteCheck <- function(x, y) {
	redrawTile(x, y)
	for (xx in c(1, 0)) {
		for (yy in c(1, 0)) {
			if (xx == yy) next
			for (i in c(1, -1)) {
				for (a in 1:7) {
					if (x + (a * i * xx) < 1 || x + (a * i * xx) > 8) break
					if (y + (a * i * yy) < 1 || y + (a * i * yy) > 8) break
					
					bp = getBoard(x + (a * i * xx), y + (a * i * yy))
					colour = substring(bp, 1, 1)
					
					if (colour == 'w') break
					if (colour == 'b' && (bp == 'bqueen' || bp == 'brook')) {
						warn(x, y)
						return(TRUE)
					}
				}
			}
		}
	}
	
	for (a in c(1, -1)) {
		for (b in c(1, -1)) {
			for (i in 1:7) {
				if (x + (i * a) < 1 || y + (i * b) < 1) break
				if (x + (i * a) > 8 || y + (i * b) > 8) break
				
				bp = getBoard(x + (i * a), y + (i * b))
				colour = substring(bp, 1, 1)
				
				if (colour == 'w') break
				if (colour == 'b' && (bp == 'bqueen' || bp == 'bbishop')) {
					warn(x, y)
					return(TRUE)
				}
			}
		}
	}
	
	for (i in c(1, -1)) {
		for (j in c(1, -1)) {
			for (a in c(1, 2)) {
				for (b in c(1, 2)) {
					if (a == b) next
					if (x + (i * a) < 1 || y + (j * b) < 1) next
					if (x + (i * a) > 8 || y + (j * b) > 8) next
					
					bp = getBoard(x + (i * a), y + (j * b))
					colour = substring(bp, 1, 1)
					
					if (colour == 'w') break
					if (colour == 'b' && bp == 'bknight') {
						warn(x, y)
						return(TRUE)
					}
				}
			}
		}
	}
	
	for (i in c(1, -1)) {
		if (x + i > 8 || x + i < 1) next
		if (y + 1 > 8) next
		
		bp = getBoard(x + i, y + 1)
		colour = substring(bp, 1, 1)
		
		if (colour == 'b' && bp == 'bpawn') {
			warn(x, y)
			return(TRUE)
		}
	}
	return(FALSE)
}

blackCheck <- function(x, y) {
	redrawTile(x, y)
	for (xx in c(1, 0)) {
		for (yy in c(1, 0)) {
			if (xx == yy) next
			for (i in c(1, -1)) {
				for (a in 1:7) {
					if (x + (a * i * xx) < 1 || x + (a * i * xx) > 8) break
					if (y + (a * i * yy) < 1 || y + (a * i * yy) > 8) break
					
					bp = getBoard(x + (a * i * xx), y + (a * i * yy))
					colour = substring(bp, 1, 1)
					
					if (colour == 'b') break
					if (colour == 'w' && (bp == 'wqueen' || bp == 'wrook')) {
						warn(x, y)
						return(TRUE)
					}
				}
			}
		}
	}
	
	for (a in c(1, -1)) {
		for (b in c(1, -1)) {
			for (i in 1:7) {
				if (x + (i * a) < 1 || y + (i * b) < 1) break
				if (x + (i * a) > 8 || y + (i * b) > 8) break
				
				bp = getBoard(x + (i * a), y + (i * b))
				colour = substring(bp, 1, 1)
				
				if (colour == 'b') break
				if (colour == 'w' && (bp == 'wqueen' || bp == 'wbishop')) {
					warn(x, y)
					return(TRUE)
				}
			}
		}
	}
	
	for (i in c(1, -1)) {
		for (j in c(1, -1)) {
			for (a in c(1, 2)) {
				for (b in c(1, 2)) {
					if (a == b) next
					if (x + (i * a) < 1 || y + (j * b) < 1) next
					if (x + (i * a) > 8 || y + (j * b) > 8) next
					
					bp = getBoard(x + (i * a), y + (j * b))
					colour = substring(bp, 1, 1)
					
					if (colour == 'w' && bp == 'wknight') {
						warn(x, y)
						return(TRUE)
					}
				}
			}
		}
	}
	
	for (i in c(1, -1)) {
		if (x + i > 8 || x + i < 1) next
		if (y + 1 > 8) next
		
		bp = getBoard(x + i, y + 1)
		colour = substring(bp, 1, 1)
		
		if (colour == 'w' && bp == 'wpawn') {
			warn(x, y)
			return(TRUE)
		}
	}
	return(FALSE)
}

switchTurn <- function() {
	turn <<- ifelse(turn == 'w', 'b', 'w')
}

onSelect <- function(x, y) {
	# Highlighting:
	# Is mouse in bounds?
	if (x > 8 || x < 1 || y > 8 || y < 1) return()
	p = getBoard(x, y)
	colour = substring(p, 1, 1)
	
	if (length(possible) == 2) {
		if (turn == 'w') {
			if (colour != 'w') return()
		} else {
			if (colour != 'b') return()
		}
	} else {
		if (!inPossible(x, y)) {
			if (colour != 0 && colour != turn) {
				removePossible()
				a = selected[1]
				b = selected[2]
				selected <<- NULL
				redrawTile(a, b)
				return()
			}
		}
	}
	
	if (length(possible) > 2) {
		if (inPossible(x, y)) {
			# Piece type selected				
			bp = getBoard(selected[1], selected[2])
			
			# Move piece to selected position
			if (bp == 'wpawn' && y == 8) {
				setBoard(x, y, 'wqueen')
			} else if (bp == 'bpawn' && y == 1) {
				setBoard(x, y, 'bqueen')
			} else if (p == 'wrook' && bp == 'wking') {
				if (x == 8) {
					setBoard(5, 1, 0)
					setBoard(8, 1, 0)
					setBoard(7, 1, 'wking')
					setBoard(6, 1, 'wrook')
					redraw()
				} else if (x == 1) {
					setBoard(5, 1, 0)
					setBoard(1, 1, 0)
					setBoard(3, 1, 'wking')
					setBoard(4, 1, 'wrook')
					redraw()
				}
			} else if (p == 'brook' && bp == 'bking') {
				if (x == 8) {
					setBoard(5, 8, 0)
					setBoard(8, 8, 0)
					setBoard(7, 8, 'bking')
					setBoard(6, 8, 'brook')
					redraw()
				} else if (x == 1) {
					setBoard(5, 8, 0)
					setBoard(1, 8, 0)
					setBoard(3, 8, 'bking')
					setBoard(4, 8, 'brook')
					redraw()
				}
			} else {
				if (bp == 'wking') wkngmvd <<- TRUE
				if (bp == 'bking') bkngmvd <<- TRUE
				
				if (bp == 'wrook') {
					if (selected[1] == 1) wlrkmvd <<- TRUE
					if (selected[1] == 8) wrrkmvd <<- TRUE
				} else if (bp == 'brook') {
					if (selected[1] == 1) blrkmvd <<- TRUE
					if (selected[1] == 8) brrkmvd <<- TRUE
				}
				
				if (p != 0) dead <<- c(dead, p)
				
				setBoard(x, y, bp)
			}			
			
			# Remove original piece
			setBoard(selected[1], selected[2], 0)
			
			# Remove and redraw all possible tile positions
			removePossible()
			
			# Set to next player's turn
			switchTurn()
			
			moved <<- TRUE
			return()
		}
	}

	# Is mouse position the same as before
	if (!is.null(selected) && x == selected[1] && y == selected[2]) return() 
	
	if (!is.null(selected)) {
		a = selected[1]
		b = selected[2]
		
		if (getBoard(x, y) == 0) {
			selected <<- NULL
		} else {
			selected <<- c(x, y)
			redrawTile(x, y)
		}		
		
		redrawTile(a, b)
	} else if (getBoard(x, y) != 0) {
		selected <<- c(x, y)
		redrawTile(x, y)
	}
	# Highlighting
	
	# Removing / redrawing over old hovered positions
	if (length(possible) > 2) {
		removePossible()
	}
	
	# Displaying possible moves
	if (p == 0) return()
	
	# Moves for a ROOK
	if (p == 'wrook' || p == 'brook') {
		for (xx in c(1, 0)) {
			for (yy in c(1, 0)) {
				if (xx == yy) next
				for (i in c(1, -1)) {
					for (a in 1:7) {
						if (x + (a * i * xx) < 1 || x + (a * i * xx) > 8) break
						if (y + (a * i * yy) < 1 || y + (a * i * yy) > 8) break
						
						bp = getBoard(x + (a * i * xx), y + (a * i * yy))
						
						if (bp == 0) {
							possible <<- cbind(possible, c(x + (a * i * xx), y + (a * i * yy)))
						} else if (substring(bp, 1, 1) != colour) {
							possible <<- cbind(possible, c(x + (a * i * xx), y + (a * i * yy)))
							break
						} else {
							break
						}
					}
				}
			}
		}
	}
	
	# Moves for a BISHOP
	if (p == 'wbishop' || p == 'bbishop') {
		for (a in c(1, -1)) {
			for (b in c(1, -1)) {
				for (i in 1:7) {
					if (x + (i * a) < 1 || y + (i * b) < 1) break
					if (x + (i * a) > 8 || y + (i * b) > 8) break
					
					bp = getBoard(x + (i * a), y + (i * b))
					
					if (bp == 0) {
						possible <<- cbind(possible, c(x + (i * a), y + (i * b)))
					} else if (substring(bp, 1, 1) != colour) {
						possible <<- cbind(possible, c(x + (i * a), y + (i * b)))
						break
					} else {
						break
					}
				}
			}
		}
	}
	
	# Moves for a KNIGHT
	if (p == 'wknight' || p == 'bknight') {
		for (i in c(1, -1)) {
			for (j in c(1, -1)) {
				for (a in c(1, 2)) {
					for (b in c(1, 2)) {
						if (a == b) next
						if (x + (i * a) < 1 || y + (j * b) < 1) next
						if (x + (i * a) > 8 || y + (j * b) > 8) next
						
						bp = getBoard(x + (i * a), y + (j * b))
						
						if (bp == 0 || substring(bp, 1, 1) != colour) {
							possible <<- cbind(possible, c(x + (i * a), y + (j * b)))
						} else {
							next
						}
					}
				}
			}
		}
	}
	
	# Moves for a QUEEN
	if (p == 'wqueen' || p == 'bqueen') {
		for (a in c(1, -1)) {
			for (b in c(1, -1)) {
				for (i in 1:7) {
					if (x + (i * a) < 1 || y + (i * b) < 1) break
					if (x + (i * a) > 8 || y + (i * b) > 8) break
					
					bp = getBoard(x + (i * a), y + (i * b))
					
					if (bp == 0) {
						possible <<- cbind(possible, c(x + (i * a), y + (i * b)))
					} else if (substring(bp, 1, 1) != colour) {
						possible <<- cbind(possible, c(x + (i * a), y + (i * b)))
						break
					} else {
						break
					}
				}
			}
		}
		
		for (xx in c(1, 0)) {
			for (yy in c(1, 0)) {
				if (xx == yy) next
				for (i in c(1, -1)) {
					for (a in 1:7) {
						if (x + (a * i * xx) < 1 || x + (a * i * xx) > 8) break
						if (y + (a * i * yy) < 1 || y + (a * i * yy) > 8) break
						
						bp = getBoard(x + (a * i * xx), y + (a * i * yy))
						
						if (bp == 0) {
							possible <<- cbind(possible, c(x + (a * i * xx), y + (a * i * yy)))
						} else if (substring(bp, 1, 1) != colour) {
							possible <<- cbind(possible, c(x + (a * i * xx), y + (a * i * yy)))
							break
						} else {
							break
						}
					}
				}
			}
		}
	}
	
	# Moves for a WHITE PAWN
	if (p == 'wpawn') {
		for (i in 1:2) {
			if (i == 2 && y != 2) break
			if (y + i > 8) break
			
			bp = getBoard(x, y + i)
			
			if (bp == 0) {
				possible <<- cbind(possible, c(x, y + i))
			} else if (substring(bp, 1, 1) != colour) {
				break
			} else {
				break
			}
		}
		
		for (i in c(1, -1)) {
			if (y + 1 > 8) break
			if (x + i < 1 || x + i > 8) next
			
			bp = getBoard(x + i, y + 1)
			if (bp == 0) next
			
			if (substring(bp, 1, 1) != colour) {
				possible <<- cbind(possible, c(x + i, y + 1))
			}
		}
	}
	
	# Moves for a BLACK PAWN
	if (p == 'bpawn') {
		for (i in -1:-2) {
			if (i == -2 && y != 7) break
			if (y + i < 1) break
			
			bp = getBoard(x, y + i)
			
			if (bp == 0) {
				possible <<- cbind(possible, c(x, y + i))
			} else {
				break
			}
		}
		
		for (i in c(1, -1)) {
			if (y - 1 < 1) break
			if (x + i < 1 || x + i > 8) next
			
			bp = getBoard(x + i, y - 1)			
			if (bp == 0) next
			
			if (substring(bp, 1, 1) != colour) {
				possible <<- cbind(possible, c(x + i, y - 1))
			}
		}
	}
	
	# Moves for a KING
	if (p == 'bking' || p == 'wking') {
		for (a in 1:-1) {
			for (b in 1:-1) {
				if (a == 0 && b == 0) next
				if (y + b < 1 || y + b > 8) next
				if (x + a < 1 || x + a > 8) next
				
				bp = getBoard(x + a, y + b)
				
				if (bp == 0) {
					possible <<- cbind(possible, c(x + a, y + b))
				} else if (substring(bp, 1, 1) != colour) {
					possible <<- cbind(possible, c(x + a, y + b))
					next
				} else {
					next
				}
			}
		}
	}
	
	# Castling for a WHITE KING
	if (p == 'wking' && !wkngmvd) {
		if (!wrrkmvd) {
			if (getBoard(6, 1) == 0 && getBoard(7, 1) == 0) {
				if (getBoard(8, 1) == 'wrook') {
					possible <<- cbind(possible, c(8, 1))
				}
			}
		} 
		if (!wlrkmvd) {
			if (getBoard(4, 1) == 0 && getBoard(3, 1) == 0 && getBoard(2, 1) == 0) {
				if (getBoard(1, 1) == 'wrook') {
					possible <<- cbind(possible, c(1, 1))
				}
			}
		}
	}
	
	# Castling for a BLACK KING
	if (p == 'bking' && !bkngmvd) {
		if (!brrkmvd) {
			if (getBoard(6, 8) == 0 && getBoard(7, 8) == 0) {
				if (getBoard(8, 8) == 'brook') {
					possible <<- cbind(possible, c(8, 8))
				}
			}
		} 
		if (!blrkmvd) {
			if (getBoard(4, 8) == 0 && getBoard(3, 8) == 0 && getBoard(2, 8) == 0) {
				if (getBoard(1, 8) == 'brook') {
					possible <<- cbind(possible, c(1, 8))
				}
			}
		}
	}
 	
	if (length(possible) > 2) {
		drawPossible()
	}
}

mouse <- function(btn, x, y) {
	# Screen pos to board pos
	boxx = ceiling((round(x, 2) - 0.05) * 9)
	boxy = ceiling((round(y, 2) - 0.05) * 9)
	
	onSelect(boxx, boxy)
	
	if (!moved) return()
	
	wk = wkngchk	# White / black king in check before?
	bk = bkngchk
	
	kingCheck()
	
	if ((wk && wkngchk) || (bk && bkngchk)) { # If king is still in check, invalid move
		board <<- ob
		switchTurn()
		redraw()
		print('Invalid move, King is in check.')
	} else {
		ob <<- board
	}
	
	moved <<- FALSE
}

gameloop <- function() {
	redraw()
	while (TRUE) {
		if (!running) break		
		getGraphicsEvent("",
			onMouseDown = mouse,
			onKeybd = NULL
		)
	}
}

gameloop()