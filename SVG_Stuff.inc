	integer*4 maxXY
	parameter (maxXY = 1000)
	real*8 x1(maxXY),y1(maxXY),x2(maxXY),y2(maxXY)	! = x and y values of segments read off of the svg file. Used only to make the grid
	real*8 minX,maxX,minY,maxY
	common /SVG_Stuff/ x1,y1,x2,y2,minX,maxX,minY,maxY
	