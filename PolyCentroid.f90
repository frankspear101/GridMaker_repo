	Subroutine PolyCentroid(iPoly,xcenter,ycenter)
	! Routine to compute the centroid of a polygon
	! Method is described in the file "Polygon Center of Mass.pdf"
	
	Implicit none
	include "Grid_Stuff.inc"
	integer*4 i,iPoly,Node1,Node2,Node3
	real*8 x1,y1,x2,y2,x3,y3,xcenter,ycenter,area,a,xcntr,ycntr
	
	! Compute the area of each triangle in the polygon
	
	Node1 = PolyNodes(iPoly,1)
	x1 = nodeX(Node1)
	y1 = nodeY(Node1)
	area = 0.0d0
	xCenter = 0.0d0
	yCenter = 0.0d0
	Do i = 2,numPolyNodes(iPoly)-1
		Node2 = PolyNodes(iPoly,i)
		Node3 = PolyNodes(iPoly,i+1)
		x2 = nodeX(Node2)
		y2 = nodeY(Node2)
		x3 = nodeX(Node3)
		y3 = nodeY(Node3)
		! Calculate area of the triangle
		call get_area(x1,y1,x2,y2,x3,y3,a)
		if(a.eq.0.0d0)cycle			! if the triangle is null (flat) then area is 0 and there is no centroid
		area = area + a 	! sum the areas
		! compute the center of the triangle
		call get_center(x1,y1,x2,y2,x3,y3,xcntr,ycntr)
		xCenter = xCenter + a * xcntr
		yCenter = yCenter + a * ycntr		
		end do	
	xCenter = xCenter/area
	yCenter = yCenter/area
	return
	end
		
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$		
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$		
	Subroutine Get_Center(x1,y1,x2,y2,x3,y3,xcenter,ycenter)
	! routine to calculate the center of a triangle given 3 points
	implicit none
	real*8 x1,y1,x2,y2,x3,y3,xcenter,ycenter
	real*8 x1m,y1m,x2m,y2m,x3m,y3m,x4m,y4m,a1,a2,b1,b2

	x1m = x1			!	// get endpoints of first median
	y1m = y1			!	// get endpoints of first median
	x2m = (x2 + x3) / 2.0d0		!;	// get endpoints of first median
	y2m = (y2 + y3) / 2.0d0		!;	// get endpoints of first median

	x3m = x2			!;	// get endpoints of second median (only need two)
	y3m = y2			!;	// get endpoints of second median
	x4m = (x3 + x1) / 2.0d0		!;	// get endpoints of second median
	y4m = (y3 + y1) / 2.0d0		!;	// get endpoints of second median

	!// see if either median is vertical (slope == infinity)

	if (x1m.eq.x2m)then
		x1m = x3
		y1m = y3
		x2m = (x1 + x2)/2.0d0
		y2m = (y1 + y2)/2.0d0
		elseif (x3m.eq.x4m)then
		x3m = x3
		y3m = y3
		x4m = (x1 + x2)/2.0d0
		y4m = (y1 + y2)/2.0d0
		endif

		
	! if (x1 == x2)	// if so...
	! 	{
	! 	x1 = p3.x;	// use third median (can't be two vertical medians)
	! 	y1 = p3.y; // use third median
	! 	x2 = (p1.x + p2.x) / 2.0; // use third median
	! 	y2 = (p1.y + p2.y) / 2.0; // use third median
	! 	}
	! else if (x3 == x4)
	! 	{
	! 	x3 = p3.x;
	! 	y3 = p3.y;
	! 	x4 = (p1.x + p2.x) / 2.0;
	! 	y4 = (p1.y + p2.y) / 2.0;
	! 	}
	
	!long double a1, a2, b1, b2;
	a1 = (y2m - y1m) / (x2m - x1m)		!;	// compute slope of first median
	b1 = y1m - a1 * x1m			!;	// compute intercept of first median
	a2 = (y4m - y3m) / (x4m - x3m)		!;	// compute slope of second median
	b2 = y3m - a2 * x3m			!;	// compute intercept of second median

	!// solve a1 * x + b1 = a2 * x + b2
	xCenter = (b2 - b1) / (a1 - a2)		!;	// solve for x coordinate of intersection
	yCenter = a1 * xCenter + b1		!;	// solve for y coordinate of intersection
	!return cntr;	// return center as a point
	return
	end

	
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$		
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$		
	Subroutine Get_Area(x1,y1,x2,y2,x3,y3,area)
	! routine to calculate the area of a triangle
	implicit none
	real*8 x1,y1,x2,y2,x3,y3,area
	real*8 v1x,v1y,v2x,v2y
	! compute the side vectors
	v1x = x2 - x1
	v1y = y2 - y1
	v2x = x3 - x1
	v2y = y3 - y1
	! compute the cross product of the side vectors/2
	area = 0.5d0*(v1x*v2y - v1y*v2x)
	return
	end