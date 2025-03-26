!	Include file for Grid_Maker program
	integer*4 maxNodes,maxSegs,maxPoints,maxElements,maxPhases,maxPhCo,maxPolys
	parameter (maxNodes = 2000, maxSegs = 2000,maxPoints = 30,maxElements = 10)
	parameter (maxPolys = 2000, maxPhases = 10,maxPhCo = 10)
!	integer*4 maxXY
!	parameter (maxXY = 2000)


!	node variables
	Integer*4					&!
		numNodeSegs(maxNodes),			&! = number of segments that a node connects to
		nodeSegConnect(maxNodes,8), 		&! = index of the segments connected to a node. Each node can have up to 8 segments attached
		nodeSegEnd(maxNodes,8),			&! 1 = start at beginning of segment, 2 = start at end of segment
		nodeNodeConnect(maxNodes,8),		&! Nodes that connect to a node (at the end of the segment). 8 max. These are in the same order as nodeSegConnect
		numNodes				 ! = self evident

	real*8						&
		nodeX(maxNodes),nodeY(maxNodes)		! = x,y coordinates of the nodes of the hex grid

!	Segment variables
	integer*4					&!
		numSegs,				&! = number of total segments
		segNodes(maxSegs,2)			! nodes at the end of a segment
	real*8						&
		segX(maxSegs,2),			&
		segY(maxSegs,2),			&!
		segLength(maxSegs)

!	Polygon variables
	integer*4					&!
		numPolys,				&
		numPolyNodes(maxPolys),			&
		PolyNodes(maxPolys,200),			&! 200 is the maximum number of nodes in a single polygon
		PolySegs(maxPolys,200)
	real*8						&
		PolyCenterX(maxPolys),			&
		PolyCenterY(maxPolys)
		
		
	Common /Grid_Stuff/ numNodeSegs,nodeSegConnect,nodeSegEnd,numNodes,nodeX,nodeY,nodeNodeConnect,	&
		numSegs,segNodes,segX,segY,segLength,							&
		numPolys,numPolyNodes,PolyNodes,PolySegs,PolyCenterX,PolyCenterY
		
		
		