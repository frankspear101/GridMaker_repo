	Program GridMaker
	!Program to read an SVG file comprised of lines that intersect at nodes and to create the grid from these
	USE AWE_INTERFACES
      	implicit none

	TYPE(AWE_Canvas) :: GridPlot

	include "Grid_Stuff.inc"
	include "SVG_Stuff.inc"

	character*256 aline,title,GridFile,SaveFile
	character*1 quote
	integer*4 i,j,k,k1,icolstart,icolend,status,istart,iend,err,didone,isave,debug,ipoly
	real*8 YViewBox,shift,xcenter,ycenter
!	real*8 x1(maxXY),y1(maxXY),x2(maxXY),y2(maxXY)




1	continue
	call FSS_Alert('Alert','Open svg model file')

	open(16,file='',status = 'OLD',iostat = status)
	if(status.ne.0)then
		call FSS_Alert('Alert','Problem opening model file')
		stop
		endif
	INQUIRE (16, NAME=GridFile)
	write(*,*)'--------------'
	write(*,*)' Do you want to debug (lots of output)? 0 = no, 1 = yes'
	read(*,*)debug
	write(*,*)GridFile
!	read(16,*)Title
!	write(*,*)'Grid title line  ',Title
!	read(16,*)YviewBox		! used to convert Y down to Y up
	YviewBox = 576	
	quote = '"'	
	write(*,*)'         x1                y1                x2              y2'
	j = 1
	i = 1
	maxX = -1.d5
	minX =  1.d5
	maxY = -1.d5
	minY =  1.d5
10	continue
	Read(16,11,END = 20,err=99)aline
11	format(A256)
	if(aline(1:5).ne.'<line')go to 10		!skip everything that is not a line
	! Find the X1
	j = j + 1
	do k = 1,255
		k1 = k+1
		if(aline(k:k1).eq.'x1')then
			icolStart = k
			go to 12
			endif
		end do		
	write(*,*)' line = ',j
	call fss_alert('Alert',' The text x1 was not found')
12	continue
	call Findquote(aline,icolstart,err)
	icolstart = icolstart + 1			! the first number (x1) starts here
	icolend = icolstart
	call Findquote(aline,icolend,err)
	icolend = icolend - 1				! this is the end of the first number
	read(aline(icolstart:icolend),*)x1(i)

	icolstart = icolend + 2
	call Findquote(aline,icolstart,err)
	icolstart = icolstart + 1			! the first number (y1) starts here
	icolend = icolstart
	call Findquote(aline,icolend,err)
	icolend = icolend - 1				! this is the end 
	read(aline(icolstart:icolend),*)y1(i)
	y1(i) = yViewBox - y1(i)

	icolstart = icolend + 2
	call Findquote(aline,icolstart,err)
	icolstart = icolstart + 1			! the second number (x2) starts here
	icolend = icolstart
	call Findquote(aline,icolend,err)
	icolend = icolend - 1				! this is the end  
	read(aline(icolstart:icolend),*)x2(i)

	icolstart = icolend + 2
	call Findquote(aline,icolstart,err)
	icolstart = icolstart + 1			! the second number (y2) starts here
	icolend = icolstart
	call Findquote(aline,icolend,err)
	icolend = icolend - 1				! this is the end 
	read(aline(icolstart:icolend),*)y2(i)
	y2(i) = yViewBox - y2(i)
	write(*,15)i,x1(i),y1(i),x2(i),y2(i)
15	format(I5,4F15.8)

	x1(i) = int(x1(i) + 0.5)		!Round off to integers
	x2(i) = int(x2(i) + 0.5)		!Round off to integers
	y1(i) = int(y1(i) + 0.5)		!Round off to integers
	y2(i) = int(y2(i) + 0.5)		!Round off to integers

	if(x1(i).gt.maxX)maxX = x1(i)
	if(x2(i).gt.maxX)maxX = x2(i)
	if(x1(i).le.minX)minX = x1(i)
	if(x2(i).le.minX)minX = x2(i)
	if(y1(i).gt.maxY)maxY = y1(i)
	if(y2(i).gt.maxY)maxY = y2(i)
	if(y1(i).le.minY)minY = y1(i)
	if(y2(i).le.minY)minY = y2(i)

	
	i = i + 1
	go to 10

20	continue
	write(*,*)' minX, maxX = ',minX,maxX
	write(*,*)' minY, maxY = ',minY,maxY
	numSegs = i - 1

	! shift all values by minX or minY, depending on which is smaller (we don't want negative numbers
	if(minY.lt.minX)then
		shift = minY - 1
		else
		shift = minX - 1
		endif
	do i = 1,numSegs
		x1(i) = x1(i) - shift
		x2(i) = x2(i) - shift
		y1(i) = y1(i) - shift
		y2(i) = y2(i) - shift
		end do
	minX = minX - shift
	maxX = maxX - shift
	minY = minY - shift
	maxY = maxY - shift

	write(*,*)' '
	write(*,*)' &&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
	write(*,*)' Shifted xy coordinates '
	write(*,*)'         x1                y1                x2              y2'
	do i = 1,numSegs
		write(*,15)i,x1(i),y1(i),x2(i),y2(i)
		end do
	write(*,*)' minX, maxX = ',minX,maxX
	write(*,*)' minY, maxY = ',minY,maxY

	! Calculate the length of each segment
	do i = 1,numSegs
		segLength(i) = sqrt((x1(i)-x2(i))**2 + (y1(i)-y2(i))**2)
		end do
	

	! Now find all of the nodes
	numNodes = 0

	do i = 1,numSegs
		! First check to see if either end of this segment has already been added to a node
		! if it has, then skip
		! check the first point on the segment
		do k = 1,numNodes
			if(nodeX(k).eq.x1(i).and.nodeY(k).eq.y1(i))then
				go to 26	! one end of this segment has already been put in a node
				endif
			end do
		! We haven't used this one before -- see if it matches any other points
		do j = i+1,numSegs
			if(x1(i).eq.x1(j).and.y1(i).eq.y1(j))then
				istart = 1
				iend = 1
				Call MakeNode(i,istart,j,iend,x1(i),y1(i))
				go to 25
				endif
			if(x1(i).eq.x2(j).and.y1(i).eq.y2(j))then
				istart = 1
				iend = 2
				Call MakeNode(i,istart,j,iend,x1(i),y1(i))
				go to 25
				endif
25			continue
			end do
26		continue
		! now check the second point on the segment
		do k = 1,numNodes
			if(nodeX(k).eq.x2(i).and.nodeY(k).eq.y2(i))then
				go to 28	! one end of this segment has already been put in a node
				endif
			end do
		! We haven't used this one before -- see if it matches any other points
		do j = i+1,numSegs
			if(x2(i).eq.x1(j).and.y2(i).eq.y1(j))then
				istart = 2
				iend = 1
				Call MakeNode(i,istart,j,iend,x2(i),y2(i))
				go to 27
				endif
			if(x2(i).eq.x2(j).and.y2(i).eq.y2(j))then
				istart = 2
				iend = 2
				Call MakeNode(i,istart,j,iend,x2(i),y2(i))
				go to 27
				endif
27			continue
			end do
28		continue
		end do

	write(*,*)'        NodeX     NodeY        numNodeSegs       nodeSegConnect (nodeSegEnd)'
	do i = 1,numNodes
		write(*,16)i,nodeX(i),nodeY(i),numNodeSegs(i),((nodeSegConnect(i,j),nodeSegEnd(i,j)),j=1,numNodeSegs(i))
		end do
16	format(I5,2F12.3,I5,10(I5,' (',I1,')'))



	! Now we need to order the segments around a node in a counterclockwise direction
	! If there are only 2 segments, then it doesn't matter
	! For 3 or more segments we might need to change the ordering
	write(*,*)' '
	write(*,*)' &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
	write(*,*)' Ordering segments around a node '
	Do i = 1,numNodes
		!if(numNodeSegs(i).eq.2)cycle
	  	call OrderSegments(i,debug)
!		write(*,16)i,nodeX(i),nodeY(i),numNodeSegs(i),((nodeSegConnect(i,j),nodeSegEnd(i,j)),j=1,numNodeSegs(i))
		end do
	write(*,*)'Summary of ordered segments around a node '
	write(*,*)'   node       numNodeSegs       nodeSegConnect (nodeSegEnd)'
	do i = 1,numNodes
		write(*,19)i,numNodeSegs(i),((nodeSegConnect(i,j),nodeSegEnd(i,j)),j=1,numNodeSegs(i))
19		format(I5,I5,10(I5,' (',I1,')'))
		end do

	! figure out the other nodes that connect to each node
	! We do this after ordering segment so that the node order is the same as the segment order.
	write(*,*)' '
	write(*,*)' Ordered segments and nodes around a node'
	write(*,*)' node   numNodesegs   nodeSegConnect  (nodeNodeConnect) '
	do i = 1,numNodes
		do j = 1,numNodeSegs(i)
			do k = 1,numNodes
				if(k.eq.i)cycle		! skip this node
				do k1 = 1,numNodeSegs(k)
					if(nodeSegConnect(i,j).eq.nodeSegConnect(k,k1))then
						nodeNodeConnect(i,j) = k
						exit	! done with this node
						endif
					end do
				end do
			end do
		write(*,18)i,numNodeSegs(i),(nodeSegConnect(i,j),nodeNodeConnect(i,j),j=1,numNodeSegs(i))
18		format(2I5,'...',10(I5,' (node',I2,')'))
		end do
		

	! Figure out the nodes at the ends of each segment
	write(*,*)' '
	write(*,*)' &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
	write(*,*)' Nodes at ends of segments and segment xy '
	write(*,*)' Seg   SegNode1   segX    segY      SegNode2     segX     segY '
	do j = 1,numSegs
		didone = 0
		do i = 1,numNodes
			if(x1(j).eq.nodeX(i).and.y1(j).eq.nodeY(i))then
				segNodes(j,1) = i
				segX(j,1) = nodeX(i)
				segY(j,1) = nodeY(i)
				didone = 1
				!go to 30
				endif
			if(x2(j).eq.nodeX(i).and.y2(j).eq.nodeY(i))then
				segNodes(j,2) = i
				segX(j,2) = nodeX(i)
				segY(j,2) = nodeY(i)
				didone = 1
				!go to 30
				endif
			end do
		if(didone.eq.0)then
			write(*,*)'Segment = ',j
			pause 'Could not find nodes for the above segment'
			endif
!30		continue
		write(*,32)j,segNodes(j,1),segX(j,1),segY(j,1),segNodes(j,2),segX(j,2),segY(j,2)
32		format(I5,2(I5,2F12.3))
		end do		
			

	pause ' Line 296 How does it look??'

!	Do i = 1,numNodes
!		write(*,*)'Polygon on node = ',i
!		write(*,16)i,nodeX(i),nodeY(i),numNodeSegs(i),((nodeSegConnect(i,j),nodeSegEnd(i,j)),j=1,numNodeSegs(i))
	  	call MakePolygons()
!		end do


	pause ' Line 305   How does it look??'

!	Plot the grid
300	continue
	write(*,*)' Options'
	write(*,*)' 0 = quit'
	write(*,*)' 1 = set up grid plot (open a new graphics window)'
	write(*,*)' 2 = FillPolys'
	write(*,*)' 3 = Plot grid (fill polys before plotting grid)'
	write(*,*)' 4 = label nodes'
	write(*,*)' 5 = label segs'
	write(*,*)' 6 = label polygons'
	write(*,*)' 7 = Save grid as a model input file'
	write(*,*)' 8 = Calculate poly center for a specific polygon (for debugging)'
	read(*,*)i
	select case(i)
		case(0)
			stop
		case(1)
			call SetUpGridPlot(GridPlot,maxX,maxY)
		case(2)
			call FillPolys(GridPlot)
		case(3)
			call PlotGrid(GridPlot)
		case(4)
			call LabelNodes(GridPlot)
		case(5)
			call LabelSegs(GridPlot)
		case(6)
			call LabelPolys(GridPlot)
		case(7)
			!write(*,*)' ###########################'
			!write(*,*)' Save as a model input file? 0 = NO, 1 = YES'
			!read(*,*)isave
			!if(isave.eq.1)then
	
			open(17,file='',status = 'NEW',iostat = status)
			if(status.ne.0)stop
			INQUIRE (17, NAME=SaveFile)
			write(17,*)'Input SVG file ',GridFile
			write(17,*)'--------------------------------'
			write(17,*)' Segments '
			write(17,*)numSegs,'      = numSegs'
			write(17,*)'         x1                y1                x2              y2       length'
			do i = 1,numSegs
				write(17,115)i,x1(i),y1(i),x2(i),y2(i),segLength(i)
	115			format(I5,5F15.8)
				end do
			write(17,*)minX,maxX,' = minX, maxX '
			write(17,*)minY,maxY,' = minY, maxY '
			write(17,*)'--------------------------------'
			write(17,*)' Nodes '
			write(17,*)numNodes,'      = numNodes'
	!		write(17,*)'        NodeX     NodeY        num       nodeSegConnect nodeSegEnd'
	!		do i = 1,numNodes
	!			write(17,116)i,nodeX(i),nodeY(i),numNodeSegs(i),((nodeSegConnect(i,j),nodeSegEnd(i,j)),j=1,numNodeSegs(i))
	!116			format(I5,2F12.3,I5,5x,10(I5,1x,I1,5x))
	!			end do
	!		write(17,*)'--------------------------------'
	!		write(17,*)'Ordered nodes, segments and endpoints around a node '
			write(17,*)'node      NodeX       NodeY   numNodeSegs     nodeNodeConnect   nodeSegConnect nodeSegEnd'
			do i = 1,numNodes
				write(17,119)i,nodeX(i),nodeY(i),numNodeSegs(i),((nodeNodeConnect(i,j),nodeSegConnect(i,j),nodeSegEnd(i,j)),j=1,numNodeSegs(i))
	119			format(I5,2F12.3,I5,5x,10(I5,I5,I2,5x))
				end do
			write(17,*)'--------------------------------'
			write(17,*)' Nodes at ends of segments and segment xy '
			write(17,*)' Seg   SegNode1   segX    segY      SegNode2     segX     segY '
			do j = 1,numSegs
				write(17,32)j,segNodes(j,1),segX(j,1),segY(j,1),segNodes(j,2),segX(j,2),segY(j,2)
				end do
			write(17,*)'--------------------------------'
			write(17,*)' Polygon list'
			write(17,*)numPolys, ' = numPolys '
			write(17,*)' Poly   num    PolyNodes PolySegs'
			do i = 1,numPolys
				write(17,120)i,numPolyNodes(i),(PolyNodes(i,k),PolySegs(i,k),k=1,numPolyNodes(i))
				end do
	120			Format(2I5,20(2I5,5x))	

			write(17,*)'--------------------------------'
			write(17,*)' Center of Polys '
			write(17,*)' Poly    Xcenter    Ycenter'
			do i = 1,numPolys
				write(17,121)i,PolyCenterX(i),PolyCenterY(i)
	121			format(I5,2f12.3)
				end do
			write(17,*)'--------------------------------'

			close(17)
			!endif

		case(8)
80			continue
			write(*,*)' Input poly to examine (0 to exit)'
			read(*,*)ipoly
			if(ipoly.eq.0)go to 300
			call PolyCentroid(ipoly,xCenter,yCenter)
			write(*,*)xCenter,yCenter
			go to 80
	
		case default
		end select
	
	go to 300
	!pause ' Happy now????'
!	go to 1
	
	! find a polygon
!	iNode = 1
!	do j = 2,numNodeSegs(iNode)
!		end do



99	continue
	write(*,*)' %%%%%%%%%%%%%%%%%%%%%%%%%%'
	write(*,*)' Error reading SVG file '
	pause 'Hit return to exit'
	end

! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine OrderSegments(iNode,debug)
	! Routine to order the segments in a counter clockwise way around the initial segment
	! This will facilitate
		!(a) Finding crystal polygons (using the right turn convention)
		!(b) Determining how to move nodes after reactions
	! The algorithm is this:
		! Start with segment 1 (whatever it is from the initial gridding)
		! Transform all segments so the node point for segment 1 is the origin
			! This is just xn - x1 and yn - y1
		! Scale all vectors to unit length
			! x' = x/length, y' = y/length where length = sqrt(x**2 + y**2)
		! Start with segment 1 and order the segments thusly:
			! If y > 0 then order with decreasing x starting with x1 -- do these first
			! If y = 0 (unusual case) add this one in
			! If y < 0 then order with increasing x
			! if y = 0 again (this will be the positive x axis) add this one in
			! If more segments exist then continue with decreasing x until done
			! this should work if y1 (the initial segment) > 0. If not, then start at step 3
			! check out the spreadsheet "NodeCircle.xlsx"
	implicit none
	include "Grid_Stuff.inc"
	include "SVG_Stuff.inc"
	integer*4 iNode,j,k,jj,didone,jNext(10),iSeg,iend,debug
	real*8 xStart,yStart,x1T(10),y1T(10),x2T(10),y2T(10),xPt(10),yPt(10),xNext,length
	integer*4 nodeSegConnectTemp(10),nodeSegEndTemp(10)
	
	if(debug.eq.1)then
		write(*,*)'--------------------'
		write(*,*)'iNode = ',iNode
		write(*,16)iNode,nodeX(iNode),nodeY(iNode),numNodeSegs(iNode),		&
			((nodeSegConnect(iNode,j),nodeSegEnd(iNode,j)),j=1,numNodeSegs(iNode))
16		format('Initial ',I5,2F12.3,I5,10(I5,' (',I1,')'))
		endif
	! Transform all segments so the node point for segment 1 is the origin
	xStart = nodeX(iNode)
	yStart = nodeY(iNode)
	if(debug.eq.1)then
		write(*,*)' '
		write(*,*)' Transformed vectors'
		endif
	do j = 1,numNodeSegs(iNode)
		iSeg = nodeSegConnect(iNode,j)
		x1T(j) = x1(iSeg) - xStart
		y1T(j) = y1(iSeg) - yStart
		x2T(j) = x2(iSeg) - xStart
		y2T(j) = y2(iSeg) - yStart
		if(debug.eq.1)then
			write(*,101)iSeg,nodeSegEnd(iNode,j),x1T(j),y1T(j),x2T(j),y2T(j)
101			format(2I5,4F12.3)
			endif
		end do

	! All segments should now have one point on the origin
	! The point on the origin is given by the variable nodeSegEnd(iNode,j)  = 1 if it is the first point (x1,y1) = 2 if (x2,y2)
	! Scale all vectors to unit length
		! we only need to scale the point that is not sitting on the node		

	if(debug.eq.1)then
		write(*,*)' '
		write(*,*)' Unit vectors'
		endif
	do j = 1,numNodeSegs(iNode)
		iSeg = nodeSegConnect(iNode,j)
		iEnd = nodeSegEnd(iNode,j)
		if(iEnd.eq.1)then	! scale point 2
			length = sqrt(x2T(j)**2 + y2T(j)**2)
			xPt(j) = x2T(j)/length
			yPt(j) = y2T(j)/length
			else		! scale point 1
			length = sqrt(x1T(j)**2 + y1T(j)**2)
			xPt(j) = x1T(j)/length
			yPt(j) = y1T(j)/length
			endif			
			if(debug.eq.1)write(*,101)iSeg,nodeSegEnd(iNode,j),xPt(j),yPt(j)
		end do

	! Start the ordering
	xStart = xPt(1)
	yStart = yPt(1)
	jNext(1) = 1
	k = 2
	if(ystart.ge.0.0d0)then
		! do the decreasing x when y is positive
30		continue
		xNext = -2.		! just to be sure that it is way to the left of the origin
		didone = 0
		do j = 2,numNodeSegs(iNode)
			! check to see we haven't done this point yet
			do jj = 2,k-1
				if(jNext(jj).eq.j)then
					go to 31
					endif
				end do
!			if(yPt(j).ge.0.0d0)then		! we order the x values differently if Y is positive vs negative
			if(yPt(j).gt.0.0d0)then		! we order the x values differently if Y is positive vs negative
				if(xPt(j).le.xStart.and.xPt(j).ge.xNext)then
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					endif
				endif
31			continue
			end do
		if(didone.eq.1)then
			k = k + 1
			go to 30
			endif
		! if we got here then we've found all points with positive Y
		! do the special case if y = 0
		do j = 2,numNodeSegs(iNode)
			if(yPt(j).eq.0.0d0)then		! special case. Assume there can only be 1 point with Y = 0 
				if(xPt(j).le.xStart)then	! just to be sure this is not the positive x axis
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					k = k + 1			
					endif
				endif
			end do
	
		! now do the increasing x when y is negative
35		continue
		xNext = 2.		! just to be sure that it is way to the right of the origin
		didone = 0
		do j = 2,numNodeSegs(iNode)
			do jj = 2,k-1
				if(jNext(jj).eq.j)then
					go to 36
					endif
				end do
!			if(yPt(j).le.0.0d0)then		! we order the x values differently if Y is positive vs negative
			if(yPt(j).lt.0.0d0)then		! we order the x values differently if Y is positive vs negative
				if(xPt(j).le.xNext)then
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					endif
				endif
36			continue
			end do
		if(didone.eq.1)then
			k = k + 1
			go to 35
			endif

		! do the special case if y = 0 on the positive x axis
		do j = 2,numNodeSegs(iNode)
			if(yPt(j).eq.0.0d0)then		! special case. Assume there can only be 1 point with Y = 0 
				if(xPt(j).gt.xStart)then	! just to be sure this is not the positive x axis
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					k = k + 1			
					endif
				endif
			end do

		! do the decreasing x when y is positive but x > xStart
38		continue
		xNext = -2.		! just to be sure that it is way to the left of the origin
		didone = 0
		do j = 2,numNodeSegs(iNode)
			do jj = 2,k-1
				if(jNext(jj).eq.j)then
					go to 39
					endif
				end do
!			if(yPt(j).ge.0.0d0)then		! we order the x values differently if Y is positive vs negative
			if(yPt(j).gt.0.0d0)then		! we order the x values differently if Y is positive vs negative
				if(xPt(j).gt.xStart.and.xPt(j).ge.xNext)then
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					endif
				endif
39			continue
			end do
		if(didone.eq.1)then
			k = k + 1
			go to 38
			endif

		! XXXXX - ELSE -- XXXX
		else	! code when Y1 < 0 


		! now do the increasing x when y is negative
45		continue
		xNext = 2.		! just to be sure that it is way to the right of the origin
		didone = 0
		do j = 2,numNodeSegs(iNode)
			do jj = 2,k-1
				if(jNext(jj).eq.j)then	! skip if we did this one already
					go to 46
					endif
				end do
			if(yPt(j).lt.0.0d0)then		! we order the x values differently if Y is positive vs negative
				if(xPt(j).ge.xStart.and.xPt(j).le.xNext)then
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					endif
				endif
46			continue
			end do
		if(didone.eq.1)then
			k = k + 1
			go to 45
			endif

		! do the special case if y = 0 on the positive x axis
		do j = 2,numNodeSegs(iNode)
			if(yPt(j).eq.0.0d0)then		! special case. Assume there can only be 1 point with Y = 0 
				if(xPt(j).gt.xStart)then	! just to be sure this is not the positive x axis
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					k = k + 1			
					endif
				endif
			end do
		! do the decreasing x when y is positive
40		continue
		xNext = -2.		! just to be sure that it is way to the left of the origin
		didone = 0
		do j = 2,numNodeSegs(iNode)
			! check to see we haven't done this point yet
			do jj = 2,k-1
				if(jNext(jj).eq.j)then
					go to 41
					endif
				end do
			if(yPt(j).gt.0.0d0)then		! we order the x values differently if Y is positive vs negative
				if(xPt(j).ge.xNext)then
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					endif
				endif
41			continue
			end do
		if(didone.eq.1)then
			k = k + 1
			go to 40
			endif
		! if we got here then we've found all points with positive Y
		! do the special case if y = 0
		do j = 2,numNodeSegs(iNode)
			if(yPt(j).eq.0.0d0)then		! special case. Assume there can only be 1 point with Y = 0 
				if(xPt(j).le.xStart)then	! just to be sure this is not the positive x axis
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					k = k + 1			
					endif
				endif
			end do
		! do the increasing x when y is negative but x < xStart
48		continue
		xNext = 2.		! just to be sure that it is way to the left of the origin
		didone = 0
		do j = 2,numNodeSegs(iNode)
			do jj = 2,k-1
				if(jNext(jj).eq.j)then
					go to 49
					endif
				end do
!			if(yPt(j).le.0.0d0)then		! we order the x values differently if Y is positive vs negative
			if(yPt(j).lt.0.0d0)then		! we order the x values differently if Y is positive vs negative
				if(xPt(j).lt.xStart.and.xPt(j).le.xNext)then
					xNext = xPt(j)
					jNext(k) = j
					didone = 1
					endif
				endif
49			continue
			end do
		if(didone.eq.1)then
			k = k + 1
			go to 48
			endif



		endif		! end if Ystart > 0


	! If we got here then we should have ordered all of the segments into the array jNext
	if(debug.eq.1)then
		write(*,102)(jNext(j),j = 1,numNodeSegs(iNode))
		endif
102	format(10I5)
	! now rearrange the order of segments according to jNext array
	
	do j = 2,numNodeSegs(iNode)
		nodeSegConnectTemp(j) = nodeSegConnect(iNode,jNext(j))
		nodeSegEndTemp(j) = nodeSegEnd(iNode,jNext(j))
		end do
	do j = 2,numNodeSegs(iNode)
		nodeSegConnect(iNode,j) = nodeSegConnectTemp(j)
		nodeSegEnd(iNode,j) = nodeSegEndTemp(j)
		end do
	if(debug.eq.1)then
		write(*,17)iNode,nodeX(iNode),nodeY(iNode),numNodeSegs(iNode),		&
			((nodeSegConnect(iNode,j),nodeSegEnd(iNode,j)),j=1,numNodeSegs(iNode))
		endif
17	format('Ordered ',I5,2F12.3,I5,10(I5,' (',I1,')'))
		
	!pause 'In sub OrderSegments How does it look?'

		

	return
	end



! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine MakePolygons(iNode)
	! routine to define a polygon (crystal)
	! The algorithm is to start at a node and take a segment to the next node
	! At each new node, make a right turn
		! The right turn is always the next segment in the segment list for the node (output from OrderSegments)
	! This will end up at the beginning and the polygon is complete.
	implicit none
	include "Grid_Stuff.inc"
	include "SVG_Stuff.inc"
	integer*4 iNode,i,j,k,jj,jjj,iSeg,np,ki,kj,kiT,kjT
	integer*4 PolyNode(400,100,200),numNodesPoly(400,200),PolySeg(400,100,200),dup(maxPolys)
!	Integer*4 PolyNodes(500,10),PolySegs(500,10),numPolyNodes(500),numPolys,dup(500)	! these are nodes and segments for each polygon
	real*8 xCenter,yCenter
	
 	numPolys = 1
	write(*,*)' &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
	write(*,*)' Making polygons '
	do iNode = 1,numNodes
	
		write(*,*)' iNode = ',iNode	
		write(*,*)'iNode  Seg  NumNodes  Node  (iSeg)'
		do j = 1,numNodeSegs(iNode)			! find the polygon for every segment around a node
			k = 1					! k counts the number of nodes in a polygon
			numNodesPoly(iNode,j) = k
			PolyNode(iNode,j,1) = iNode			! this is the first node in the polygon
			iSeg = nodeSegConnect(iNode,j)
			PolySeg(iNode,j,1) = iSeg	! the index of the next segment
			write(*,*)' iSeg = ',iSeg
	10		continue
			do i = 1,numNodes	! find the node this segment connects to
				do jj = 1,numNodesPoly(iNode,j)			
					if(i.eq.PolyNode(iNode,j,jj))go to 12		! skip the node if it's already part of the polygon
					end do
				do jj = 1,numNodeSegs(i)
					if(iSeg.eq.nodeSegConnect(i,jj))then
						! This segment is common to both nodes
						k = k + 1
						if(k.gt.200)then
							call FSS_Alert('Alert','numPolyNodes > 200. Redimension arrays)')
							stop
							endif
						PolyNode(iNode,j,k) = i	! the index of the next node
						numNodesPoly(iNode,j) = k
						jjj = jj + 1
						if(jjj.gt.numNodeSegs(i))jjj = 1	! cycle the segs on this new node
						iSeg = nodeSegConnect(i,jjj)
						PolySeg(iNode,j,k) = iSeg	! the index of the next node
						write(*,*)' iSeg = ',iSeg
						go to 10
						endif				
					end do
	12			continue
				end do

			write(*,101)iNode,j,numNodesPoly(iNode,j),(PolyNode(iNode,j,k),PolySeg(iNode,j,k),k=1,numNodesPoly(iNode,j))
	101		format(3I5,20(I5,1x,'(',I3,')'))	

			numPolyNodes(numPolys) = numNodesPoly(iNode,j)
			do k = 1,numNodesPoly(iNode,j)
				PolyNodes(numPolys,k) = PolyNode(iNode,j,k)
				PolySegs(numPolys,k) = PolySeg(iNode,j,k)
				end do
			numPolys = numPolys + 1

14			continue
			end do	
		end do		! end loop for all nodes


	numPolys = numPolys - 1

	write(*,*)' numPolys = ',numPolys
	do i = 1,numPolys
		write(*,102)i,numPolyNodes(i),(PolyNodes(i,k),PolySegs(i,k),k=1,numPolyNodes(i))
		end do
102		Format(2I5,20(I5,1x,'(',I3,')'))	

	! now check for redundancies
	do j = 1,numPolys
		dup(j) = 0		! this is flag for duplicate polys
		end do
	do i = 1,numPolys
		if(dup(i).eq.1)go to 32		! skip duplicates
		do ki = 1,numPolyNodes(i)	! loop on the poly we are checking
			do j = i+1,numPolys	! loop on the rest of the polys
				if(dup(j).eq.1)go to 30
				if (numPolyNodes(i).ne.numPolyNodes(j))go to 30		! only check if there are the same number of nodes
				do kj = 1,numPolyNodes(j)	! loop on the poly below the one
					if(PolyNodes(i,ki).eq.PolyNodes(j,kj))then
						! at least 1 node matches. Check the next pair
						kiT = ki + 1
						if(kiT.gt.numPolyNodes(i))kiT = 1		! cycle back to 1
						kjT = kj + 1
						if(kjT.gt.numPolyNodes(j))kjT = 1		! cycle back to 1
						if(PolyNodes(i,kiT).eq.PolyNodes(j,kjT))then
							! if 2 segments match these must be the same poly
							dup(j) = 1
							go to 30	! go and check the next poly
							endif
						endif
					end do
					
	30			continue
				end do		
			end do
			! if we get to here, then we have flagged duplicates for poly(i)
32		continue
		end do
	! Now we have flagged all duplicates
	! Parse the array
	np = 1	
	do i = 1,numPolys
		if(dup(i).eq.1)go to 34
		numPolyNodes(np) = numPolyNodes(i)
		do k = 1,numPolyNodes(i)
			PolyNodes(np,k) = PolyNodes(i,k)	
			PolySegs(np,k) = PolySegs(i,k)
			end do
		np = np + 1
34		continue
		end do
	numPolys = np - 1

	write(*,*)' Parsed polygons list '
	write(*,*)' numPolys = ',numPolys
	do i = 1,numPolys
		write(*,102)i,numPolyNodes(i),(PolyNodes(i,k),PolySegs(i,k),k=1,numPolyNodes(i))
		end do



	! One polygon always goes around the entire perimeter -- this will be the poly with the largest number of nodes
	! Find the poly with the greatest number of nodes and remove
	np = 1
	do i = 1,numPolys
		if(numPolyNodes(i).gt.np)then
			np = numPolyNodes(i)
			k = i
			endif
		end do
	! k is the poly to remove
	! now remove poly k
	j = 0
	do i = 1,numPolys
		if(i.ne.k)then
			j = j + 1
			numPolyNodes(j) = numPolyNodes(i)
			do jj = 1,numPolyNodes(j)
				PolyNodes(j,jj) = PolyNodes(i,jj)
				PolySegs(j,jj)  = PolySegs(i,jj)
				end do
			endif
		end do
	numPolys = numPolys - 1					
		

	write(*,*)' Parsed polygons list with perimeter removed'
	write(*,*)' numPolys = ',numPolys
	do i = 1,numPolys
		write(*,102)i,numPolyNodes(i),(PolyNodes(i,k),PolySegs(i,k),k=1,numPolyNodes(i))
		end do
!102		Format(2I5,20(I5,1x,'(',I3,')'))	

	write(*,*)' '
	write(*,*)' Center of Polys '
	do i = 1,numPolys
		call PolyCentroid(i,xCenter,yCenter)
		PolyCenterX(i) = xCenter
		PolyCenterY(i) = yCenter
		write(*,*)i,xCenter,yCenter
		end do

	pause 'In Sub MakePolygons  List of polys (parsed) Take a look...'	

		
	return
	end

! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine FindAngle(i,iconnect,j,jconnect)
	!routine to find the angle between two vectors
	implicit none
	include "Grid_Stuff.inc"
	include "SVG_Stuff.inc"
	integer*4 i,iconnect,j,jconnect
	real*8 ax,ay,bx,by,length,numerator,arg,angle

	! Each vector is a:(x1,y1),(x2,y2)
	! Each vector is b:(x1,y1),(x2,y2)
	! so we first have to do the subtraction
	! One point in each vector is in common, but it might be the first or the second point
	if(iconnect.eq.1)then
		ax = x1(i) - x2(i)
		ay = y1(i) - y2(i)
		else
		ax = x2(i) - x1(i)
		ay = y2(i) - y1(i)
		endif
	if(jconnect.eq.1)then
		bx = x1(j) - x2(j)
		by = y1(j) - y2(j)
		else
		bx = x2(j) - x1(j)
		by = y2(j) - y1(j)
		endif

	length = sqrt((ax**2 + ay**2)*(bx**2 + by**2))
	numerator = ax*bx + ay*by
	arg = numerator/length
	angle = acos(arg)		! angle in radians
	write(*,*)i,iconnect,j,jconnect
	write(*,*)length,numerator,arg,angle
	return
	end


! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine MakeNode(i,startorendi,j,startorendj,x1T,y1T)
	implicit none
	include "Grid_Stuff.inc"

	integer*4 i,k,startorendi,j,startorendj
	real*8 x1T, y1T
	! the points are the same
	! is this a new node or is this one already found?
!	write(*,*)x1T,y1T
	do k = 1,numNodes
		if(nodeX(k).eq.x1T.and.nodeY(k).eq.y1T)then
			numNodeSegs(k) = numNodeSegs(k) + 1
			nodeSegConnect(k,numNodeSegs(k)) = j
			nodeSegEnd(k,numNodeSegs(k)) = startorendj			! this says we match with the start of the segment
			if(numNodeSegs(k).gt.3)then
				call FSS_Alert('Alert','Number of node segments > 3. See output for details')
				write(*,*)'Node with segments > 3 = ',k
				pause 'hit return to continue'
				endif
			return
!			yesitsnew = 0		!this one is not new
!			go to 10
			endif
		end do
!	yesitsnew = 1		! if we get here, we checked all previous nodes and this one is new
!10	continue
!	if(yesitsnew.eq.1)then
	! A new node
	numNodes = numNodes + 1	
	nodeX(numNodes) = x1T
	nodeY(numNodes) = y1T
	numNodeSegs(numNodes) = 2
	nodeSegConnect(numNodes,1) = i		
	nodeSegConnect(numNodes,2) = j		
	nodeSegEnd(numNodes,1) = startorendi		! this says we match with the start of the segment
	nodeSegEnd(numNodes,2) = startorendj		! this says we match with the start of the segment
!		write(*,*)numNodes,nodeX(numNodes),nodeY(numNodes)
!		else
		! this adds on to node k
!		numNodeSegs(k) = numNodeSegs(k) + 1
!		nodeSegConnect(k,numNodeSegs(k)) = j
!		nodeSegEnd(k,numNodeSegs(k)) = startorendj			! this says we match with the start of the segment
!		endif
	return
	end
		
	
	
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine Findquote(aline,icol,err)
	implicit none
	
	character*256 aline
	character*1 achar,quote
	integer*4 icol,err
	
	err = 0
	quote = '"'	
10	continue
	if(icol.ge.256)then
		call FSS_Alert('Alert','Quote mark not found')
		err = 1
		return
		endif
	achar = aline(icol:icol)
	if(achar.eq.quote)then			! we found the end		
		err = 0
		return
		else
		icol = icol + 1
		go to 10
		endif
	
	end
	
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	subroutine FSS_alert(title, text)
    	use AWE_Interfaces
	implicit none
!	interface Alert
!	subroutine AWE_alertBox(title, text)
	character(len=*) :: title, text
	call AWE_alertBox(title,text)
	end
!	end subroutine AWE_alertBox
!	end interface
!	title is used as the title of the alert box text is the text that will be displayed in it.
	
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$	
	Subroutine SetUpGridPlot(GridPlot,maxX,maxY)
	USE AWE_INTERFACES
      	implicit none
	TYPE(AWE_Canvas) :: GridPlot
	Type(AWE_CanvasBrush) :: brush
	TYPE(AWE_CanvasPen) :: pen
!	include "Diffuse_GB_Hex.inc"
	INCLUDE "PlotStuff.inc"				
!	real*4 asprat
	real*8 maxX,maxY
	CurrentColor = 1		! black is the default
	if(maxY.gt.maxX)then
		xmax = maxY
		ymax = maxY
		else
		xmax = maxX
		ymax = maxX
		endif
	xmin = 0.
	ymin = 0.

	xlen = 20		! values are in cm
	ylen = 20
	write(*,*)' Input value to use as xlength=ylength (i.e. 20). Values are in cm'
	read(*,*)xlen
	ylen=xlen
      	CALL USER()			! sets the user coordinates that were input in Sub SetPlot
!	GridPlot%width  = 1.1*xmax*xconv			! *xScale scales the X dimension
!	GridPlot%height = 1.1*ymax*yconv
	GridPlot%width  = 1.2*maxX*xconv			! *xScale scales the X dimension
	GridPlot%height = 1.1*maxY*yconv
	xor = 20.				! origin for X-Y plot (xmin,ymin) in pixels
	yor = GridPlot%height - 20
	CALL AWE_createCanvas(GridPlot)	

	return
	end
	
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$	
	Subroutine PlotGrid(GridPlot)
	USE AWE_INTERFACES
      	implicit none
	TYPE(AWE_Canvas) :: GridPlot
	Type(AWE_CanvasBrush) :: brush
	TYPE(AWE_CanvasPen) :: pen
	include "Grid_Stuff.inc"
!	include "Diffuse_GB_Hex.inc"
	INCLUDE "PlotStuff.inc"				
	real*4 xnode,ynode,x,y,dX,dY
	integer*4 i,j,index,iup
	CurrentColor = 1		! black is the default
	pen%penStyle = CanvasPenStyle_SolidLine
	Do index = 1,numNodes
		x = nodeX(index)
		y = nodeY(index)
		dx = .005*(xmax-xmin)
		dy = dx
		!Call PlotCenteredEllipseOnScreen(GridPlot,X,dX,Y,dY,brush,pen)
	!	write(*,*)' '
		do j = 1, numNodeSegs(index)
		!	write(*,*)index,j
			iup = 0
			xnode = nodeX(index) 		
			ynode = nodeY(index)
			call plot(GridPlot,xnode,ynode,iup)
			iup = 1	
!			i = iConnect(index,j)
			i = nodeNodeConnect(index,j)
!			if(i.lt.index)go to 12		! skip if connection is to a node with a lower index
			if(i.lt.index)cycle		! skip if connection is to a node with a lower index
			x = nodeX(i)
			y = nodeY(i)
			call plot(GridPlot,x,y,iup)
			end do
		end do	
!	12	continue
!10	continue

	return
	end
	
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine FillPolys(canvas)
!	routine to draw all of the model crystals on the grid
	USE AWE_INTERFACES
	implicit none
	TYPE(AWE_Canvas) :: canvas
	Type(AWE_CanvasBrush) :: brush
	TYPE(AWE_CanvasPen) :: pen
	include "Grid_Stuff.inc"
	real*4 pointArray(12,2)
	integer*4 i,j,iNode,numPts

!123	Chlorite
!green                           32768    0  128    0   1.000   0.000   1.000   0.498
!1
!94,95,107,119,118,106

	Do i = 1,numPolys
		do j = 1,numPolyNodes(i)
			iNode = PolyNodes(i,j)
			pointArray(j,1) = nodeX(iNode)
			pointArray(j,2) = nodeY(iNode)
			end do
			
		numPts = numPolyNodes(i)
		pen%penStyle = CanvasPenStyle_SolidLine
! 		pen%penColor     = AWE_olive
! 		brush%brushColor = AWE_olive
		pen%penColor     = AWE_pink
		brush%brushColor = AWE_pink
! 		pen%penColor = PhaseColorNumber(i)
! 		brush%brushColor = PhaseColorNumber(i)
		call DrawPolygonOnScreen(canvas,numPts,pointArray,brush,pen)
		end do
	return
	end

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine DrawPolygonOnScreen(canvas,numPts,pointArray,brush,pen)
	USE AWE_INTERFACES
	implicit none
	TYPE(AWE_Canvas) :: canvas
	Type(AWE_CanvasBrush) :: brush
	TYPE(AWE_CanvasPen) :: pen
!	TYPE(AWE_Point) :: poly_points(:)
	TYPE(AWE_Point) :: poly_points(numPts)

	real xpix,ypix			! functions to convert X,Y to pixels
	real*4 pointArray(12,2)
	integer*4 numPts,i
	!	write(*,*)T,dT,P,dP

	do i = 1,numPts
		poly_points(i)%x = xpix(pointArray(i,1))
		poly_points(i)%y = ypix(pointArray(i,2))
		end do

	call AWE_canvasDrawPolygon(canvas,poly_points,pen,brush)	
	return
	end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	Subroutine LabelPolys(canvas)
!	routine to draw all of the model crystals on the grid
	USE AWE_INTERFACES
	implicit none
	TYPE(AWE_Canvas) :: canvas
	Type(AWE_CanvasBrush) :: brush
	TYPE(AWE_CanvasPen) :: pen
	include "Grid_Stuff.inc"
	integer*4 i,textsize
	character*5 text
	real*4 x,y

	textsize = 12
	pen%penStyle = CanvasPenStyle_SolidLine
	Do i = 1,numPolys
		x = PolyCenterX(i)
		y = PolyCenterY(i)
		write(text,101)i
101		format(I5)
		text = 'p'//adjustL(text)
		CALL TextOnPlot(Canvas,x,y,text,textSize)
		end do
	return
	end

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$	
	Subroutine LabelNodes(GridPlot)
	USE AWE_INTERFACES
      	implicit none
	TYPE(AWE_Canvas) :: GridPlot
	Type(AWE_CanvasBrush) :: brush
	TYPE(AWE_CanvasPen) :: pen
	include "Grid_Stuff.inc"
	INCLUDE "PlotStuff.inc"				
	character*5 text
	real*4 x,y
	integer*4 index,textsize
	CurrentColor = 1		! black is the default

	textsize = 12
	pen%penStyle = CanvasPenStyle_SolidLine
	Do index = 1,numNodes
		x = nodeX(index) + .01*(xmax-xmin)
		y = nodeY(index) + .01*(ymax-ymin)
		write(text,101)index
101		format(I5)
		text = 'n'//adjustL(text)
		CALL TextOnPlot(GridPlot,x,y,text,textSize)
	!	SUBROUTINE TextOnPlot(GridPlot,xplot,yplot,text,textSize)
		end do
	return
	end
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$	
	Subroutine LabelSegs(GridPlot)
	USE AWE_INTERFACES
      	implicit none
	TYPE(AWE_Canvas) :: GridPlot
	Type(AWE_CanvasBrush) :: brush
	TYPE(AWE_CanvasPen) :: pen
	include "Grid_Stuff.inc"
	INCLUDE "PlotStuff.inc"				
	character*5 text
	real*4 x,y
	integer*4 j,textsize
	CurrentColor = 1		! black is the default

	textsize = 12
	pen%penStyle = CanvasPenStyle_SolidLine
	Do j = 1,numSegs
		!	ipoint = numPoints(j)/2 + 1
		!	x = pointX(j,ipoint) + .01*(xmax-xmin)
		x = (segX(j,1) + segX(j,2))/2.
		y = (segY(j,1) + segY(j,2))/2.
		!	x = pointX(j,ipoint) 
		!	y = pointY(j,ipoint) + .01*(ymax-ymin)
		write(text,101)j
		text = 's'//adjustL(text)
	101	format(I5)
		CALL TextOnPlot(GridPlot,x,y,text,textSize)
		!	SUBROUTINE TextOnPlot(GridPlot,xplot,yplot,text,textSize)
		end do
	return
	end
