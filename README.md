# GridMaker_repo
Program to convert an SVG file into a 2-D grid with nodes and connecting segments for use in program GibbsMDF


This readme file will provide the basic program operation.
(1) Create an SVG file for your 2-D grid using only line segments. The key is to be sure that the end points of each line segment are at the same xy coordinates. I've done this in Adobe Illustrator. A "trick" is to set the "snap to point" in the preferences to a relatively large value -- I use 10. Then, when I get close to a point with which to connect, Illustrator snaps there. Note that only SVG commands starting with "<line" are recognized, so you can add other elements such as text and these will be ignored by program GridMaker.
(2) Modify the SVG file slightly as shown in the example SVG file using a text editor. Just add a title and a Y offset value (575 in this example) to the top 2 lines. Save with a .txt extension (not .svg)

example
Model_1
576
<line fill="none" stroke="#000000" stroke-miterlimit="10" x1="123.983" y1="159.966" x2="229.915" y2="159.966"/>
<line fill="none" stroke="#000000" stroke-miterlimit="10" x1="292.334" y1="219" x2="263.5" y2="198.5"/>
<line fill="none" stroke="#000000" stroke-miterlimit="10" x1="305" y1="174" x2="286.333" y2="159.667"/>
etc.

(3) Run program GridMaker and open the modified SVG file file. Click through the queries until you get to this menu:
 Options
  0 = quit
  1 = set up grid plot (open a new graphics window)
  2 = FillPolys
  3 = Plot grid (fill polys before plotting grid)
  4 = label nodes
  5 = label segs
  6 = label polygons
  7 = Save grid as a model input file
  8 = Calculate poly center for a specific polygon (for debugging)

(4) Choose 1 to see if the program gives what you expected. If not, fix the SVG file and repeat.
(5) You can choose to label nodes, segments and polygons, if desired. I recommend all three, but "label polygons" is a necessity because the input to program GibbsMDF requires you specify which polygons correspond to which mineral phases, so you need these numbers.
(6) When you're happy with the results, choose option 7 and save the grid for input into program GibbsMDF
