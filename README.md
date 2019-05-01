# path-finder-116

Problems that require minimum paths through some domain appear in many different areas of computerscience. For example, one of the constraints in VLSI routing problems is minimizing wire length. TheTraveling Salesperson Problem (TSP) — finding whether all the cities in a salesperson’s route can bevisited exactly once with a specified limit on travel time — is one of the canonical examples of anNP-complete problem; solutions appear to require an inordinate amount of time to generate, but aresimple to check.This problem deals with finding a minimal path through a grid of points while traveling only fromleft to right.Given anmnmatrix of integers, you are to write a program that computes a pathof minimal weight. A path starts anywhere in column 1 (the first column) and consistsof a sequence of steps terminating in columnn(the last column). A step consists oftraveling from columnito columni+ 1in an adjacent (horizontal or diagonal) row.The first and last rows (rows1andm) of a matrix are considered adjacent, i.e., thematrix “wraps” so that it represents a horizontal cylinder. Legal steps are illustratedon the right.Theweightof a path is the sum of the integers in each of thencells of the matrix that are visited.For example, two slightly different56matrices are shown below (the only difference is the numbersin the bottom row).The minimal path is illustrated for each matrix. Note that the path for the matrix on the righttakes advantage of the adjacency property of the first and last rows.InputThe input consists of a sequence of matrix specifications. Each matrix specification consists of the rowand column dimensions in that order on a line followed bymnintegers wheremis the row dimensionandnis the column dimension. The integers appear in the input in row major order, i.e., the firstnintegers constitute the first row of the matrix, the secondnintegers constitute the second row and soon. The integers on a line will be separated from other integers by one or more spaces.Note:integersare not restricted to being positive.There will be one or more matrix specifications in an input file. Input is terminated by end-of-file.For each specification the number of rows will be between 1 and 10 inclusive; the number of columnswill be between 1 and 100 inclusive. No path’s weight will exceed integer values representable using 30bits.OutputTwo lines should be output for each matrix specification in the input file, the first line represents aminimal-weight path, and the second line is the cost of a minimal path. The path consists of a sequenceofnintegers (separated by one or more spaces) representing the rows that constitute the minimal path.If there is more than one path of minimal weight the path that islexicographicallysmallest should beoutput.Note:Lexicographicallymeans the natural order on sequences induced by the order on their elements