# Project 2: OpenMPI

Consider the following two files:

* [500_Cities\__City-level_Data\__GIS_Friendly_Format_.csv](Project2Data/500_Cities__City-level_Data__GIS_Friendly_Format_.csv)
* [rows.json](Project2Data/rows.json)

Just FYI: These two files were obtained from the CDC at [data.gov](https://catalog.data.gov/dataset/500-cities-city-level-data-gis-friendly-format).

The two files contain the same basic data in two different forms. The <tt>csv</tt> file may be easier for you to parse, but the <tt>json</tt> file contains additional descriptive information that will tell you what the columns of the spreadsheet mean. I believe it will be easiest for you to parse and store the data from the <tt>csv</tt> file.

(The descriptive information in the <tt>json</tt> file will give you more information on what the columns in the spreadsheet really mean, although you shouldn't need to incorporate that into your programs unless you want to produce more descriptive output &ndash; which is not required. You can read the <tt>json</tt> file into some text editor and search for "<tt>description</tt>". Skip the first instance which is the description for the data file as a whole. The second and subsequent instances of "<tt>description</tt>" identify the data stored in columns A, B, C, &hellip;.)

### Project Requirements

1. The rank 0 process must read the <tt>csv</tt> file referenced above and store all the data. You can treat columns with range data (stored, for example, as "(13.3, 14.0)") in any way you want. You will not need to use data from those columns in any part of this project.

2. Use the parameters in argv to determine how the data is to be communicated to other processes as well as how results are to be sent back to the rank 0 process. In general, you will be computing some quantity or quantities, and then reporting the results to the standard output. All I/O (both reading the file as well as writing results to standard output) must be done strictly by the rank 0 process.</li>

3. All queries will be handled either by a "scatter &ndash; reduce" or "broadcast &ndash; gather" strategy. The value of <tt>argv[1]</tt> will be either "<tt>sr</tt>" or "<tt>bg</tt>", respectively.

4. The choices for "<tt>sr</tt>" are:

  | <tt>mpirun</tt> command | answer reported by rank 0 process |  
  |---|---|  
  | <tt>mpirun -np 20 proj2 sr max D</tt> | <tt>New York, NY, Population2010 = 8175133</tt> |   
  | <tt>mpirun -np 20 proj2 sr min D</tt> | <tt>Burlington, VT, Population2010 = 42417</tt> |   
  | <tt>mpirun -np 20 proj2 sr avg CO</tt> | <tt>Average OBESITY_CrudePrev = 21.67<sup>&dagger;</sup></tt> |  
  | <tt>mpirun -np 20 proj2 sr number AS gt 55</tt> | <tt>Number cities with COLON_SCREEN_CrudePrev gt 55 = 243<sup>&dagger;</sup></tt> |  

	The string following "max", "min", "avg", and "number" refers to a column in the original spreadsheet (i.e., column D, column CO, and column AS are in the table above). In addition to supporting "gt" for "greater than", support "lt" for "less than".  No other relationals are needed.  

  An optional extra credit feature would be to support multiple conditionals, in which case an "AND" is assumed to be between them. So, for example:  

	`mpirun -np 20 proj2 sr number CO gt 55 BQ lt 10`  

  would report  

  `Number cities with OBESITY_CrudePrev gt 55 and DIABETES_CrudePrev lt 10 = 129<sup>&dagger;</sup>`  

	Divide the work evenly among the number of processes specified in the "<tt>-np</tt>" directive. There are 500 cities in the data file, and it is fine to assume the number of processes will evenly divide into 500. Moreover, you are free to exit with an error message if the program is launched with a value that does not evenly divide 500.  

  <sup>&dagger;</sup>I don't know if these values are correct.  

4. The choices for "<tt>bg</tt>" are:

  | <tt>mpirun</tt> command | answer reported by rank 0 process |  
  |---|---|  
  | <tt>mpirun -np 4 proj2 bg max D E I M</tt> | <tt>max Population2010 = 8175133</tt> <br /> <tt>max ACCESS2_CrudePrev = 37<sup>&dagger;</sup></tt> <br /> <tt>max ARTHRITIS_CrudePrev = 41.8<sup>&dagger;</sup></tt> <br /> <tt>max BINGE_CrudePrev = 18.3<sup>&dagger;</sup></tt> |  

	In addition to "<tt>max</tt>", support "<tt>min</tt>" and "<tt>avg</tt>". As before, the strings following "<tt>max</tt>", "<tt>min</tt>", or "<tt>avg</tt>" refer to columns in the original spreadsheet, hence they may be two-letters (e.g., "<tt>AS</tt>").  

  The number of processes specified by the "<tt>-np</tt>" <i><b>must</b></i> be the same as the number of columns to be examined. Notice in the example shown, we want to examine four columns (<tt>D</tt>, <tt>E</tt>, <tt>I</tt>, and <tt>M</tt>), and we specified "<tt>-np 4</tt>". Terminate the program with an error message if the number of processes is not the same as the number of columns to scan.  

  Each process must do the work associated with one of the columns requested.  

  <sup>&dagger;</sup>I don't know if these values are correct.
