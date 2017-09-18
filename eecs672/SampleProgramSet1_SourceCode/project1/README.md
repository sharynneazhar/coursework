# Project 1 - Cubic Curves

Cubic curves are commonly used in freeform design. One mathematical representation that is well-suited to rendering such curves uses the power basis and is written:

(x(t), y(t)) = (a<sub>0</sub> + a<sub>1</sub>t + a<sub>2</sub>t<sup>2</sup> + a<sub>3</sub>t<sup>3</sup>, b<sub>0</sub> + b<sub>1</sub>t + b<sub>2</sub>t<sup>2</sup> + b<sub>3</sub>t<sup>2</sup>)

where the four a<sub>i</sub> and the four b<sub>i</sub> are constants that define the shape of the curve. The parameter, t, varies across some parametric range: t<sub>min</sub> ≤ t ≤ t<sub>max</sub>.

## Project 2

You will read a file and create one ModelView instance for each curve described in the file. The format of the data describing a single curve in this file is as follows:

a<sub>0</sub>  a<sub>1</sub> a<sub>2</sub> a<sub>3</sub>  
b<sub>0</sub>  b<sub>1</sub> b<sub>2</sub> b<sub>3</sub>  
t<sub>min</sub>  t<sub>max</sub> nPoints

Do not make any assumptions where line breaks occur in this file. (It's actually much easier that way, and I will generate other test files that won't necessarily have line breaks in the same places.) There can be multiple curves in a single file, so this sequence of data will be repeated an arbitrary number of times as you will see in the sample data set below.

You will evaluate each curve at the specified number of points ("nPoints") for that curve:

t<sub>min</sub>, t<sub>min</sub> + dt, t<sub>min</sub> + 2 * dt, &hellip;, t<sub>max</sub>   
where dt = (t<sub>max</sub> - t<sub>min</sub>) / (nPoints - 1).

Store the points in an array and send them to the GPU in a VBO.

Each curve must be drawn in a visually distinct constant color. Since there is an unpredictable number of curves to be drawn, it will be sufficient if you simply have a table of 5 or 6 or some such number of unique colors and then cycle through them as you read each curve.

Use a per-primitive uniform to specify this color. **Do not use a VBO for color**. You should only use VBOs in this project to send the coordinate data for the curves to the GPU. If you want white (or some other color) for the background instead of the default black, use <code>glClearColor</code>. You can call it, for example, from the main program immediately before invoking the Controller's <code>run</code> method.

Start from the project1 folder you will find in the SampleProgramSet1_SourceCode.tar.gz file whose link is on the [SampleProgramSet1_SourceCode][1] web page we have been studying. **Do not start from any other base. Your project grade will be adversely affected if you do.** You will need to modify and/or add code in several of the files in the project1 directory as indicated in code comments and in the rest of this assignment. This is an important part of the learning process – hence the requirement to start from this base. The best way to learn the API and the framework is by going through this effort.

Do not modify code in any of the other directories.

### Sample data

Here is a [sample data set][2], and the image it should produce is on the right. Choice of colors is up to you (as long as each is visually distinct), but the shape, size, and placement on the screen should match this very closely. The black border around the image represents the window that would appear on the screen, hence "size" and "placement" relate to that reference.


[1]: https://people.eecs.ku.edu/~jrmiller/Courses/OpenGL/SampleProgramSet1/SampleProgramSet1.html
[2]: https://people.eecs.ku.edu/~jrmiller/Courses/672/Fall2017/Projects/proj1TestDataSet1.txt
