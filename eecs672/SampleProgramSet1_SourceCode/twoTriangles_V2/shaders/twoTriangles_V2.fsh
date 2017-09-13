#version 410 core

in float ldsX, ldsY; // Logical Device Space: -1 <= lds_ <= +1
uniform int colorMode;

// parameters for checkerboard.
// Exercise for those interested: Modify the code so that these are imported
// as uniform variables.

const vec4 colorOdd = vec4(1.0, 0.0, 0.0, 1.0), colorEven = vec4(0.0, 1.0, 1.0, 1.0);

// END: parameters for checkerboard

out vec4 fragmentColor;

vec4 checkerboard(int blendAcrossRow, int blendAcrossCol, int numRowsCols)
{
	float row0To1 = 0.5 * (ldsY + 1.0);
	float col0To1 = 0.5 * (ldsX + 1.0);
	vec4 fColor;
	if (numRowsCols <= 0)
	{
		if ((1.0-row0To1) < col0To1)
			fColor = vec4(0.5, 0.5, 0.7, 1.0);
		else
			fColor = vec4(0.7, 0.5, 0.5, 1.0);
	}
	else
	{
		// The (row, col) coming in are in a 0..1 range. We want to scale:
		float r = row0To1 * numRowsCols;
		float c = col0To1 * numRowsCols;
		float r0or1 = mod(floor(r), 2.0);
		float c0or1 = mod(floor(c), 2.0);
		bool odd = ((r0or1 + c0or1) == 1.0);
		
		if ((blendAcrossRow == 0) && (blendAcrossCol == 0))
		{
			if (odd)
				fColor = colorOdd;
			else
				fColor = colorEven;
		}
		else if ((blendAcrossRow == 0) && (blendAcrossCol == 1))
		{
			float colFract = fract(c);
			if (odd)
				fColor = mix(colorOdd, colorEven, colFract);
			else
				fColor = mix(colorEven, colorOdd, colFract);
		}
		else if ((blendAcrossCol == 0) && (blendAcrossRow == 1))
		{
			float rowFract = fract(r);
			if (odd)
				fColor = mix(colorOdd, colorEven, rowFract);
			else
				fColor = mix(colorEven, colorOdd, rowFract);
		}
		else // blending across the row AND the column
		{
			float colFract = fract(c);
			vec4 c1;
			if (odd)
				c1 = mix(colorOdd, colorEven, colFract);
			else
				c1 = mix(colorEven, colorOdd, colFract);
			float rowFract = fract(r);
			vec4 c2;
			if (odd)
				c2 = mix(colorOdd, colorEven, rowFract);
			else
				c2 = mix(colorEven, colorOdd, rowFract);
			fColor = mix(c1, c2, 0.5);
		}
	}
	return fColor;
}

//    A fragment shader function drawing Mandlebrot and Julia sets.
//    Source adapted from "The Orange Book" example authored by Dave Baldwin,
//    Steve Koren, and Randi Rost (which they say was based on a shader by
//    Michael Rivero).

// parameters. Exercise: import these as uniform variables.
const int   MaxIterations = 50;
const float MaxRSquared = 4.0;
const float Zoom = 2.7;
const float XCenter = -0.3;
const float YCenter =  1.3;
const vec3 InnerColor = vec3(0.0, 0.0, 0.0);
const vec3 OuterColor1 = vec3(1.0, 0.0, 0.0);
const vec3 OuterColor2 = vec3(1.0, 1.0, 0.0);

const float Jreal = -0.765;
const float Jimag = 0.11;
// END: parameters

vec4 colorFrom(float real, float imag, float Creal, float Cimag)
{
	float rSquared = 0.0;
	
	int iter;
	for (iter=0; (iter < MaxIterations) && (rSquared < MaxRSquared) ; iter++)
	{
		float tempreal = real;
		real = tempreal*tempreal - imag*imag + Creal;
		imag = 2.0*tempreal*imag + Cimag;
		rSquared = real*real + imag*imag;
	}
	
	// Base the color on the number of iterations
	
	vec3 color;
	if (rSquared < MaxRSquared)
		color = InnerColor;
	else
		color = mix(OuterColor1, OuterColor2, fract(float(iter)/float(MaxIterations)));//*0.05));
	return vec4(color, 1.0);
}

vec4 computeMandelOrJulia(int MandelOrJulia)
{
	float real = ldsX*Zoom + XCenter;
	float imag = ldsY*Zoom;
	if (ldsY < 0.0)
		imag += YCenter;
	else
		imag -= YCenter;
	
	// Not all GLSL implementations support conditional returns, so we
	// conditionally assign to "theColor" and unconditionally return it.
	vec4 theColor;
	if (MandelOrJulia == 0)
		// Mandelbrot set
		theColor = colorFrom(real,imag,real,imag);
	else
		// Julia set
		theColor = colorFrom(real,imag,Jreal,Jimag);
	
	return theColor;
}
void main()
{
	const float PI = 3.14159;
	float x0To1 = 0.5 * (ldsX + 1.0);
	float y0To1 = 0.5 * (ldsY + 1.0);
	switch (colorMode)
	{
		case 0: // Mandelbrot set
			fragmentColor = computeMandelOrJulia(0);
			break;
		case 1: // Julia set
			fragmentColor = computeMandelOrJulia(1);
			break;
		case 2: // checkerboard
			fragmentColor = checkerboard(0, 0, 16);
			break;
		case 3: // checkerboard
			fragmentColor = checkerboard(1, 1, 16);
			break;
		case 4: // checkerboard
			fragmentColor = checkerboard(0, 1, 8);
			break;
		case 5: // trig functions used for varying magenta
			fragmentColor = vec4(abs(cos(ldsX*PI)), 0.0, abs(cos(ldsX*PI)), 1.0);
			break;
		case 6: // trig functions used for varying red-green
			fragmentColor = vec4(abs(cos(ldsX*PI)), abs(sin(ldsY*PI)), 0.0, 1.0);
			break;
		case 7: // trig functions used for varying red
			fragmentColor = vec4(abs(cos(ldsX*PI)), 0.0, 0.0, 1.0);
			break;
		case 8: // trig functions used for varying blue
			fragmentColor = vec4(0.0, 0.0, abs(cos(1.7*ldsX*ldsY*PI)), 1.0);
			break;
		case 9: // varying green-blue
			fragmentColor = vec4(0.0, x0To1, y0To1, 1.0);
			break;
		default: // UNKNOWN ERROR: set solid black
			fragmentColor = vec4(0.0, 0.0, 0.0, 1.0);
	}
}

