// PNGImageReader.c++ -- subclass of ImageReader for reading png image files

// This software was developed by James R. Miller (jrmiller@ku.edu) and is
// OPEN SOURCE.

#include <stdio.h>

#include "png.h"

#include "PNGImageReader.h"

using namespace std;

// The copy constructor cannot be used.

PNGImageReader::PNGImageReader(const PNGImageReader& j) : ImageReader(j)
{
}

// Here are the supported constructors

PNGImageReader::PNGImageReader(std::string fileName) :
	ImageReader(fileName)
{
	readImage();
}

int PNGImageReader::numChannelsFromColorType(int cType)
{
	if (debug)
		cout << "PNGImageReader::numChannelsFromColorType: ";
	switch (cType)
	{
		case PNG_COLOR_TYPE_GRAY:
			if (debug)
				cout << "PNG_COLOR_TYPE_GRAY ==> 1\n";
			return 1;
		case PNG_COLOR_TYPE_GRAY_ALPHA:
			cerr << getFileName() << " has unsupported PNG_COLOR_TYPE_GRAY_ALPHA\n";
			return 0;
		case PNG_COLOR_TYPE_PALETTE:
			cerr << getFileName() << " has unsupported PNG_COLOR_TYPE_PALETTE\n";
			return 0;
		case PNG_COLOR_TYPE_RGB:
			if (debug)
				cout << "PNG_COLOR_TYPE_RGB ==> 3\n";
			return 3;
		case PNG_COLOR_TYPE_RGB_ALPHA:
			if (debug)
				cout << "PNG_COLOR_TYPE_RGB_ALPHA ==> 4\n";
			return 4;
		case PNG_COLOR_MASK_PALETTE:
			cerr << getFileName() << " has unsupported PNG_COLOR_MASK_PALETTE\n";
			return 0;
//		case PNG_COLOR_MASK_COLOR: Same as PNG_COLOR_TYPE_RGB
//			cout << "PNG_COLOR_MASK_COLOR\n"; break;
//		case PNG_COLOR_MASK_ALPHA: Same as PNG_COLOR_TYPE_GRAY_ALPHA
//			cout << "PNG_COLOR_MASK_ALPHA\n"; break;
		default:
			cerr << getFileName() << " has UNKNOWN PNG color type: " << cType << "\n";
			return 0;
	}
}

bool PNGImageReader::read()
{
    FILE *fp = fopen(fullFileName.c_str(), "rb");
    if (fp == nullptr)
	{
		cerr << "PNGImageReader::read - could not open: '" << fullFileName
		     << "'\n";
        return false;
	}
	// check signature
	const int NUM_HEADER_BYTES_TO_CHECK = 8;
	unsigned char header[NUM_HEADER_BYTES_TO_CHECK];
	fread(header, 1, NUM_HEADER_BYTES_TO_CHECK, fp);
	if (png_sig_cmp(header, 0, NUM_HEADER_BYTES_TO_CHECK) != 0)
	{
		cerr << "PNGImageReader::read - bad signature\n";
        return false;
	}
	png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, nullptr, nullptr, nullptr);
	if (png_ptr == nullptr)
	{
		cerr << "PNGImageReader::read - could not allocate png_struct\n";
		return false;
	}
	png_infop info_ptr = png_create_info_struct(png_ptr);
	if (info_ptr == nullptr)
	{
		png_destroy_read_struct(&png_ptr, (png_infopp)nullptr, (png_infopp)nullptr);
		cerr << "PNGImageReader::read - could not allocate png_info\n";
		return false;
	}
	// set up input reading code
	png_init_io(png_ptr, fp);
	png_set_sig_bytes(png_ptr, NUM_HEADER_BYTES_TO_CHECK); // tell it we read the signature
	png_read_info(png_ptr, info_ptr);

	// get dimensions and such so we can
	png_uint_32 width, height;
	int bit_depth, color_type;
	png_get_IHDR(png_ptr, info_ptr, &width, &height, &bit_depth, &color_type,
				 nullptr, nullptr, nullptr); // don't care about interlace, compression, or filter types
	if (debug)
	{
		cout << "PNGImageReader::read - Image specs: "
		     << width << " x " << height << "; bit_depth: " << bit_depth
		     << "; color_type: " << color_type << "\n";
	}
	int nChannels = numChannelsFromColorType(color_type);
	if (nChannels == 0)
		return false;
	if (debug)
		cout << "PNGImageReader::read - nChannels = " << nChannels << '\n';
	theImage = new cryph::Packed3DArray<GLubyte>(height, width, nChannels);
	GLubyte* p = theImage->getModifiableData();
	png_bytep* row_pointers = new png_byte*[height];
	// need to flip order of the rows:
	int rpi = height;
	for (int i=0 ; i<height ; i++)
		row_pointers[--rpi] = p + i*width*nChannels*sizeof(GLubyte);
	png_read_image(png_ptr, row_pointers);
	delete [] row_pointers;
	png_read_end(png_ptr, (png_infop)nullptr);
	png_destroy_read_struct(&png_ptr, &info_ptr, nullptr);
	return true;
}
