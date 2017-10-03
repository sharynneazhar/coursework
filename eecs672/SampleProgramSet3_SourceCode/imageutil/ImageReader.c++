//  ImageReader.c++ -- read an Image file and allow it to be queried.
// This software was developed by James R. Miller and is open source
//
// Acknowledgments:
// Various public domain and/or open source image reading utilities written by
// other people are used in files associated with the ImageReader class.
// Appropriate attribution appears in those files obtained from other sources.

#include <stdlib.h>
#include <string.h>
#include <fstream>

#include "ImageReader.h"

// Known subclasses (needed by factory method "create"):
#include "BMPImageReader.h"
#include "JPEGImageReader.h"
#include "PNGImageReader.h"
#include "TGAImageReader.h"

using namespace std;
using namespace cryph;

// class variables

bool ImageReader::defaultDebug = false;
bool ImageReader::ensureAlphaChannel = false;
bool ImageReader::promoteSingleChannelToGray = true;

// The copy constructor cannot be used.

ImageReader::ImageReader(const ImageReader&)
{
	std::cerr << "***** ERROR: ImageReader copy constructor called!!\n";
}

// Here are the supported constructors

ImageReader::ImageReader(const std::string& fileName) :
	theImage(nullptr), border(0),
	fullFileName(fileName), debug(ImageReader::defaultDebug),
	readFailed(false), textureID(0)
{
}

// Methods....

ImageReader* ImageReader::create(std::string fileName, bool debug) // CLASS METHOD
{
	ifstream seeIfExists(fileName.c_str());
	if (seeIfExists.good())
		seeIfExists.close();
	else
	{
		std::cerr << "ImageReader::create could not open " << fileName
		          << " for reading.\n";
		return nullptr;
	}
	ImageReader* p = guessFileType(fileName);
	if (p == nullptr)
		return nullptr;

	if (p->readFailed)
	{
		delete p;
		return nullptr;
	}
	p->setDebug(debug);

	if ((p->theImage->getDim3() == 1) && ImageReader::promoteSingleChannelToGray)
	{
		int nRows = p->theImage->getDim1();
		int nCols = p->theImage->getDim2();
		cryph::Packed3DArray<GLubyte>* grayImage = new cryph::Packed3DArray<GLubyte>(nRows, nCols, 3);
		for (int i=0 ; i<nRows ; i++)
			for (int j=0 ; j<nCols ; j++)
			{
				GLubyte b = p->theImage->getDataElement(i,j,0);
				for (int k=0 ; k<3 ; k++)
					grayImage->setDataElement(i,j,k,b);
			}
		delete p->theImage;
		p->theImage = grayImage;
	}
	if ((p->theImage->getDim3() == 3) && ImageReader::ensureAlphaChannel)
	{
		int nRows = p->theImage->getDim1();
		int nCols = p->theImage->getDim2();
		cryph::Packed3DArray<GLubyte>* imageWithAlpha =
			new cryph::Packed3DArray<GLubyte>(nRows, nCols, 4);
		for (int i=0 ; i<nRows ; i++)
			for (int j=0 ; j<nCols ; j++)
			{
				for (int k=0 ; k<3 ; k++)
				{
					GLubyte b = p->theImage->getDataElement(i,j,k);
					imageWithAlpha->setDataElement(i,j,k,b);
				}
				imageWithAlpha->setDataElement(i,j,3,255);
			}
		delete p->theImage;
		p->theImage = imageWithAlpha;
	}
	return p;
}

int ImageReader::getBorder() const
{
	return border;
}

GLenum ImageReader::getFormat() const
{
	int nChannels = theImage->getDim3();
	if (nChannels == 1)
		return GL_RED;
	if (nChannels == 3)
		return GL_RGB;
	if (nChannels == 4)
		return GL_RGBA;

	// probably an error:
	std::cout << "ImageReader::getFormat(), nChannels = " << nChannels << endl;
	return GL_RGB;
}

GLint ImageReader::getInternalFormat() const
{
	int nChannels = theImage->getDim3();
	if (nChannels == 1)
		return GL_RED;
	if (nChannels == 3)
		return GL_RGB;
	if (nChannels == 4)
		return GL_RGBA;
	
	// probably an error:
	std::cout << "ImageReader::getInternalFormat(), nChannels = " << nChannels << endl;
	return GL_RGB;
}

int ImageReader::getNumChannels() const
{
	return theImage->getDim3();
}

int ImageReader::getHeight() const
{
	return theImage->getDim1();
}

int ImageReader::getWidth() const
{
	return theImage->getDim2();
}

const void* ImageReader::getTexture() const
{
	return theImage->getData();
}

GLenum ImageReader::getType() const
{
	return GL_UNSIGNED_BYTE;
}

ImageReader* ImageReader::guessFileType(const std::string& fileName) // CLASS METHOD
{
	int dotLoc = fileName.find_last_of('.');
	if (dotLoc != std::string::npos)
	{
		std::string extension = fileName.substr(dotLoc+1);
		if ((extension.compare("bmp") == 0) || (extension.compare("BMP") == 0))
			return new BMPImageReader(fileName);
		if ((extension.compare("jpg") == 0) || (extension.compare("JPG") == 0))
			return new JPEGImageReader(fileName);
		if ((extension.compare("jpeg") == 0) || (extension.compare("JPEG") == 0))
			return new JPEGImageReader(fileName);
		if ((extension.compare("png") == 0) || (extension.compare("PNG") == 0))
			return new PNGImageReader(fileName);
		if ((extension.compare("tga") == 0) || (extension.compare("TGA") == 0))
			return new TGAImageReader(fileName);
	}

	cerr << "ImageReader::guessFileType cannot determine file type of: "
		<< fileName << endl;
	return nullptr;
}

void ImageReader::readImage()
{
	readFailed = !read();
}

void ImageReader::setTextureID(GLuint tID)
{
	if (debug)
		std::cout << "Setting ImageReader textureID to " << tID << std::endl;
	textureID = tID;
}
