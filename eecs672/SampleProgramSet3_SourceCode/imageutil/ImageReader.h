//  ImageReader.h -- Abstract Base Class. Read an Image file and allow it to be
//       queried and passed to various OpenGL routines like glDrawPixels and
//       glTexImage2D. Currently the following image file types are supported:
//          * BMP format (.bmp)
//          * JPEG format (.jpg; .jpeg) (Assumes a system jpeg library)
//          * PNG format (.png) (Assumes a system png library)
//          * TARGA format (.tga)
//
//  Except as noted in the Acknowledgments below, this code was written by
//  James R. Miller (jrmiller@ku.edu) and is OPEN SOURCE.
//
// Quick synopsis of use:
// 1. Use ImageReader::create to open and read an image file
// 2. The various query methods can be used to retrieve relevant parameters
//    for OpenGL function calls like glTexImage2D.
//
// Acknowledgments:
// Various public domain and/or open source image reading utilities written by
// other people are used in files associated with the ImageReader class.
// Attribution appears in those files obtained from other sources.

#ifndef IMAGEREADER_H
#define IMAGEREADER_H

#ifdef __APPLE_CC__
#include "GLFW/glfw3.h"
#else
#include <GL/gl.h>
#endif

#include "Packed3DArray.h"

class ImageReader
{
public:
	virtual ~ImageReader() {}

	// Methods to return appropriate values for various parameters to
	// OpenGL functions like glTexImage2D, glDrawPixels, etc.
	int		getBorder() const;
	GLenum	getFormat() const;
	GLint	getInternalFormat() const;
	GLenum	getType() const;
	int		getWidth() const;
	int		getHeight() const;

	// In the "getTexture" call, a pointer to the actual internal image
	// is returned. The caller should NEVER delete this pointer.
	const void* getTexture() const;

	// Actual number of logical channels per pixel; e.g., 3 ==> RGB; 4 ==> RGBA
	int		getNumChannels() const;

	// convenience interface; allows you to record a textureID with an instance
	GLuint getTextureID() const { return textureID; }
	void setTextureID(GLuint tID);

	void	setDebug(bool b) { debug = b; }
	
	std::string getFileName() const { return fullFileName; }

	// public class methods
	// 1. create - dynamically allocates an ImageReader instance of the
	//             appropriate subtype if the type can be determined from the
	//             file name extension.
	//             It returns nullptr if either the file does not exist, or its
	//             type cannot be determined based on the file name extension.
	//             If a non-nullptr ImageReader pointer is returned, the caller is
	//             responsible for deleting it when it is done with it.
	static ImageReader*	create(std::string fileName,
						bool debug=ImageReader::defaultDebug);
	// 2. Miscellaneous
	static void setDefaultDebug(bool b) { defaultDebug = b; }
	static void	setEnsureAlphaChannel(bool b) { ensureAlphaChannel = b; }
	static void	setPromoteSingleChannelToGray(bool b)
		{ promoteSingleChannelToGray = b; }
	const cryph::Packed3DArray<GLubyte>* getInternalPacked3DArrayImage() const
		{ return theImage; }

protected:
	// Since the class is abstract, you CANNOT use these constructors.
	// Use instead the class method "create" or create instances of
	// concrete subclasses (TGAImageReader, RGBImageReader, etc.)
	ImageReader(const std::string& fileName);

	ImageReader(const ImageReader& s); // cannot use the copy constructor

	virtual bool read() = 0;
	void	readImage();

	// The image read from the file
	cryph::Packed3DArray<GLubyte>*	theImage;

	int	border; // always '0' in OpenGL 3.3 and beyond
	std::string	fullFileName;
	bool	debug;
	bool	readFailed;

private:

	// Other private instance methods:
	static ImageReader* guessFileType(const std::string& fileName);

	GLuint textureID;

	static bool defaultDebug;
	static bool	ensureAlphaChannel, promoteSingleChannelToGray;
};

#endif
