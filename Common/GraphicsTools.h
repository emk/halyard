// -*- Mode: C++; tab-width: 4; -*-

#ifndef GraphicsTools_H
#define GraphicsTools_H

#include "TCommon.h"

#include <assert.h>
#include <stddef.h>

//////////
// Portable graphics primitives.  Over time, it might be good to refactor
// more code into this library.
//
namespace GraphicsTools {

	//////////
	// A distance in pixels.
	//
	typedef signed long Distance;

	//////////
	// An 8-bit color channel.
	//
	typedef unsigned char Channel;

	//////////
	// Multiply two channels together, treating values in the range
	// 0 to 255 as floating point numbers between 0.0 and 1.0 inclusive.
	// It might be wise to compute this function using a table lookup.
	//
	static Channel MultiplyChannels(Channel inLeft, Channel inRight)
	{
		return (((unsigned int) inLeft) * ((unsigned int) inRight)) / 255;
	}

	//////////
	// A point.
	//
	struct Point {
		Distance x;
		Distance y;

		Point() {}
		Point(Distance inX, Distance inY) : x(inX), y(inY) {}

		friend Point operator+(const Point &inLeft, const Point &inRight)
		    { return Point(inLeft.x + inRight.x, inLeft.y + inRight.y); }

		friend Point operator-(const Point &inLeft, const Point &inRight)
		    { return Point(inLeft.x - inRight.x, inLeft.y - inRight.y); }
	};

	//////////
	// A 24-bit color with an 8-bit alpha channel.
	//
	struct Color {
		Channel red;
		Channel green;
		Channel blue;
		Channel alpha;

		Color() {}
		Color(Channel inRed, Channel inGreen,
			  Channel inBlue, Channel inAlpha = 0)
			: red(inRed), green(inGreen), blue(inBlue), alpha(inAlpha) {}

		static Color ApplyAlpha(Color inColor, Channel inAlpha)
		{
			// Mark Noel says we should actually use some kind of log scale here.
			return Color(inColor.red, inColor.green, inColor.blue,
						 255 - MultiplyChannels(255 - inColor.alpha,
												255 - inAlpha));
		}

		friend bool operator==(Color inLeft, Color inRight)
		{
			return (inLeft.red == inRight.red &&
					inLeft.green == inRight.green &&
					inLeft.blue == inRight.blue &&
					inLeft.alpha == inRight.alpha);
		}
	};


	//////////
	// Blend the foreground color into the background color, using the
	// alpha value to control the mixing.  An alpha 255 uses only the
	// background color, and an alpha of 0 uses only the foreground color.
	//
	inline Channel AlphaBlendChannel(Channel inBackground,
									 Channel inForeground,
									 Channel inAlpha)
	{
		return (MultiplyChannels(inBackground, inAlpha) +
				MultiplyChannels(inForeground, 255 - inAlpha));
	}

	//////////
	// A pixmap template.  You can instantiate this with the pixel type of
	// your choice.  This class exposes a lot of 'const' member variables
	// to support high-speed drawing routines.
	//
	template <class Pixel>
	class PixelMap {
	private:
		PixelMap &operator=(const PixelMap &inPixelMap)
		    { ASSERT(false); return *this; }
		PixelMap(const PixelMap &inPixelMap)
			: width(0), height(0), pitch(0), pixels(NULL) { ASSERT(false); }

	public:
		//////////
		// The display width of the pixmap, in pixels. 
		//
		const Distance width;

		//////////
		// The height of the pixmap, in pixels. 
		//
		const Distance height;

		//////////
		// The memory width of the pixmap, in pixels. 
		//
		const Distance pitch;

		//////////
		// The actual pixmap.  To access the pixel at a given x,y
		// co-ordinate, use 'pixmap->pixels[x + bitmap->pitch * y]'.
		// If you want some error-checking, it might be better
		// to call 'At' (below).
		//
		Pixel *const pixels;		

	public:
		//////////
		// Create a pixmap.
		//
		PixelMap(Distance inWidth, Distance inHeight);

		//////////
		// Destroy a pixmap.
		//
		~PixelMap();

		//////////
		// Clear the pixmap to the specified color.
		//
		void Clear(Pixel inColor);

		//////////
		// Access the pixel at (inX,inY).  You can use this function
		// on the left-hand-side of an assignment.
		// 
		Pixel &At(int inX, int inY)
		{
			ASSERT(inX >= 0 && inX < width);
			ASSERT(inY >= 0 && inY < height);
			return pixels[inX + pitch * inY];
		}

		//////////
		// Access the pixel at 'inPoint'.  You can use this function
		// on the left-hand-side of an assignement.
		//
		Pixel &At(Point inPoint)
		{
			return At(inPoint.x, inPoint.y);
		}

		//////////
		// Estimate the memory used to store this pixmap.
		//
		size_t EstimatedMemoryUse() const
		{
			return sizeof(PixelMap<Pixel>) + sizeof(Pixel) * height * pitch;
		}
	};

	//////////
	// An RGBA pixmap, used as a portable output buffer by a number
	// of different graphics-related subsystems (i.e., Typography).
	//
	class PixMap : public PixelMap<Color> {
	public:
		PixMap(Distance inWidth, Distance inHeight)
			: PixelMap<Color>(inWidth, inHeight) {}
	};

	//////////
	// A single-channel pixel map, typically used for storing greyscale
	// images.
	//
	class GreyMap : public PixelMap<Channel> {
	public:
		GreyMap(Distance inWidth, Distance inHeight)
			: PixelMap<Channel>(inWidth, inHeight) {}

		//////////
		// Fill a color pixmap with 'inColor', treating our grey values as
		// an alpha channel, where white -> transparent, and black ->
		// opaque.  For example, this could take a greyscale letter "A" and
		// create a green "A" on a transparent background.
		//
		// [out] outPixMap - A PixMap to fill.  It must be at least
		//                   as large as this GreyMap.
		//
		void TransferToPixMap(Color inColor, PixMap *outPixMap) const;
	};

	//////////
	// An abstract class representing a drawing area.  No pixel-by-pixel
	// access is available; for that, create pixmaps and call DrawPixMap.
	//
	class Image {
	public:
		Image() {}
		virtual ~Image() {}

		//////////
		// Draw a pixmap at the specified point.
		//
		virtual void DrawPixMap(Point inPoint, PixMap &inPixMap) = 0;
	};
}

#endif // GraphicsTools_H
