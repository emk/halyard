// -*- Mode: C++; tab-width: 4; -*-

#ifndef GraphicsTools_H
#define GraphicsTools_H

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
	static __inline Channel MultiplyChannels(Channel inLeft, Channel inRight)
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

	public:
		Color() {}
		Color(Channel inRed, Channel inGreen,
			  Channel inBlue, Channel inAlpha = 255)
			: red(inRed), green(inGreen), blue(inBlue), alpha(inAlpha) {}

		__inline bool IsCompletelyOpaque() const
			{ return alpha == 255; }
		__inline bool IsCompletelyTransparent() const
			{ return alpha == 0; }

		//////////
		// Apply an alpha value to a color (which may have an alpha value
		// already).
		//
		static __inline Color ApplyAlpha(Color inColor, Channel inAlpha) {
			// Mark Noel thinks we might need to use some kind of log scale
			// here, but we seem to be fine without it.
			return Color(inColor.red, inColor.green, inColor.blue,
						 MultiplyChannels(inColor.alpha, inAlpha));
		}

		//////////
		// Combine two alpha channels.  This is done when compositing
		// two semi-transparent pixels.
		//
		static __inline Channel
		CombineAlphaChannels(Channel inA1, Channel inA2) {
			return 255 - MultiplyChannels(255 - inA1, 255 - inA2);
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
	// foreground color, and an alpha of 0 uses only the background color.
	//
	static __inline Channel AlphaBlendChannel(Channel inBackground,
											  Channel inForeground,
											  Channel inAlpha)
	{
		return (MultiplyChannels(inBackground, 255 - inAlpha) +
				MultiplyChannels(inForeground, inAlpha));
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

		//////////
		// Assume that this PixelMap will be drawn at inDrawAt on a
		// screen of size (inScreenWidth, inScreenHeight).  Return
		// the portion of this PixelMap which will actually be drawn.
		// outTopLeft is inclusive; outBottomRight is exclusive.
		//
		void ClipDrawOperation(Point inDrawAt,
							   Distance inScreenWidth,
							   Distance inScreenHeight,
							   Point &outTopLeft, Point &outBottomRight);
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

		// This might be a useful optimization, but we haven't implemented
		// it yet.  Feel free to remove the stubs if they linger.
#if 0
		//////////
		// Draw a greymap at the specified point, using the specified
		// color.
		//
		virtual void DrawGreyMap(Point inPoint, GreyMap &inGreyMap,
								 Color inColor) = 0;
#endif // 0

		//////////
		// Draw a pixmap at the specified point.
		//
		virtual void DrawPixMap(Point inPoint, PixMap &inPixMap) = 0;
	};
}

#endif // GraphicsTools_H
