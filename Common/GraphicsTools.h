// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#ifndef GraphicsTools_H
#define GraphicsTools_H

#include <assert.h>
#include <stddef.h>

/// Portable graphics primitives.  Over time, it might be good to refactor
/// more code into this library.
namespace GraphicsTools {

    /// A distance in pixels.
    typedef signed long Distance;

    /// An 8-bit color channel.
    typedef unsigned char Channel;

    /// Multiply two channels together, treating values in the range
    /// 0 to 255 as floating point numbers between 0.0 and 1.0 inclusive.
    /// It might be wise to compute this function using a table lookup.
    static __inline Channel MultiplyChannels(Channel inLeft, Channel inRight)
    {
        return (((unsigned int) inLeft) * ((unsigned int) inRight)) / 255;
    }

    /// A point.
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

    /// A 24-bit color with an 8-bit alpha channel.
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

        /// Apply an alpha value to a color (which may have an alpha value
        /// already).
        static __inline Color ApplyAlpha(Color inColor, Channel inAlpha) {
            // Mark Noel thinks we might need to use some kind of log scale
            // here, but we seem to be fine without it.
            return Color(inColor.red, inColor.green, inColor.blue,
                         MultiplyChannels(inColor.alpha, inAlpha));
        }

        /// Combine two alpha channels.  This is done when compositing
        /// two semi-transparent pixels.
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

        friend std::ostream &operator<<(std::ostream &out, const Color &c) {
            out << "(color " << int(c.red) << " " << int(c.green) << " "
                << int(c.blue) << " " << int(c.alpha) << ")";
            return out;
        }
    };


    /// Blend the foreground color into the background color, using the
    /// alpha value to control the mixing.  An alpha 255 uses only the
    /// foreground color, and an alpha of 0 uses only the background color.
    static __inline Channel AlphaBlendChannel(Channel inBackground,
                                              Channel inForeground,
                                              Channel inAlpha)
    {
        return (MultiplyChannels(inBackground, 255 - inAlpha) +
                MultiplyChannels(inForeground, inAlpha));
    }

    /// A greyscale image.  The 'stride' value is guaranteed to be
    /// compatible with Cario's cairo_image_surface_create_for_data
    /// function.
    class GreyMap : boost::noncopyable {
    public:
        /// The display width of the greymap, in pixels. 
        const Distance width;

        /// The height of the greymap, in pixels. 
        const Distance height;

        /// The memory width of each row of the greymap, in pixels. 
        const Distance stride;

        /// The actual greymap.  To access the pixel at a given x,y
        /// co-ordinate, use 'greymap->pixels[x + bitmap->stride * y]'.
        /// If you want some error-checking, it might be better
        /// to call 'At' (below).
        Channel *const pixels;

    public:
        /// Create a greymap.
        GreyMap(Distance inWidth, Distance inHeight);

        /// Destroy a greymap.
        ~GreyMap();

        /// Clear the greymap to the specified color.
        void Clear(Channel inColor);

        /// Access the pixel at (inX,inY).  You can use this function
        /// on the left-hand-side of an assignment.
        Channel &At(int inX, int inY)
        {
            ASSERT(inX >= 0 && inX < width);
            ASSERT(inY >= 0 && inY < height);
            return pixels[inX + stride * inY];
        }

        /// Access the pixel at 'inPoint'.  You can use this function
        /// on the left-hand-side of an assignement.
        Channel &At(Point inPoint)
        {
            return At(inPoint.x, inPoint.y);
        }

        /// Estimate the memory used to store this greymap.
        size_t EstimatedMemoryUse() const
        {
            return sizeof(GreyMap) + sizeof(Channel) * height * stride;
        }
    };

    /// An abstract class representing a drawing area.  No pixel-by-pixel
    /// access is available; for that, create greymaps and call DrawGreyMap.
    class Image {
    public:
        Image() {}
        virtual ~Image() {}

        /// Draw a greymap at the specified point, using the specified
        /// color.
        virtual void DrawGreyMap(Point inPoint, const GreyMap *inGreyMap,
                                 Color inColor) = 0;
    };
}

#endif // GraphicsTools_H
