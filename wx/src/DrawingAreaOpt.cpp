// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#include "TamaleHeaders.h"

#define IN_DRAWING_AREA_OPT 1

#include "DrawingAreaOpt.h"

USING_NAMESPACE_FIVEL

using GraphicsTools::Channel;
using GraphicsTools::AlphaBlendChannel;
using GraphicsTools::MultiplyChannels;
using GraphicsTools::Color;
using GraphicsTools::Distance;
using GraphicsTools::Point;

typedef wxNativePixelData::Iterator NativeIterator;
typedef wxAlphaPixelData::Iterator AlphaIterator;

template <class Iterator>
static void BlendPixel(Iterator &i, const Color &c);

template <> __inline static void
BlendPixel<NativeIterator>(NativeIterator &i, const Color &c) {
	i.Red()   = AlphaBlendChannel(i.Red(),   c.red,   c.alpha);
	i.Green() = AlphaBlendChannel(i.Green(), c.green, c.alpha);
	i.Blue()  = AlphaBlendChannel(i.Blue(),  c.blue,  c.alpha);
}

template <> __inline static void
BlendPixel<AlphaIterator>(AlphaIterator &i, const Color &c) {
	// We need to combine two colors, each with an alpha value.  Let:
	//
	//   x = composited background
	//   y = layer pointed to by 'i' (range 0 to 1)
    //   z = layer pointed to by 'c'
	//   m = blend of x, y and z layers
	//   n = blend of y and z layers
    //   a_n = alpha value for layer n (range 0 to 1, 1 is opaque)
    //   \bar{a_n} = 1 - a_n
	//
	// We know that:
	//
    //   m = (x\bar{a_y} + ya_y)\bar{a_z} + za_z
    //   m = x\bar{a_n} + na_n
	//
	// If we solve for n and na_n, we get:
	//
	//   n = \over{ya_y\bar{a_z} + za_z}{a_n}
	//   a_n = \bar{\bar{a_y}\bar{a_z}}
	//
	// However, reality is a bit more rude: 'i' points towards 255-opaque,
	// premuliplied pixel data.  'c' contains 255-opaque, non-premuliplied
	// pixel data.  So we actually need something like this:
	i.Red()   = AlphaBlendChannel(i.Red(),   c.red,   c.alpha);
	i.Green() = AlphaBlendChannel(i.Green(), c.green, c.alpha);
	i.Blue()  = AlphaBlendChannel(i.Blue(),  c.blue,  c.alpha);
	i.Alpha() = Color::CombineAlphaChannels(i.Alpha(), c.alpha);
}

template <class PixelData>
void DrawPixMapOpt(PixelData &inDstData,
				   GraphicsTools::Point inPoint,
				   GraphicsTools::PixMap &inPixMap)
{
	// Clip our pixmap boundaries to fit within our screen.
	Point begin, end;
	inPixMap.ClipDrawOperation(inPoint, inDstData.GetWidth(),
							   inDstData.GetHeight(), begin, end);
		
	// Figure out where in memory to begin drawing the first row.
	if ( !inDstData )
		gLog.FatalError("Error: Can't access raw pixels for bitmap");
	PixelData::Iterator dst_row_start(inDstData);
	dst_row_start.Offset(inDstData, inPoint.x + begin.x, inPoint.y + begin.y);
	
	// Figure out where in memory to get the data for the first row.
	Color *src_base_addr = inPixMap.pixels;
	Distance src_row_size = inPixMap.pitch;
	Color *src_row_start = src_base_addr + begin.y * src_row_size + begin.x;

	// Do the actual drawing.
	for (int y = begin.y; y < end.y; y++)
	{
		PixelData::Iterator dst_cursor = dst_row_start;
		Color *src_cursor = src_row_start;
		for (int x = begin.x; x < end.x; x++)
		{
			// Make sure we're in bounds.
			// TODO - Check dst_cursor is in bounds.
			//ASSERT(src_cursor >= src_base_addr);
			//ASSERT(src_cursor <
			//	   src_base_addr + (inPixMap.height * src_row_size));
			
			// Draw a single pixel.
			BlendPixel(dst_cursor, *src_cursor);

			dst_cursor++;
			src_cursor++;
		}

		dst_row_start.OffsetY(inDstData, 1);
		src_row_start += src_row_size;
	}

}

template <class PixelData>
void FillBoxOpt(PixelData &inDstData,
				const wxRect &inBounds, 
				const GraphicsTools::Color &inColor)
{
	// Clip the rectangle to fit within the screen 
	Point begin, end;
	begin.x = Max(0, Min(inDstData.GetWidth(), inBounds.x));
	begin.y = Max(0, Min(inDstData.GetHeight(), inBounds.y));
	end.x = Max(0, Min(inDstData.GetWidth(), 
					   inBounds.x + inBounds.width));
	end.y = Max(0, Min(inDstData.GetHeight(),
					   inBounds.y + inBounds.height));

	// Get iterator for directly accessing memory.
	if ( !inDstData )
		gLog.FatalError("Error: Can't access raw pixels for bitmap");
	PixelData::Iterator row_start(inDstData);
	row_start.Offset(inDstData, inBounds.x, inBounds.y);
	
	// Draw it
	for (int y = begin.y; y < end.y; y++)
	{
		PixelData::Iterator cursor = row_start;
		for (int x = begin.x; x < end.x; x++)
		{
			BlendPixel(cursor, inColor);
			cursor++;
		}
		
		row_start.OffsetY(inDstData, 1);
	}

}

void ClearOpt(wxAlphaPixelData &inDstData,
			  const GraphicsTools::Color &inColor)
{
	Point end;
	end.x = inDstData.GetWidth();
	end.y = inDstData.GetHeight();

	if ( !inDstData )
		gLog.FatalError("Error: Can't access raw pixels for bitmap");
	wxAlphaPixelData::Iterator row_start(inDstData);
	
	// Draw it
	Channel a = inColor.alpha;
	Channel r = MultiplyChannels(inColor.red, a);
	Channel g = MultiplyChannels(inColor.green, a);
	Channel b = MultiplyChannels(inColor.blue, a);
	for (int y = 0; y < end.y; y++) {
		wxAlphaPixelData::Iterator cursor = row_start;
		for (int x = 0; x < end.x; x++) {
			cursor.Red()   = r;
			cursor.Green() = g;
			cursor.Blue()  = b;
			cursor.Alpha() = a;

			cursor++;
		}
		
		row_start.OffsetY(inDstData, 1);
	}
}

// Manual template instantiations.
template
void DrawPixMapOpt(wxNativePixelData &inDstData,
				   GraphicsTools::Point inPoint,
				   GraphicsTools::PixMap &inPixMap);
template
void DrawPixMapOpt(wxAlphaPixelData &inDstData,
				   GraphicsTools::Point inPoint,
				   GraphicsTools::PixMap &inPixMap);
template
void FillBoxOpt(wxNativePixelData &inDstData,
				const wxRect &inBounds, 
				const GraphicsTools::Color &inColor);
template
void FillBoxOpt(wxAlphaPixelData &inDstData,
				const wxRect &inBounds, 
				const GraphicsTools::Color &inColor);
