// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>

#include "TCommon.h"

// This file contains stubs for MMX support, for what it's worth.  This
// is one in a long series of experimental optimizations that didn't
// seem to buy enough performance to be worth the hassle.
#define USE_MMX 0

#ifdef FIVEL_PLATFORM_WIN32
// Crank up the optimizations under MSVC.  It's not clear that this does
// anything useful (or anything at all, in debug mode).
#pragma optimize("agt", on)
#endif

#include "Stage.h"

// This might be a useful optimization, but we haven't implemented
// it yet.  Feel free to remove the stubs if they linger.
#if 0
void Stage::DrawGreyMap(GraphicsTools::Point inPoint,
						GraphicsTools::GreyMap &inGreyMap,
						GraphicsTools::Color inColor)
{
    // Mark the rectangle as dirty.
    InvalidateRect(wxRect(inPoint.x, inPoint.y,
						  inGreyMap.width, inGreyMap.height));
	
	using GraphicsTools::Channel;
	using GraphicsTools::Color;
	using GraphicsTools::Distance;
	using GraphicsTools::Point;
	
	// Clip our pixmap boundaries to fit within our screen.
	Point begin, end;
	inGreyMap.ClipDrawOperation(inPoint, mStageSize.GetWidth(),
								mStageSize.GetHeight(), begin, end);
		
	// Figure out where in memory to begin drawing the first row.
	wxRawBitmapPixelRef24 dst_base_addr = mOffscreenPixmap.GetData24();
	wxRawBitmapStride24 dst_row_size = mOffscreenPixmap.GetStride24();
	wxRawBitmapPixelRef24 dst_row_start = dst_base_addr;
	WX_RAW24_OFFSET(dst_row_start, dst_row_size,
					inPoint.x + begin.x, inPoint.y + begin.y);
	WX_RAW24_DECLARE_LIMITS(dst_limits, mOffscreenPixmap);

	// Figure out where in memory to get the data for the first row.
	Channel *src_base_addr = inGreyMap.pixels;
	Distance src_row_size = inGreyMap.pitch;
	Channel *src_row_start = src_base_addr + begin.y * src_row_size + begin.x;

	// Do the actual drawing.
	for (int y = begin.y; y < end.y; y++)
	{
		wxRawBitmapPixelRef24 dst_cursor = dst_row_start;
		Channel *src_cursor = src_row_start;
		for (int x = begin.x; x < end.x; x++)
		{
			// Make sure we're in bounds.
			WX_RAW24_ASSERT_WITHIN_LIMITS(dst_limits, dst_cursor);
			ASSERT(src_cursor >= src_base_addr);
			ASSERT(src_cursor <
				   src_base_addr + (inGreyMap.height * src_row_size));
			
			// Draw a single pixel.
			/*
			GraphicsTools::Color new_color = *src_cursor;
			WX_RAW24_RED(dst_cursor) =
				AlphaBlendChannel(WX_RAW24_RED(dst_cursor),
								  new_color.red, new_color.alpha);
			WX_RAW24_GREEN(dst_cursor) =
				AlphaBlendChannel(WX_RAW24_GREEN(dst_cursor),
								  new_color.green, new_color.alpha);
			WX_RAW24_BLUE(dst_cursor) =
				AlphaBlendChannel(WX_RAW24_BLUE(dst_cursor),
								  new_color.blue, new_color.alpha);
			*/

			WX_RAW24_OFFSET_X(dst_cursor, 1);
			src_cursor++;
		}

		WX_RAW24_OFFSET_Y(dst_row_start, dst_row_size, 1);
		src_row_start += src_row_size;
	}	
}
#endif // 0

void Stage::DrawPixMap(GraphicsTools::Point inPoint,
					   GraphicsTools::PixMap &inPixMap)
{
	// Mark the rectangle as dirty.
	InvalidateRect(wxRect(inPoint.x, inPoint.y,
						  inPixMap.width, inPixMap.height));

	using GraphicsTools::AlphaBlendChannel;
	using GraphicsTools::Color;
	using GraphicsTools::Distance;
	using GraphicsTools::Point;
	
	// Clip our pixmap boundaries to fit within our screen.
	Point begin, end;
	inPixMap.ClipDrawOperation(inPoint, mStageSize.GetWidth(),
							   mStageSize.GetHeight(), begin, end);
		
	// Figure out where in memory to begin drawing the first row.
	wxRawBitmapPixelRef24 dst_base_addr = mOffscreenPixmap.GetData24();
	wxRawBitmapStride24 dst_row_size = mOffscreenPixmap.GetStride24();
	wxRawBitmapPixelRef24 dst_row_start = dst_base_addr;
	WX_RAW24_OFFSET(dst_row_start, dst_row_size,
					inPoint.x + begin.x, inPoint.y + begin.y);
	//WX_RAW24_DECLARE_LIMITS(dst_limits, mOffscreenPixmap);
	
	// Figure out where in memory to get the data for the first row.
	Color *src_base_addr = inPixMap.pixels;
	Distance src_row_size = inPixMap.pitch;
	Color *src_row_start = src_base_addr + begin.y * src_row_size + begin.x;

	// Do the actual drawing.
	for (int y = begin.y; y < end.y; y++)
	{
		wxRawBitmapPixelRef24 dst_cursor = dst_row_start;
		Color *src_cursor = src_row_start;
		for (int x = begin.x; x < end.x; x++)
		{
			// Make sure we're in bounds.
			//WX_RAW24_ASSERT_WITHIN_LIMITS(dst_limits, dst_cursor);
			//ASSERT(src_cursor >= src_base_addr);
			//ASSERT(src_cursor <
			//	   src_base_addr + (inPixMap.height * src_row_size));
			
			// Draw a single pixel.
			GraphicsTools::Color new_color = *src_cursor;
			WX_RAW24_RED(dst_cursor) =
				AlphaBlendChannel(WX_RAW24_RED(dst_cursor),
								  new_color.red, new_color.alpha);
			WX_RAW24_GREEN(dst_cursor) =
				AlphaBlendChannel(WX_RAW24_GREEN(dst_cursor),
								  new_color.green, new_color.alpha);
			WX_RAW24_BLUE(dst_cursor) =
				AlphaBlendChannel(WX_RAW24_BLUE(dst_cursor),
								  new_color.blue, new_color.alpha);

			WX_RAW24_OFFSET_X(dst_cursor, 1);
			src_cursor++;
		}

		WX_RAW24_OFFSET_Y(dst_row_start, dst_row_size, 1);
		src_row_start += src_row_size;
	}
}

void Stage::ShowFadeStep(int inIntensity)
{
	wxASSERT(0 <= inIntensity && inIntensity <= 256);

	// Prepare to iterate through our source pixmap.
	wxRawBitmapPixelRef24 src_base_addr = mOffscreenPixmap.GetData24();
	wxRawBitmapStride24 src_row_size = mOffscreenPixmap.GetStride24();
	wxRawBitmapPixelRef24 src_row_start = src_base_addr;
	//WX_RAW24_DECLARE_LIMITS(src_limits, mOffscreenPixmap);

	// Prepare to iterate through our destination pixmap.
	wxRawBitmapPixelRef24 dst_base_addr = mOffscreenFadePixmap.GetData24();
	wxRawBitmapStride24 dst_row_size = mOffscreenFadePixmap.GetStride24();
	wxRawBitmapPixelRef24 dst_row_start = dst_base_addr;
	//WX_RAW24_DECLARE_LIMITS(dst_limits, mOffscreenFadePixmap);

#if USE_MMX
	OptimizedDrawingStart();
#endif // USE_MMX

	// Copy and fade values.
	int rows = mStageSize.GetHeight();
	while (rows--)
	{
#if USE_MMX
		wxRawBitmapPixelRef24 src_row_end = src_row_start;
		WX_RAW24_OFFSET_X(src_row_end, mStageSize.GetWidth());
		OptimizedFadePixels(src_row_start, src_row_end, dst_row_start,
							inIntensity);

#else // !USE_MMX
		wxRawBitmapPixelRef24 src_cursor = src_row_start;
		wxRawBitmapPixelRef24 dst_cursor = dst_row_start;

		int columns = mStageSize.GetWidth();
		while (columns--)
		{
			WX_RAW24_RED(dst_cursor) =
				(WX_RAW24_RED(src_cursor) * inIntensity) >> 8;
			WX_RAW24_GREEN(dst_cursor) =
				(WX_RAW24_GREEN(src_cursor) * inIntensity) >> 8;
			WX_RAW24_BLUE(dst_cursor) =
				(WX_RAW24_BLUE(src_cursor) * inIntensity) >> 8;

			WX_RAW24_OFFSET_X(src_cursor, 1);
			WX_RAW24_OFFSET_X(dst_cursor, 1);
		}
#endif // !USE MMX

		WX_RAW24_OFFSET_Y(src_row_start, src_row_size, 1);
		WX_RAW24_OFFSET_Y(dst_row_start, dst_row_size, 1);
	}

#if USE_MMX
	OptimizedDrawingEnd();
#endif // USE_MMX

	// Blit our fade pixmap to the screen.  This operation takes about
	// 4 milliseconds on emk's 800MHz laptop.  If we combine this operation
	// with a memory-to-memory blit, the pair of blits require about
	// 14 milliseconds.  These numbers were measured under wxWindows 2.3.3
	// and Win98 with an Intel i815em graphics card.
    wxClientDC client_dc(this);
	mOffscreenFadePixmap.BlitTo(&client_dc, 0, 0,
								mStageSize.GetWidth(), mStageSize.GetHeight(),
								0, 0);
}
