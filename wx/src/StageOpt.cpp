// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/rawbmp.h>

#include "TCommon.h"
#include "TLogger.h"
#include "Stage.h"

void Stage::DrawPixMap(GraphicsTools::Point inPoint,
					   GraphicsTools::PixMap &inPixMap)
{
#if 0
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
	wxRawBitmapData dst_data(mOffscreenPixmap);
	if ( !dst_data )
		gLog.FatalError("Error: Can't access raw pixels for bitmap");
	wxRawBitmapIterator dst_row_start(dst_data);
	dst_row_start.Offset(inPoint.x + begin.x, inPoint.y + begin.y);
	
	// Figure out where in memory to get the data for the first row.
	Color *src_base_addr = inPixMap.pixels;
	Distance src_row_size = inPixMap.pitch;
	Color *src_row_start = src_base_addr + begin.y * src_row_size + begin.x;

	// Do the actual drawing.
	for (int y = begin.y; y < end.y; y++)
	{
		wxRawBitmapIterator dst_cursor = dst_row_start;
		Color *src_cursor = src_row_start;
		for (int x = begin.x; x < end.x; x++)
		{
			// Make sure we're in bounds.
			// TODO - Check dst_cursor is in bounds.
			//ASSERT(src_cursor >= src_base_addr);
			//ASSERT(src_cursor <
			//	   src_base_addr + (inPixMap.height * src_row_size));
			
			// Draw a single pixel.
			GraphicsTools::Color new_color = *src_cursor;
			wxBMP_RED(dst_cursor) =
				AlphaBlendChannel(wxBMP_RED(dst_cursor),
								  new_color.red, new_color.alpha);
			wxBMP_GREEN(dst_cursor) =
				AlphaBlendChannel(wxBMP_GREEN(dst_cursor),
								  new_color.green, new_color.alpha);
			wxBMP_BLUE(dst_cursor) =
				AlphaBlendChannel(wxBMP_BLUE(dst_cursor),
								  new_color.blue, new_color.alpha);

			dst_cursor.OffsetX(1);
			src_cursor++;
		}

		dst_row_start.OffsetY(1);
		src_row_start += src_row_size;
	}
#endif
}

void Stage::ShowFadeStep(int inIntensity)
{
	wxASSERT(0 <= inIntensity && inIntensity <= 256);

#if 0
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

	// Copy and fade values.
	int rows = mStageSize.GetHeight();
	while (rows--)
	{
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

		WX_RAW24_OFFSET_Y(src_row_start, src_row_size, 1);
		WX_RAW24_OFFSET_Y(dst_row_start, dst_row_size, 1);
	}

	// Blit our fade pixmap to the screen.  This operation takes about
	// 4 milliseconds on emk's 800MHz laptop.  If we combine this operation
	// with a memory-to-memory blit, the pair of blits require about
	// 14 milliseconds.  These numbers were measured under wxWindows 2.3.3
	// and Win98 with an Intel i815em graphics card.
    wxClientDC client_dc(this);
	mOffscreenFadePixmap.BlitTo(&client_dc, 0, 0,
								mStageSize.GetWidth(), mStageSize.GetHeight(),
								0, 0);
#endif
}
