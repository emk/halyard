// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#include <wx/wx.h>
#include <wx/rawbmp.h>

#include "TCommon.h"
#include "TLogger.h"
#include "Stage.h"

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
	wxNativePixelData dst_data(mOffscreenPixmap);
	if ( !dst_data )
		gLog.FatalError("Error: Can't access raw pixels for bitmap");
	wxNativePixelData::Iterator dst_row_start(dst_data);
	dst_row_start.Offset(dst_data, inPoint.x + begin.x, inPoint.y + begin.y);
	
	// Figure out where in memory to get the data for the first row.
	Color *src_base_addr = inPixMap.pixels;
	Distance src_row_size = inPixMap.pitch;
	Color *src_row_start = src_base_addr + begin.y * src_row_size + begin.x;

	// Do the actual drawing.
	for (int y = begin.y; y < end.y; y++)
	{
		wxNativePixelData::Iterator dst_cursor = dst_row_start;
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
			dst_cursor.Red() =
				AlphaBlendChannel(dst_cursor.Red(),
								  new_color.red, new_color.alpha);
			dst_cursor.Green() =
				AlphaBlendChannel(dst_cursor.Green(),
								  new_color.green, new_color.alpha);
			dst_cursor.Blue() =
				AlphaBlendChannel(dst_cursor.Blue(),
								  new_color.blue, new_color.alpha);

			dst_cursor++;
			src_cursor++;
		}

		dst_row_start.OffsetY(dst_data, 1);
		src_row_start += src_row_size;
	}
}

void Stage::ShowFadeStep(int inIntensity)
{
	wxASSERT(0 <= inIntensity && inIntensity <= 256);

	// Prepare to iterate through our source pixmap.
	wxNativePixelData src_data(mOffscreenPixmap);
	wxNativePixelData::Iterator src_row_start(src_data);

	// Prepare to iterate through our destination pixmap.
	wxNativePixelData dst_data(mOffscreenFadePixmap);
	wxNativePixelData::Iterator dst_row_start(dst_data);

	// Copy and fade values.
	int rows = mStageSize.GetHeight();
	while (rows--)
	{
		wxNativePixelData::Iterator src_cursor = src_row_start;
		wxNativePixelData::Iterator dst_cursor = dst_row_start;

		int columns = mStageSize.GetWidth();
		while (columns--)
		{
			dst_cursor.Red() =
				(src_cursor.Red() * inIntensity) >> 8;
			dst_cursor.Green() =
				(src_cursor.Green() * inIntensity) >> 8;
			dst_cursor.Blue() =
				(src_cursor.Blue() * inIntensity) >> 8;

			src_cursor++;
			dst_cursor++;
		}

		src_row_start.OffsetY(src_data, 1);
		dst_row_start.OffsetY(dst_data, 1);
	}

	// Blit our fade pixmap to the screen.
	//
	// A performance note for an earlier version of this code, which may or
	// may not still be relevant: "This operation takes about 4
	// milliseconds on emk's 800MHz laptop.  If we combine this operation
	// with a memory-to-memory blit, the pair of blits require about 14
	// milliseconds.  These numbers were measured under wxWindows 2.3.3 and
	// Win98 with an Intel i815em graphics card."
    wxClientDC client_dc(this);
	client_dc.DrawBitmap(mOffscreenFadePixmap, 0, 0, false);
}
