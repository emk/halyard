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
