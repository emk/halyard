// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef DrawingAreaOpt_H
#define DrawingAreaOpt_H

#include <wx/rawbmp.h>

// These functions require raw bitmap access because they work with
// alpha channels.  Most of them are parameterized for PixelData type
// so they can work with both wxNativePixelData and wxAlphaPixelData.

template <class PixelData> extern
void DrawPixMapOpt(PixelData &inDstData,
				   GraphicsTools::Point inPoint,
				   GraphicsTools::PixMap &inPixMap);

template <class PixelData> extern
void FillBoxOpt(PixelData &inDstData,
				const wxRect &inBounds,
				const GraphicsTools::Color &inColor);

extern
void ClearOpt(wxAlphaPixelData &inDstData,
			  const GraphicsTools::Color &inColor);

#endif // DrawingAreaOpt_H
