// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef DrawingArea_H
#define DrawingArea_H

#include "GraphicsTools.h"

class Stage;

class DrawingArea : public GraphicsTools::Image {
    Stage *mStage;
    wxBitmap mPixmap;

    //////////
    // Invalidate the entire stage.
    //
    void InvalidateStage();

	//////////
	// Invalidate the specified rectangle.
	//	
	void InvalidateRect(const wxRect &inRect);

public:
    DrawingArea(Stage *inStage, int inWidth, int inHeight, int inDepth)
	: mStage(inStage), mPixmap(inWidth, inHeight, inDepth) { }

    wxBitmap &GetPixmap() { return mPixmap; }

    //////////
    // Clear the drawing area to the specified color.
    //
    void Clear(const wxColor &inColor);

	//////////
	// Draw a line in the specified color.
	//
	void DrawLine(const wxPoint &inFrom, const wxPoint &inTo,
				  const wxColour &inColor, int inWidth);

	//////////
	// Fill in the specified box with the specified color.
	//
	void FillBox(const wxRect &inBounds, 
				 const GraphicsTools::Color &inColor);

	//////////
	// An optimized version of fill box for the case when the color
	// has a non-opaque alpha channel. Needs to be a separate function
	// so it can be in a separate, optimized file.
	//
	void FillBoxAlpha(const wxRect &inBounds,
					  const GraphicsTools::Color &inColor);

	//////////
	// Fill in the specified box with the specified color.
	//
	void OutlineBox(const wxRect &inBounds, const wxColour &inColor,
					int inWidth);

	//////////
	// Draw a portable PixMap to the screen, blending alpha
	// values appropriately.
	//
	// [in] inPoint - The location at which to draw the pixmap.
	// [in] inPixMap - The pixmap to draw.
	//
	void DrawPixMap(GraphicsTools::Point inPoint,
					GraphicsTools::PixMap &inPixMap);

    //////////
    // Draw a bitmap on the stage at the specified location.
	//
	// [in] inBitmap - The bitmap to draw.
	// [in] inX - The X coordinate to draw it at.
	// [in] inY - The Y coordinate to draw it at.
	// [in_optional] inTransparent - Should we honor transparency information
	//                               in the bitmap?
    //
    void DrawBitmap(const wxBitmap &inBitmap, wxCoord inX, wxCoord inY,
					bool inTransparent = true);

	//////////
	// Blit the contents of the specified DC to our offscreen buffer.
	// If the blit fails, fill the offscreen buffer with black.
	// (This is currently used for synchronizing our display with
	// whatever Quake 2 left on the screen.)
	// 
	// [in] inDC - a DC the same size as the stage
	//
	void DrawDCContents(wxDC &inDC);
};

#endif // DrawingArea_H
