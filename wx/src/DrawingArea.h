// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef DrawingArea_H
#define DrawingArea_H


class Stage;

class DrawingArea : public GraphicsTools::Image {
    Stage *mStage;
	wxRect mBounds;
    wxBitmap mPixmap;

	void InitializePixmap(bool inHasAlpha);

	//////////
	// Invalidate the specified rectangle.
	//
	// [in] inRect - The rectangle to invalidate.
	// [in] inInflate - The number of pixels by which we should inflate
	// 		            the rectangle.
	//	
	void InvalidateRect(const wxRect &inRect, int inInflate = 0);

    //////////
    // Invalidate everything associated with this drawing area.
    //
    void InvalidateDrawingArea();

public:
    DrawingArea(Stage *inStage, int inWidth, int inHeight, bool inHasAlpha);
	DrawingArea(Stage *inStage, const wxRect &inBounds, bool inHasAlpha);
    ~DrawingArea();
    
    wxBitmap &GetPixmap() { return mPixmap; }
	wxRect GetBounds() { return mBounds; }
	bool HasAlpha() { return mPixmap.HasAlpha(); }

    //////////
    // Relocate this drawing area to the specified location.
    //
    void MoveTo(const wxPoint &inPoint);

    //////////
    // Clear the drawing area to the default color.
    //
    void Clear();

    //////////
    // Clear the drawing area to the specified color.
    //
    void Clear(const GraphicsTools::Color &inColor);

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

	//////////
	// Get the color at the specified location (specified in DrawingArea
	// co-ordinates).
	//
	GraphicsTools::Color GetPixel(wxCoord inX, wxCoord inY);

	//////////
	// Composite our data into the specified DC.
	//
	// [in] inDC - The compositing DC.
	// [in] inClipRect - The rectangle (in terms of inDC co-ordinates)
	//                   which we're updating.
	//
	void CompositeInto(wxDC &inDC, const wxRect &inClipRect);
};

#endif // DrawingArea_H
