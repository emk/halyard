// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2006 Trustees of Dartmouth College
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

#ifndef DrawingArea_H
#define DrawingArea_H

class Stage;
class wxQuake2Overlay;

/// An object which can be drawn to by scripts.
class DrawingArea : public GraphicsTools::Image {
    Stage *mStage;
	wxRect mBounds;
    wxBitmap mPixmap;
    bool mIsShown;
    shared_ptr<wxQuake2Overlay> mQuake2Overlay;

    //////////
    /// Returns true if this DrawingArea has an area of zero.
    ///
    bool HasAreaOfZero() const;

    //////////
    /// Initialize our underlying pixmap.
    ///
	void InitializePixmap(bool inHasAlpha);

    ///////////
    /// Initialize the Quake 2 overlay object associated with this drawing
    /// area.  We'll only call this function if we are floating this
    /// DrawingArea over Quake 2.
    ///
    void InitializeQuake2Overlay();

	//////////
	/// Invalidate the specified rectangle.
	///
	/// \param inRect  The rectangle to invalidate.
	/// \param inInflate  The number of pixels by which we should inflate
	/// 		         the rectangle.
    /// \param inHasPixmapChanged  If false, the contents of this rect haven't
    ///               changed, just the stage's alpha-compositing for this
    ///               region.  If we're relying on game engine for real-time
    ///               compositing, it doesn't need to reconvert the data in
    ///               this rectangle.
	///	
	void InvalidateRect(const wxRect &inRect, int inInflate = 0,
                        bool inHasPixmapChanged = true);

    //////////
    /// Invalidate everything associated with this drawing area.
    ///
    void InvalidateDrawingArea(bool inHasPixmapChanged = true);

public:
    DrawingArea(Stage *inStage, int inWidth, int inHeight, bool inHasAlpha);
	DrawingArea(Stage *inStage, const wxRect &inBounds, bool inHasAlpha);
    ~DrawingArea();
    
    /// Set the size of this DrawingArea.  Erases all contents.
    void SetSize(const wxSize &inSize);

    wxBitmap &GetPixmap() { return mPixmap; }
	wxRect GetBounds() { return mBounds; }
	bool HasAlpha() { return mPixmap.HasAlpha(); }

    //////////
    /// Show or hide this drawing area.
    ///
    void Show(bool inShow);

    //////////
    /// Relocate this drawing area to the specified location.
    ///
    void MoveTo(const wxPoint &inPoint);

    //////////
    /// Add this drawing area's bounding box to the dirty list.
    ///
    void InvalidateCompositing() { InvalidateDrawingArea(false); }

    //////////
    /// Clear the drawing area to the default color.
    ///
    void Clear();

    //////////
    /// Clear the drawing area to the specified color.
    ///
    void Clear(const GraphicsTools::Color &inColor);

	//////////
	/// Draw a line in the specified color.
	///
	void DrawLine(const wxPoint &inFrom, const wxPoint &inTo,
				  const GraphicsTools::Color &inColor, int inWidth);

	//////////
	/// Fill in the specified box with the specified color.
	///
	void FillBox(const wxRect &inBounds, 
				 const GraphicsTools::Color &inColor);

	//////////
	/// Fill in the specified box with the specified color.
	///
	void OutlineBox(const wxRect &inBounds,
                    const GraphicsTools::Color &inColor,
					int inWidth);

	//////////
	/// Draw a portable PixMap to the screen, blending alpha
	/// values appropriately.
	///
	/// \param inPoint  The location at which to draw the pixmap.
	/// \param inPixMap  The pixmap to draw.
	///
	void DrawPixMap(GraphicsTools::Point inPoint,
					GraphicsTools::PixMap &inPixMap);

    //////////
    /// Draw a bitmap on the stage at the specified location.
	///
	/// \param inBitmap  The bitmap to draw.
	/// \param inX  The X coordinate to draw it at.
	/// \param inY  The Y coordinate to draw it at.
	/// [in_optional] inTransparent - Should we honor transparency information
	///                               in the bitmap?
    ///
    void DrawBitmap(const wxBitmap &inBitmap, wxCoord inX, wxCoord inY,
					bool inTransparent = true);

    //////////
    /// Use the alpha channel of a bitmap to selectively mask parts of the
    /// DrawingArea.  Completely opaque parts of the "mask" bitmap will be
    /// left unchanged in the DrawingArea, while completely transparent
    /// parts of the mask will be erased from the DrawingArea.
    /// Transparency values in between will have a proportional effect.
    ///
    /// \param inMask  The bitmap to use a mask.
    /// \param inX The X coordinate to erase at.
    /// \param inY The Y coordinate to erase at.
    ///
    void Mask(const wxBitmap &inMask, wxCoord inX, wxCoord inY);

	//////////
	/// Blit the contents of the specified DC to our offscreen buffer.
	/// If the blit fails, fill the offscreen buffer with black.
	/// (This is currently used for synchronizing our display with
	/// whatever Quake 2 left on the screen.)
	/// 
	/// \param inDC  a DC the same size as the stage
	///
	void DrawDCContents(wxDC &inDC);

	//////////
	/// Get the color at the specified location (specified in DrawingArea
	/// co-ordinates).
	///
	GraphicsTools::Color GetPixel(wxCoord inX, wxCoord inY);

	//////////
	/// Composite our data into the specified DC.
	///
	/// \param inDC  The compositing DC.
	/// \param inClipRect  The rectangle (in terms of inDC co-ordinates)
	///                   which we're updating.
	///
	void CompositeInto(wxDC &inDC, const wxRect &inClipRect);
};

#endif // DrawingArea_H
