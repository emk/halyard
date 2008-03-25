// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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

#ifndef DrawingContextStack_H
#define DrawingContextStack_H

class Stage;
class Element;
typedef shared_ptr<Element> ElementPtr;
class DrawingArea;

//////////
/// This is an ad-hoc utility class for keeping track of which DrawingArea
/// we're supposed to be using.  It's tightly-coupled to Stage, but split
/// into a separate file to keep Stage as small as possible.
///
class DrawingContextStack {
    typedef std::vector<ElementPtr> ElementStack;

	//////////
	/// The stage we're associated with.  This provides our default
	/// DrawingArea when we have nothing else.
	///
	Stage *mStage;

    //////////
    /// A stack of currently-active drawing contexts.
    ///
    ElementStack mDrawingContextStack;

public:
    DrawingContextStack(Stage *inStage) : mStage(inStage) {}

	bool IsEmpty() { return mDrawingContextStack.empty(); }
	bool ContainsElement(ElementPtr inElement);

	DrawingArea *GetCurrentDrawingArea();

	void PushDrawingContext(ElementPtr inElement);
	void PopDrawingContext(ElementPtr inElement);
};

#endif // DrawingContextStack_H
