// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef DrawingContextStack_H
#define DrawingContextStack_H

class Stage;
class Element;
typedef shared_ptr<Element> ElementPtr;
class DrawingArea;

//////////
// This is an ad-hoc utility class for keeping track of which DrawingArea
// we're supposed to be using.  It's tightly-coupled to Stage, but split
// into a separate file to keep Stage as small as possible.
//
class DrawingContextStack {
    typedef std::vector<ElementPtr> ElementStack;

	//////////
	// The stage we're associated with.  This provides our default
	// DrawingArea when we have nothing else.
	//
	Stage *mStage;

    //////////
    // A stack of currently-active drawing contexts.
    //
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
