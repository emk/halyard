//////////////////////////////////////////////////////////////////////////////
//
//   (c) Copyright 1999, Trustees of Dartmouth College, All rights reserved.
//        Interactive Media Lab, Dartmouth Medical School
//
//			$Author$
//          $Date$
//          $Revision$
//
//////////////////////////////////////////////////////////////////////////////

#if !defined (_QTGraphic_h_)
#define _QTGraphic_h_

/*-----------------------------------------------------------------

CLASS
    QTGraphic

	Encapsulates a Quicktime graphic and handles creating them from
	a file and drawing them to the screen.

AUTHOR
    Chuck Officer

-----------------------------------------------------------------*/
class QTGraphic : public TObject
{
public:
	//////////
	// Constructor.  A call to Create() needs to be made to import
	// the graphic from a file.
	//
	QTGraphic();
	
	//////////
	// Constructor.  Calls Create() with the given path.
	//
	// [in] inPath - full path to the Quicktime graphic file
	//
	QTGraphic(TString &inPath);
	
	//////////
	// Destructor.
	//
	~QTGraphic();

	//////////
	// Create a Quicktime graphic from a file.
	//
	// [in] inPath - full path to the Quicktime graphic file
	//
	void		Create(TString &inPath);

	//////////
	// Draw the graphic at the given location.
	//
	// [in] inGWorld - Quicktime graphical world pointer
	// [in] inPt - topleft corner of the rectangle where the graphic should 
	//			   be drawn on the screen
	// [in_optional] inTrans - transparent image? (default false)
	//
	void		Draw(GWorldPtr inGWorld, POINT *inPt, bool inTrans = false);
	
	//////////
	// Draw the graphic at the given location.
	//
	// [in] inGWorld - Quicktime graphical world pointer
	// [in] inRect - Win32 RECT specify where the graphic should be drawn on the screen
	//
	void		Draw(GWorldPtr inGWorld, RECT *inRect);
	
	//////////
	// Get width.
	//
	// [out] return - the width of the graphic
	//
	long		Width(void);
	
	//////////
	// Get height.
	//
	// [out] return - the height of the graphic
	//
	long		Height(void);
	
	//////////
	// Get bit count.
	//
	// [out] return  - the bit count of the graphic
	//
	long		BitCount(void);
	
	//////////
	// Get Quicktime color table handle.
	//
	// [out] return - Quicktime color table handle for the graphic
	//
	CTabHandle	GetColorTable(void);
	
	//////////
	// Get palette for the graphic.
	//
	// [in_optional] inCTab - Quicktime color table for the graphics (default NULL)
	// [out] return - Win32 palette handle for the graphics's palette
	//
	HPALETTE	GetPalette(CTabHandle inCTab = NULL);

	//////////
	// Do we have a graphic?
	//
	// [out] - true if we have a graphic, false otherwise
	//
	inline bool	HaveGraphic(void)
	{ 
		if (m_gi != NULL) 
			return (true); 
		else 
			return (false); 
	}

	//////////
	// Do we have graphic information?
	//
	// [out] - true if we have a graphic info, false otherwise
	//
	inline bool HaveInfo(void)
	{ 
		if (m_idh != NULL) 
			return (true); 
		else 
			return (false); 
	}

protected:
	//////////
	// Initialize.
	//
	void		Init(void);
	
	//////////
	// Dispose of this graphic.
	//
	void		Toss(void);
	
	//////////
	// Setup Quicktime graphics world.
	//
	// [in] inGWorld - Quicktime graphics world 
	// [out] return - true on success, false if there was an error
	//
	bool		SetQTGWorld(GWorldPtr inGWorld);
	
	//////////
	// Make this graphic transparent.
	//
	// [out] return - true on success, false if there was an error
	//
	bool		SetTransparent(void);
	
	//////////
	// Set destination rectangle for this graphic.
	//
	// [in] inRect - Quicktime Rect specifying desired destination
	// [out] return - true on success, false if there was an error
	//
	bool		SetDestRect(Rect *inRect);
	
	//////////
	// Do we need to clip the graphic?
	//
	// [in] inRect - Win32 rectangle for this graphic
	// [out] return - true if the graphic needs to be clipped, false otherwise
	//
	bool		NeedClip(RECT *inRect);

	//////////
	// Quicktime graphics importer.
	//
	GraphicsImportComponent	m_gi;
	
	//////////
	// Quicktime image description.
	//
	ImageDescriptionHandle	m_idh;
	
	//////////
	// Full path to the Quicktime graphic file.
	//
	TString					m_path;
	
	//////////
	// Is the graphic transparent?
	//
	bool					m_transparent;
};

#endif // _QTGraphic_h_

/*
 $Log$
 Revision 1.2  2002/07/23 21:53:53  emk
 3.3.17 - 23 July 2002 - emk

   * Fixed RETURN in macros (bug #1053).
   * Fixed typography exception when missing buttpcx graphic (bug #1039).
   * Made Win32 BROWSE return an error if it fails (bug #793).
   * Forward-ported QtComponentVersion to Win32 (bug #1054).
   * Performance tuned Win32 textaa (bug #933).

 Revision 1.1  2001/09/24 15:11:01  tvw
 FiveL v3.00 Build 10

 First commit of /iml/FiveL/Release branch.

 There are now seperate branches for development and release
 codebases.

 Development - /iml/FiveL/Dev
 Release - /iml/FiveL/Release

 Revision 1.5  2000/04/07 17:05:16  chuck
 v 2.01 build 1

 Revision 1.4  2000/03/01 15:46:55  chuck
 no message

 Revision 1.3  2000/02/02 15:15:32  chuck
 no message

 Revision 1.2  1999/09/24 19:57:19  chuck
 Initial revision

*/
