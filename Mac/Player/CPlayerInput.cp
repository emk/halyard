/* =================================================================================
	CPlayerInput.cp	
   ================================================================================= */

#include "THeader.h"
#include "MacCarbonGlue.h"

#include <iostream>
#include <Palettes.h>
#include <LStream.h>
#include <UDrawingState.h>

#include "TLogger.h"

#include "CMac5LApp.h"
#include "CPlayerView.h"
#include "CPlayerInput.h"
#include "CPlayerText.h"
#include "TVariable.h"
#include "CHeader.h"
#include "UModalDialogs.h"
#include "UKeyFilters.h"

USING_NAMESPACE_FIVEL
using namespace PowerPlant;

static CPlayerInput *theInputThing = nil;
extern WindowPtr FiveL::gWindow;

/* ---------------------------------------------------------------------------------
		¥ CPlayerInput
		
		Constructor for a custom Player Pane class. Sets up a SPaneInfo struct to
		call the custom pane's constructor.
   --------------------------------------------------------------------------------- */

CPlayerInput::CPlayerInput(
	TString inVarName,
	TString	inStyle,
	TString	inMask,
	Rect	inBounds,
	bool	inRequired)
{
	int16	height, width;
	
	// Set the frame size, the pane's superview, and position within the superview.
	height = inBounds.bottom - inBounds.top;
	width = inBounds.right - inBounds.left;
	ResizeFrameTo(width, height, false);
	PutInside(gPlayerView, false);
	PlaceInSuperFrameAt(inBounds.left, inBounds.top, false);
	SetSuperCommander(gTheApp);

	// Skanky hack to set pane ID
	LArray &paneList = gPlayerView->GetSubPanes();
	SetPaneID((paneList.GetCount()) + 2000);

	mHaveBackColor = false;			// assume we won't get it
	
	// Fill in the Text Traits resource and write it back out.
	if (not inStyle.IsEmpty())
	{
		PaletteHandle		thePal = NULL;
		CHeader				*theHeader = NULL;
		TextTraitsH			tthand;
		
		theHeader = (CHeader *) gHeaderManager.Find(inStyle.GetString());
		if (theHeader != NULL)
		{
			tthand = (TextTraitsH) ::Get1Resource('Txtr', 1000);
			if (tthand != NULL)
			{
				// write the header information out to the Txtr resource which
				// is used to set text traits
				(*tthand)->size = theHeader->GetHeight();
				(*tthand)->style = theHeader->GetBold() ? bold : 0;
				(*tthand)->justification = theHeader->GetAlignment();
				(*tthand)->mode = srcCopy;
				thePal = ::GetPalette(gWindow);
				::GetEntryColor( thePal, theHeader->GetColor(), &((*tthand)->color)); 
				(*tthand)->fontNumber = theHeader->GetFontFamily();
				(*tthand)->fontName[0] = 0;
							
				::ChangedResource((Handle) tthand);
			}
			
			if (theHeader->GetHighlightColor() != 0)
			{
				if (thePal == NULL)
					thePal = ::GetPalette(gWindow);
					
				::GetEntryColor(thePal, theHeader->GetHighlightColor(), &mBackColor);
				mHaveBackColor = true;
			}
		}
	}

	// Assign our data.
	mVarToSet 	= inVarName;
	mStyle		= inStyle;
	mMask		= inMask;
	mRequired	= inRequired;
	
	mHasBox = false;				// don't frame the input area
	
	// Figure out what kind of input is expected, and the max length.
	SetMaxChars(mMask.Length());
	
	// We don't use the predefined filter functions here because we 
	if (mMask(0) == 'N')			// We want any alphanumeric input
		SetKeyFilter(&UKeyFilters::AlphaNumericField);
	else if (mMask(0) == '9')		// We want any numeric input
		SetKeyFilter(&UKeyFilters::IntegerField);
				
	FinishCreate();
}

// ---------------------------------------------------------------------------------
//		¥ ~CPlayerInput
// ---------------------------------------------------------------------------------

CPlayerInput::~CPlayerInput()
{
}

/* ---------------------------------------------------------------------------------
 		¥ FinishCreateSelf
 		
 		Call this once the object is created. You can use it to take care of any
 		outstanding initialization issues.
   --------------------------------------------------------------------------------- */

void CPlayerInput::FinishCreateSelf()
{

	SetTextTraitsID(1000);		// Read in our text traits information.
	
	//if (mMask.length() != 0)
	//{
	//	int16 	width;
	//	
	//	//width = ::CharWidth('N');
	//	width = 13;
	//	
	//	ResizeFrameTo((width * mMask.length()) + 10, 18, false);
	//}
	
	AlignTextEditRects();		// This needs to be done for some reason
		
	SwitchTarget(this);
	SelectAll();	
}


// ---------------------------------------------------------------------------
//		¥ HandleKeyPress
// ---------------------------------------------------------------------------
//	Handle key stroke directed at an EditField
//
//	Return true if the EditField handles the keystroke

Boolean CPlayerInput::HandleKeyPress(
	const EventRecord&	inKeyEvent)
{
	Boolean		keyHandled = true;
	EKeyStatus	theKeyStatus = keyStatus_Input;
	char		theKey = inKeyEvent.message & charCodeMask;
	
	if (inKeyEvent.modifiers & cmdKey)
	{	// Always pass up when the command
		theKeyStatus = keyStatus_PassUp;	//   key is down
	
	}
	else if (mKeyFilter != nil)
	{
	// cbo_fix - change this
	//	theKeyStatus = (*mKeyFilter)(inKeyEvent);
	}
	
	StFocusAndClipIfHidden	focus(this);
	
	switch (theKeyStatus)
	{
		case keyStatus_Input:
			Str255	theString;
			Rect	macBounds;
			
			// Check for 'enter' or 'return' - get the text, kill the TE field,
			// and re-display text in the approriate style.
			
			if ((theKey == RETURN_CHAR) or (theKey == NEWLINE_CHAR) or (theKey == '\t') or (theKey == '\3'))
			{	
				// Get the string that's in the field (it's a pstr)
				GetDescriptor(theString);

				// Check to see if it has at least one char. If not, beep & keep going
				if (theString[0] != 0)
				{
					// Set the appropriate var
					gVariableManager.SetString((const char *) mVarToSet, p2cstr(theString));

					gDebugLog.Log("input: variable <%s>, value <%s>", (const char *) mVarToSet, theString);
					
					// Create a new text field to take its place
					CalcLocalFrameRect(macBounds);
					macBounds.left += 1;
					macBounds.top -= 1;
					FiveL::TRect	bounds;
					bounds.Set(macBounds);
					
	   				new CPlayerText(mStyle, bounds, (char *)theString, 0, 0);
	   				InvalPortRect(&macBounds);
					
					// Nuke the TE field
					delete theInputThing;
					theInputThing = nil;
					
					// And finally wake up the card (that was paused in 'DoInput')
					TInterpreter::GetInstance()->WakeUp();
					gPlayerView->ProcessEvents(true);
				}
				else
					::SysBeep(10);
					
				break;
			}
				
				// Check if we are at the character limit
				// ### Not two-byte char compatible
			if (TooManyCharacters(1))
			{
				SysBeep(1);
				break;
			}
			
			if (mTypingAction == nil)
			{
				mTypingAction = new LTETypingAction(mTextEditH, this, this);
				PostAction(mTypingAction);
			}
			if (mTypingAction != nil) 
			{
			// cbo_fix - doesn't work
				//mTypingAction->InputCharacter(theKey);
			} 
			else 
			{
				::TEKey(theKey, mTextEditH);
			}
			UserChangedText();
			break;
			
		case keyStatus_TEDelete:
			if ((**mTextEditH).selEnd > 0)
			{
				if (mTypingAction == nil)
				{
					mTypingAction = new LTETypingAction(mTextEditH, this, this);
					PostAction(mTypingAction);
				}
				if (mTypingAction != nil)
					mTypingAction->BackwardErase();
				else
					::TEKey(char_Backspace, mTextEditH);

				UserChangedText();
			}
			break;
			
		case keyStatus_TECursor:
			::TEKey(theKey, mTextEditH);
			break;
			
		case keyStatus_ExtraEdit:
			switch (theKey)
			{
				case char_Home:
					::TESetSelect(0, 0, mTextEditH);
					break;
					
				case char_End:
					::TESetSelect(max_Int16, max_Int16, mTextEditH);
					break;
					
				case char_FwdDelete:
					if ((**mTextEditH).selStart < (**mTextEditH).teLength)
					{
						if (mTypingAction == nil)
						{
							mTypingAction = new LTETypingAction(mTextEditH, this, this);
							PostAction(mTypingAction);
						}
						if (mTypingAction != nil)
							mTypingAction->ForwardErase();
						else
						{
							if ((**mTextEditH).selStart == (**mTextEditH).selEnd)
							{
								::TESetSelect((**mTextEditH).selStart,
									(**mTextEditH).selStart + 1, mTextEditH);
							}
							
							::TEDelete(mTextEditH);
						}
						UserChangedText();
					}
					break;
					
				default:
					keyHandled = LCommander::HandleKeyPress(inKeyEvent);
					break;
			}
			break;
			
			case keyStatus_Reject:
				// +++ Do something
				SysBeep(1);
				break;
				
			case keyStatus_PassUp:
				keyHandled = LCommander::HandleKeyPress(inKeyEvent);
				break;
	}
	
	return keyHandled;
}

Boolean
CPlayerInput::FocusDraw(LPane*	/* inSubPane */)
{
	Boolean	focused = LEditField::FocusDraw();

	if (mHaveBackColor)
		::RGBBackColor(&mBackColor);
	else
		// now set the background color to black
		::RGBBackColor(&Color_Black);	
	
	return (focused);
}	
	
bool FiveL::HaveInputUp(void)
{
	if (theInputThing != nil)
		return (true);
	else
		return (false);
}

void FiveL::DoCPlayerInput(
	TString inVarName,
	TString	inStyle,
	TString	inMask,
	Rect	inBounds,
	bool	inRequired)
{
	if (theInputThing != nil)
		delete theInputThing;
		
	theInputThing = new CPlayerInput(inVarName, inStyle, inMask, inBounds, inRequired);
}


