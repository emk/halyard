// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
//
// This class implements a browser-style "Location:" box which can be used
// to jump between cards.  We implement it slightly differently on different
// platforms because we occasionally need to abuse the semantics of the
// underlying native control (or to abuse the wxWindows window class wrapping
// that control).

#ifndef LocationBox_H
#define LocationBox_H

#include "AppConfig.h"

#if CONFIG_LOCATION_BOX_IS_COMBO
#	define LOCATION_BOX_PARENT_CLASS wxComboBox
#else
#	define LOCATION_BOX_PARENT_CLASS wxTextCtrl
#endif

class LocationBox : public LOCATION_BOX_PARENT_CLASS
{
public:
	LocationBox(wxToolBar *inParent);
	
	void NotifyEnterCard(const wxString &inName);

	void RegisterCard(const wxString &inCardName);
	void TryJump(const wxString &inCardName);

	void UpdateUiLocationBox(wxUpdateUIEvent &inEvent);
	void OnChar(wxKeyEvent &inEvent);

#if CONFIG_LOCATION_BOX_IS_COMBO
	void OnComboBoxSelected(wxCommandEvent &inEvent);
#endif

	//////////
	// Call this function to focus the location box and prepare for the
	// user to enter a card name.
	//
	void Prompt();

	DECLARE_EVENT_TABLE();
};

#endif // LocationBox_H
