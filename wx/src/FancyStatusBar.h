// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef FancyStatusBar_H
#define FancyStatusBar_H

class ProgressMeter;

class FancyStatusBar : public wxStatusBar
{
	ProgressMeter *mProgressMeter;
	
    void OnSize(wxSizeEvent& event);

public:
	FancyStatusBar(wxWindow *inParent);

	static wxColour DEFAULT_PROGRESS_COLOR;

	void SetProgressColor(wxColour &inColor);
	void SetProgress(float inValue);

    DECLARE_EVENT_TABLE()
};

#endif // FancyStatusBar_H
