// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef Log5L_H
#define Log5L_H


//////////
// This is a wxWindows log target which knows how to write messages to
// the traditional 5L logging subsystem.
//
class Log5L : public wxLog
{
public:
	Log5L();
	
	virtual void DoLog(wxLogLevel inLevel, const wxChar *inMsg,
					   time_t inTimeStamp);
};

#endif // Log5L_H
