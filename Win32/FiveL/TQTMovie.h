// -*- Mode: C++; tab-width: 4; -*-

#ifndef TQTMovie_H
#define TQTMovie_H

#include <windows.h>

#include <string>
#include <stdexcept>

#include <Movies.h>

// TODO - Port this code to our modern code base.
//BEGIN_NAMESPACE_FIVEL

//////////
// This exception class represents a MacOS error.  Note that QuickTime
// for Windows uses MacOS-style errors.
//
// TODO - Port this code to be a subclass of TException.
//
class TMacError : public std::runtime_error
{
	std::string mFile;
	int mLine;
	OSErr mErrorCode;

public:
	//////////
	// Create a Macintosh exception from an OSErr.
	//
	TMacError(const char *inFile, int inLine, OSErr inErrorCode);
	
	//////////
	// If inErrCode does not equal noErr, raise an error.
	//
	static void Check(const char *inFile, int inLine, OSErr inErrorCode);
};

//////////
// Check the result of a MacOS function for an error.
//
// [in] expr - A value of type OSErr.
//
#define CHECK_MAC_ERROR(expr) \
	/*FIVEL_NS*/ TMacError::Check(__FILE__, __LINE__, (expr))


//////////
// This class is intended to be a portable QuickTime movie wrapper
// containing all our low-level interfaces to QuickTime.  We're
// implementing this because we're tired of non-portable,
// duplicate QuickTime code that breaks every time sombody modifies it,
// and every time that Apple updates QuickTime.  Cross your fingers.
//
// Please design this class for maximum paranoia.  Use *lots* of
// assertions, and code defensively.
//
class TQTMovie
{
private:
	static bool sIsQuickTimeInitialized;

public:
	static void InitializeMovies();
	static void ShutDownMovies();
	static void PrepareWindowForMovies(HWND inWindow);
	static CGrafPtr GetPortFromHWND(HWND inWindow);

private:
    //////////
    // The movie we're playing.
    //
    Movie mMovie;

    //////////
    // We always wrap our movie in a MovieController, even if
    // we chose not to show the controller bar.  The
    // MovieController does a ton of work for us.  Furthermore,
    // it *appears* that MovieController-based applications are
    // more reliable (especially in the face of QuickTime updates)
    // than applications which roll all their own QuickTime code.
    // This is, however, merely a hypothesis.
    //
    MovieController mMovieController;

public:
	TQTMovie(const std::string &inMoviePath);
	virtual ~TQTMovie();

	bool HandleMovieEvent(HWND hWnd, UINT message, WPARAM wParam,
						  LPARAM lParam);
	void Redraw(HWND hWnd);
};

//END_NAMESPACE_FIVEL

#endif // TQTMovie_H
