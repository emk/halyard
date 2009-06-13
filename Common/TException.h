// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
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

#ifndef TException_H
#define TException_H

BEGIN_NAMESPACE_HALYARD

/// Crashes can be associated with either the application or the script
/// we're running.  This enumeration is defined here beacause it is
/// exception-related, and so it may be used without pulling in all of
/// CrashReporter.
///
/// @see CrashReporter
enum CrashType {
    APPLICATION_CRASH,
    SCRIPT_CRASH
};

/// A handy exception class for use in Halyard code.  Don't bother
/// catching this directly; catch something like std::exception
/// instead.
class TException : public std::runtime_error {
    std::string mErrorFile;
    int mErrorLine;
    std::string mErrorMessage;
    int mErrorCode;
    std::string mWhatCache;
    
protected:
    /// Constructor for use by subclasses.  You can call this, then set up
    /// the member variables with SetErrorCode and SetErrorMessage.
    TException(const char *inErrorFile, int inErrorLine)
        : std::runtime_error(""),
          mErrorFile(inErrorFile),
          mErrorLine(inErrorLine),
          mErrorCode(kNoErrorCode) {}

    /// Set the error code associated with this exception.
    void SetErrorCode(int inErrorCode) { mErrorCode = inErrorCode; }

    /// Set the error message associated with this exception.
    void SetErrorMessage(const std::string &inErrorMessage)
        { mErrorMessage = inErrorMessage; }

public:
    enum {
        /// Not all errors have error codes.  Use this if you don't
        /// have anything better.
        kNoErrorCode = 0
    };

    /// Create a new TException object.
    ///
    /// \param inErrorMessage  The error message to display.
    /// \param inErrorCode  The code associated with this error, if any.
    TException(const char *inErrorFile, int inErrorLine,
               const std::string &inErrorMessage,
               int inErrorCode = kNoErrorCode)
        : std::runtime_error(""),
          mErrorFile(inErrorFile),
          mErrorLine(inErrorLine),
          mErrorMessage(inErrorMessage),
          mErrorCode(inErrorCode) {}

    /// Destroy a TException object.
    virtual ~TException() throw () {}

    /// Return the name of this exception class.  Subclasses must
    /// override this to provide their own name.
    virtual const char *GetClassName() const { return "TException"; }

    /// Return the error code associated with this exception, or
    /// kNoErrorCode if there is none.
    int GetErrorCode() const { return mErrorCode; }
    
    /// Return the error message associated with this exception.  You
    /// don't usually wish to call this; call what() instead.
    std::string GetErrorMessage() const { return mErrorMessage; }

    std::string GetErrorFile() const { return mErrorFile; }
    int GetErrorLine() const { return mErrorLine; }
    
    /// Return a formatted error string for this error.  This method is
    /// available on all instances of std::exception.
    ///
    /// XXX - THIS ROUTINE VIOLATES CONST-CORRECTNESS TO UPDATE THE INTERNAL
    /// mWhatCache VARIABLE.  This nasty wart is forced upon us by the fact
    /// that std::exception declares what() to return a const char*, not a
    /// proper string.  We could fix this by caching values in our Set*
    /// methods.
    ///
    /// \return  A pointer to an error string.  This string
    ///                is good until the TException is destroyed,
    ///                or what() is called a second time, whichever
    ///                comes sooner.
    virtual const char* what () const throw ();

    /// Report an exception to the user.  Since these tend to be well-behaved
    /// C++ exceptions, this function is non-fatal.
    static void ReportException(std::exception &e);

    /// Report an unknown exception to the user.  Since these tend to be
    /// Win32 protection violations, this function is fatal.
    static void ReportException();

    /// Report an exception, and always trigger a fatal error.
    static void ReportFatalException(std::exception &e);

    /// Report an unknown exception, and always trigger a fatal error.
    static void ReportFatalException();
};

#define BEGIN_EXCEPTION_TRAPPER() \
    try {

#define END_EXCEPTION_TRAPPER(REPORT_FUNC) \
    } catch (std::exception &e) { \
        REPORT_FUNC(e); \
    } catch (...) { \
        REPORT_FUNC(); \
    }

#define CATCH_ALL_EXCEPTIONS_AND_RETURN(FUNC,DEFAULT) \
    try { \
        return (FUNC); \
    } catch (std::exception &e) { \
        gLog.FatalError("Unexpected internal error: " + \
                               std::string(e.what())); \
    } catch (...) { \
        gLog.FatalError("Unexpected, unknown internal error."); \
    } \
    return DEFAULT;

#define THROW(MSG) \
    throw TException(__FILE__, __LINE__, (MSG))

#define CHECK(COND,MSG) \
    do { if (!(COND)) THROW(MSG); } while (0)

END_NAMESPACE_HALYARD

#endif // TException_H
