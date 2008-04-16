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

#if !defined (_TInterpreter_h_)
#define _TInterpreter_h_

BEGIN_NAMESPACE_HALYARD

class TValue;
class ScriptEditorDB;
class Document;

//////////
/// An identifier which might appear in a script.  Used to inform the editor
/// of highlightable keywords, autocompletable names, etc.
///
class TScriptIdentifier {
public:
    /// The type of an identifier.
    enum Type {
        KEYWORD,  //< For Scheme, includes a macro or special form.
        FUNCTION,
        VARIABLE,
        CONSTANT,
        CLASS,
        TEMPLATE,
        GROUP,
        SEQUENCE,
        CARD,
        ELEMENT,
        UNKNOWN
    };

private:
	std::string mName;
    Type mType;
    int mIndentHint;

public:
	TScriptIdentifier(std::string inName, Type inType, int inIndentHint = 0)
        : mName(inName), mType(inType), mIndentHint(inIndentHint) {}

    /// Get the name of this identifier.
	std::string GetName() const { return mName; }

    /// Get the type of this identifier.
    Type GetType() const { return mType; }

    /// Get the indentation hint for this identifier.  The meaning of this
    /// value is scripting-language-specific.
    int GetIndentHint() const { return mIndentHint; }

    /// Return true if two identifiers are equal.  (For use with STL.)
    bool operator==(const TScriptIdentifier &right) const {
        return mName == right.mName;
    }

    /// Return true if the first identifier is less than the second.  (For
    /// use with STL.)
    bool operator<(const TScriptIdentifier &right) const {
        return mName < right.mName;
    }
};


//////////
/// TInterpreter provides an abstract interface to a programming language
/// interpreter used by the Halyard engine.  In theory, it should be possible
/// to change Halyard's scripting language by providing a new implementation
/// of this class.
///
class TInterpreter : boost::noncopyable
{
public:
	//////////
	/// Platform-specific idle procedures.  These procedures can be
	/// called from within the interpreter's idle loop to process
	/// GUI events, give time to movie playback libraries, etc.
	///
	/// \param block  true if we want the system to block until user
	///              events are received
	///
	typedef void (*SystemIdleProc)(bool inBlock);

	//////////
	/// Create a new TInterpreter object.  TInterpreter is a singleton
	/// class; only one such object may be created (at any given time).
	/// Call GetInstance (below) to return that instance.
	///
	TInterpreter();

	//////////
	/// Our destructor is virtual to make sure the destructors of
	/// our child classes get called.
	///
	virtual ~TInterpreter();

	//////////
	/// Enter the interpreter's main loop, and don't come back until the
	/// interpreter has finished.
	///
	/// \param inIdleProc  This routine must be called periodically by
	/// 					 the interpreter to give the system time for
	///					 event processing and other tasks.
	///
	virtual void Run(SystemIdleProc inIdleProc) = 0;

	//////////
	/// Request the immediate shutdown of the interpreter.  This may happen
	/// for a variety of reasons: the application has finished, the user has
	/// requested a reloadscript, etc.
	///
	/// This function is called from within the SystemIdleProc or a
	/// primitive.
	///
	virtual void KillInterpreter() = 0;

	//////////
	/// Stop the interpreter so the graphical editor may run.  This
	/// exits the current card.  Use Go() to resume running
	/// the interpreter.
	///
	virtual void Stop() = 0;

	//////////
	/// Returns true if the interpreter is stopped.
	///
	virtual bool IsStopped() = 0;

	//////////
	/// Resume from a Stop() and jump to the specified card.
	///
	virtual void Go(const char *inCard) = 0;

    //////////
    /// Can the interepreter suspend in its current context?  Call this
    /// before calling Pause to prevent errors.
    ///
    virtual bool CanSuspend() = 0;

	//////////
	/// Pause the CardManager.  Use WakeUp() to resume.
	///
	virtual void Pause(void) = 0;
	
	//////////
	/// Wakeup from a Pause().  This might be called on cards
	/// which aren't paused; check to be certain.
	///
	virtual void WakeUp(void) = 0;
	
	//////////
	/// Is the CardManager paused?
	///
	/// \return  True if paused, false otherwise.
	///
	virtual bool Paused(void) = 0;

	//////////
	/// Permanently stop execution of the current card's script.
	///
	virtual void KillCurrentCard(void) = 0;

	//////////
	/// Jump to a card given its name.
	///
	/// \param inName  name of the Card to jump to.
	///
	virtual void JumpToCardByName(const char *inName) = 0;
	
	//////////
	/// Get name of current Card.
	///
	/// \return  the name of the current card.
	///
	virtual std::string CurCardName(void) = 0;
    	
	//////////
	/// Determine whether a card with the given name exists.
	///
	/// \param inCardName  The name of the card.
	/// \return  true if and only if the card exists.
	///
	virtual bool IsValidCard(const char *inCardName) = 0;

	//////////
	/// Evaluate an expression, returning any errors which occur.
	///
	/// \param inExpression  The expression to evaluate.
	/// \param outResultText  (out) The result of the expression, as a string,
	///                       or an error message.
	/// \return  true if an expression was returned, false if an
	///                error was returned.
	///
	virtual bool Eval(const std::string &inExpression,
					  std::string &outResultText) = 0;

    //////////
    /// Fetch a list of known identifiers from the interpreter.
    ///
	virtual std::vector<TScriptIdentifier> GetKnownIdentifiers() = 0;

    //////////
    /// Called when a file is loaded at application startup.  Used to
    /// implement a progress bar.
    ///
    void NotifyFileLoaded();

    //////////
    /// Called when a file is loaded at application startup.  Used to
    /// implement a progress bar.
    ///
    void NotifyScriptLoaded();

    //////////
    /// Get the fraction of startup files loaded.  Returns a number between
    /// 0.0 and 1.0, inclusive.  Used to implement a progress bar.
    ///
    double GetLoadProgress();
	
	/////////
	/// Run the system idle procedure.
	/// 
	/// \param block  Should the idle procedure block until all events are 
	///               processed, or only process a few events? 
	///
	virtual void DoIdle(bool block) = 0;

	//////////
	/// Do we have a single, global instance of this class?
	///
	static bool HaveInstance() { return sInstance != NULL; }

	//////////
	/// Return the single, global instance of this class.
	///
	static TInterpreter *GetInstance() { ASSERT(sInstance); return sInstance; }

private:
    int mSourceFilesLoaded;
    int mSourceFilesExpected;

	static TInterpreter *sInstance;
};

//////////
/// TCallback represents a "callback" function in the interpreter.  These
/// functions may be called repeatedly.  Destroying the TInterpreter
/// object invalidates all TCallbacks; calling ReDoScript may or may
/// not invalidate any given callback.
///
class TCallback : boost::noncopyable
{
public:
	TCallback() {}
	virtual ~TCallback() {}

	//////////
	/// Execute the callback.
	///
	/// \param inArguments  The argument list to pass to the function.
	///
	virtual TValue Run(const TValueList &inArguments = TValueList()) = 0;

	//////////
	/// Return a form of the callback suitable for printing.  This might not
	/// be very informative, but at least it gives you something for the
	/// debug log.
	///
	virtual std::string PrintableRepresentation() { return "#<callback>"; }
};


//////////
/// Objects of this class are automatically notified when the script is
/// is reloaded.
///
class TReloadNotified {
public:
    TReloadNotified();
    virtual ~TReloadNotified();
    virtual void NotifyReloadScriptStarting() {}
    virtual void NotifyReloadScriptSucceeded() {}
    virtual void NotifyReloadScriptFailed() {}
};


//////////
/// This class is in charge of creating, running and destroying interpreters
/// as required by the application.  It handles such features as redoscript,
/// switchscript, etc.  (These features are implemented by a separate
/// class--instead of being handled by TInterpreter--because the
/// implementation of these features typically involves destroying and
/// creating TInterpreter objects.)
///
/// Only one TInterpreterManager will ever be created, so feel free to
/// install language-specific primitives in the constructor.
///
class TInterpreterManager : boost::noncopyable
{
	static TInterpreterManager *sInstance;
	static bool sHaveAlreadyCreatedSingleton;
    static std::vector<TReloadNotified*> sReloadNotifiedObjects;
    static bool sIsInRuntimeMode;
    static bool sIsFirstLoad;
    static bool sHaveInitialCommand;
    static std::string sInitialCommand;
    static Document *sDocument;

	//////////
	/// We call this procedure to yield time to the system.
	///
	TInterpreter::SystemIdleProc mSystemIdleProc;

	//////////
	/// The TInterpreter object, if we currently have one, or NULL.
	///
	TInterpreter *mInterpreter;

	//////////
	/// This is set to true once the interpreter manager is allowed
	/// to create an interpreter and start it running.
	///
	bool mScriptIsBegun;

	//////////
	/// Should we exit our top-level event loop?
	///
	bool mDone;

	//////////
	/// Did the script exit with an error?
	///
	bool mExitedWithError;

	//////////
	/// Did our last attempt to load a script fail?  If so, we'll want
	/// to refrain from trying to open the script again until the user
	/// asks us to.
	///
	/// We need to keep track of this so that we can run mSystemIdleProc
	/// directly while we're waiting for a script author to call
	/// RequestRetryLoadScript.
	///
	bool mLoadScriptFailed;

	//////////
	/// What card should we jump to when we create a new interpreter.
	///
	std::string mInitialCardName;

public:
	//////////
	/// Create a new TInterpreterManager with the specified idle procedure.
	///
	TInterpreterManager(TInterpreter::SystemIdleProc inIdleProc);

	//////////
	/// Destroy the TInterpreterManager.  (This is a singleton class, so you
	/// won't be able to create a new one.)
	///
	virtual ~TInterpreterManager();

	//////////
	/// Create an interpreter and run it.  This function handles the
	/// application's main event loop, and will only return when the
	/// application quits.
	///
	void Run();

    //////////
    /// Inform the interpreter of the current Document.  Used so we can
    /// save the total number of files loaded.
    ///
    void RegisterDocument(Document *inDocument) { sDocument = inDocument; }

    //////////
    /// Get the document object associated with this TInterpreterManager,
    /// or NULL if no document has been loaded yet.
    ///
    Document *GetDocument() const { return sDocument; }

	//////////
	/// Call this function to start the script running.  This step is
	/// separate from the constructor so the application can choose
	/// a script to run and set up a working directory.
	///
	virtual void BeginScript();

	//////////
	/// Call this function to notify the TInterpreterManager of an
	/// application shutdown.
	///
	/// This function is generally called from within the SystemIdleProc.
	///
	void RequestQuitApplication();

	//////////
	/// Ask the TInterpreterManager to reload the currently running script,
	/// and jump to the specified card name.  (This operation has some odd
	/// semantics; basically, it's intended to work like "Reload" in a web
	/// browser.  Talk to one of the authoring staff to understand the Zen
	/// of ReloadScript.)  This function will call KillInterpreter.
	///
	/// This function is called from within the SystemIdleProc or a
	/// primitive.
	///
	/// \param inGotoCardName  The card to jump to after reloading.
	///
	void RequestReloadScript(const char *inGotoCardName);

    //////////
    /// Returns true if and only if the interpreter has begun.  We may
    /// not have a current valid interpreter, of course, but we have
    /// loaded a program at least once.
    ///
    bool InterpreterHasBegun();

	//////////
	/// Returns true if and only if the previous attempt to load a 
	/// script failed.  See RetryLoadScript.
	///
	/// This function is called from within the SystemIdleProc or a
	/// primitive.
	///
	bool FailedToLoad();

    //////////
    /// Returns true if and only if the TInterpreterManager exited because
    /// of an error.
    ///
    bool ExitedWithError() { return mExitedWithError; }

	//////////
	/// If FailedToReload() is true, then the GUI can call this function
	/// to retry the last ReloadScript command.  This allows the user
	/// to correct any syntax errors which caused the first ReloadScript
	/// to fail and to try again without relaunching the application.
	///
	/// This function is called from within the SystemIdleProc or a
	/// primitive.
	///
	void RequestRetryLoadScript();

    //////////
    /// Return the ScriptEditorDB associated with this manager's scripts.
    ///
    /// \return A pointer or NULL.
    ///
    virtual ScriptEditorDB *GetScriptEditorDBInternal() = 0;

    //////////
    /// Get the global ScriptEditorDB, if one is available.
    ///
    /// \return A pointer or NULL.
    ///
    static ScriptEditorDB *GetScriptEditorDB();

	//////////
	/// Do we have a single, global instance of this class?
	///
	static bool HaveInstance() { return (sInstance != NULL); }

	//////////
	/// Get the global interpreter manager.
	///
	static TInterpreterManager *GetInstance()
		{ ASSERT(sInstance); return sInstance; }

    //////////
    /// Request that an object be notified of script reload events.
    ///
    static void AddReloadNotified(TReloadNotified *obj);

    //////////
    /// Requent an end to reload notifications for this object.
    ///
    static void RemoveReloadNotified(TReloadNotified *obj);

    //////////
    /// Set whether the engine is in standalone runtime mode, or is
    /// a full-fledged editor.
    ///
    static void SetRuntimeMode(bool inIsInRuntimeMode);

    //////////
    /// Set a command to run (instead of jumping to the start card)
    /// when the interpreter starts up.
    ///
    static void SetInitialCommand(const std::string &inCommand);

    //////////
    /// Is the engine in standalone runtime mode, or is it a full-fledged
    /// editor?
    ///
    static bool IsInRuntimeMode();

    //////////
    /// Should we supress any splash screens, progress bars, etc.?
    ///
    static bool ShouldSuppressSplashScreen();

protected:
	//////////
	/// Create a new TInterpreter object with all the appropriate
	/// parameters.  Our subclasses implement this for us.
	///
	virtual TInterpreter *MakeInterpreter() = 0;

	//////////
	/// Make sure the initial card is set to the default value.
	///
	void ResetInitialCardName() { mInitialCardName = "/start"; }

private:
	//////////
	/// Load and run a script by calling MakeInterpreter.
	///
	void LoadAndRunScript();

    //////////
    /// Let everybody know we're starting to reload the script.
    ///
    void NotifyReloadScriptStarting();

    //////////
    /// Let everybody know we're succeeded in reloading the script.
    ///
    void NotifyReloadScriptSucceeded();

    //////////
    /// Let everybody know we've failed to reload the script.
    ///
    void NotifyReloadScriptFailed();
};

END_NAMESPACE_HALYARD

#endif // TInterpreter
