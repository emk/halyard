//
// Configuration.h
//
//----------------------------------------------------------------------------------------
// Configuration
//----------------------------------------------------------------------------------------

struct Configuration
{
	// Values from SysEnvirons (Version 1)
	short environsVersion;
	short machineType;
	short systemVersion;
	short quicktimeVersion;
	short processor;
	Boolean hasFPU;
	Boolean hasColorQD;
	short keyboardType;
	short atDrvrVersNum;
	long teVersion;								// TextEdit version
	// Derived values
	Boolean hasROM128K;							// ROM 128K - OR - Better
	Boolean hasHFS;

	Boolean hasHierarchicalMenus;
	Boolean hasScriptManager;
	Boolean hasStyleTextEdit;
	Boolean hasSoundManager;
	Boolean hasWaitNextEvent;
	Boolean hasSCSI;
	Boolean hasDesktopBus;
	Boolean hasAUX;
	Boolean hasTempMem;							// true if Process Manager temp memory is
                                                // avail
	Boolean has32BitQD;							// true if 32 bit Quickdraw is installed
	Boolean hasAppleEventMgr;					// true if running under System 7.0
	Boolean hasEditionMgr;						// true if running under System 7.0
	Boolean hasHelpMgr;							// true if running under System 7.0
	Boolean hasAliasMgr;						// true if running under System 7.0
	Boolean hasFolderMgr;						// true if running under System 7.0
	Boolean hasProcessMgr;						// true if running under System 7.0
	Boolean hasPopupCDEF;						// true if running under System 7.0
	Boolean hasTrueType;						// true if TrueType is present

	Boolean isOnlyBackground;					// true if ProcessInfoRec.processMode has modeOnlyBackground
	Boolean isHighLevelEventAware;				// true if ProcessInfoRec.processMode has modeHighLevelEventAware
	
	Boolean hasDragManager;
	Boolean hasThreadManager;
	Boolean hasAOCEToolBox;
	Boolean hasQDGXGraphics;
	Boolean hasQDGXPrinting;
#if 0
		// Removed because ASLM 1.1.2, if deinstalled by hand, will crash in Gestalt
		Boolean hasASLM;
#endif
	Boolean hasCFM;
	Boolean hasTranslationManager;
	Boolean hasSpeechManager;
	Boolean hasCustomFile;
	Boolean hasQuickTime;
	Boolean hasTSM;
};