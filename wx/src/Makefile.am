AM_CPPFLAGS = $(BOOST_CPPFLAGS) $(WX_CPPFLAGS) -I../../Common
# We turn off deprecated function warnings, because we're currently trying
# to compile the same code base under both wxWidgets 2.6 and 2.8, and we
# want all _other_ warnings to abort the compilation process, thanks to
# -Werror.  We can remove -Wno-deprecated-declarations once we port the
# official Windows build to 2.8.
AM_CXXFLAGS = $(FREETYPE_CFLAGS) $(MZSCHEME_CFLAGS) $(CURL_CFLAGS) \
	-Wno-deprecated-declarations

if NEED_BUNDLE

# If we're building on the Mac, our application won't run unless we create
# a proper *.app bundle directory.  This can be run using "open
# ./Halyard.app" from the command-line.
HALYARD_APP = Halyard.app
BUNDLED_HALYARD = $(HALYARD_APP)/Contents/MacOS/halyard

all: $(BUNDLED_HALYARD)

$(BUNDLED_HALYARD): halyard
	mkdir -p "`dirname $(BUNDLED_HALYARD)`"
	cp "$<" "$@"
	chmod +x "$@"

endif

resources.cpp: application.xrc
	$(WXRC) -c -o $@ $<

CLEANFILES = resources.cpp

# Until we can actually compile all our source files, build a library
# instead of an application.
noinst_PROGRAMS = halyard

halyard_SOURCES = \
	AnimatedOverlay.cpp AnimatedOverlay.h \
	AppConfig.h \
	AppGlobals.h \
	AppGraphics.cpp AppGraphics.h \
	AppHeaders.cpp AppHeaders.h \
	AppLog.cpp AppLog.h \
	BrowserElement.cpp BrowserElement.h \
	BrowserElementWx.cpp BrowserElementWx.h \
	BufferSpan.cpp BufferSpan.h \
	CommonWxConv.cpp CommonWxConv.h \
	Cursor.cpp Cursor.h \
	CursorElement.cpp CursorElement.h \
	CursorManager.cpp CursorManager.h \
	CustomTreeCtrl.cpp CustomTreeCtrl.h \
	DirtyList.cpp DirtyList.h \
	dlg/AdjustScreenConfirmDlg.cpp dlg/AdjustScreenConfirmDlg.h \
	dlg/AdjustScreenDlg.cpp dlg/AdjustScreenDlg.h \
	dlg/FindDlg.cpp dlg/FindDlg.h \
	dlg/MetaDotDlg.cpp dlg/MetaDotDlg.h \
	dlg/MultiButtonDlg.cpp dlg/MultiButtonDlg.h \
	dlg/ProgramPropDlg.cpp dlg/ProgramPropDlg.h \
	dlg/PropertyDlg.cpp dlg/PropertyDlg.h \
	dlg/StartupDlg.cpp dlg/StartupDlg.h \
	dlg/XrcDlg.cpp dlg/XrcDlg.h \
	DocNotebook.cpp DocNotebook.h \
	Downloader.cpp Downloader.h \
	DrawingArea.cpp DrawingArea.h \
	DrawingAreaOpt.cpp DrawingAreaOpt.h \
	DrawingContextStack.cpp DrawingContextStack.h \
	EditBox.cpp EditBox.h \
	Element.cpp Element.h \
	EventDelegator.cpp EventDelegator.h \
	EventDispatcher.cpp EventDispatcher.h \
	FancyStatusBar.cpp FancyStatusBar.h \
	GuiUtil.cpp GuiUtil.h \
	HalyardApp.cpp HalyardApp.h \
	HistoryText.cpp HistoryText.h \
	ImageCache.cpp ImageCache.h \
	InvisibleElement.h \
	LightweightAccessible.cpp LightweightAccessible.h \
	LightweightElement.cpp LightweightElement.h \
	Listener.cpp Listener.h \
	LocationBox.cpp LocationBox.h \
	MediaElement.cpp MediaElement.h \
	MovieElement.cpp MovieElement.h \
	MovieWindow.cpp MovieWindow.h \
	Overlay.cpp Overlay.h \
	ProgramTree.cpp ProgramTree.h \
	resources.cpp \
	SashFrame.cpp SashFrame.h \
	Stage.cpp Stage.h \
	StageAccessible.cpp StageAccessible.h \
	StageFrame.cpp StageFrame.h \
	TestHarness.cpp TestHarness.h \
	Timecoder.cpp Timecoder.h \
	ToolWindow.cpp ToolWindow.h \
	Transition.h TransitionOpt.cpp \
	TWxPrimitives.cpp TWxPrimitives.h \
	Widget.cpp Widget.h \
	Zone.cpp Zone.h
halyard_LDADD = \
	../../Common/libhalyard.a \
	../../Common/libimlunit.a \
	../../libs/sha1/libsha1.a \
	../../libs/sqlite3-plus/sqlite3-plus/libsqlite3_plus.a \
	$(BOOST_LDFLAGS) \
        $(BOOST_FILESYSTEM_LIB) \
	$(XML2_LIBS) \
	$(FREETYPE_LIBS) \
	$(SQLITE3_LDFLAGS) \
        $(MZSCHEME_LDFLAGS) \
	$(CURL_LIBS) \
	$(WX_LIBS)

# Temporarily disabled because of dependencies on wxWidgets 2.6 APIs
# missing in 2.8.
#FancyCrashReporter.cpp FancyCrashReporter.h

# Disabled because of dependencies on our custom-patched version of Scintilla.
#ScriptEditor.cpp ScriptEditor.h

# Disabled because they rely on various APIs we're not ready to deal with
# right now, including portaudio, QuickTime and Quake 2.
#AudioStream.cpp AudioStream.h
#AudioStreamElement.cpp AudioStreamElement.h
#BinMsgConv.cpp BinMsgConv.h
#GeigerAudioStream.cpp GeigerAudioStream.h
#GeigerSynthElement.cpp GeigerSynthElement.h
#MovieWindowQT.cpp MovieWindowQT.h
#Quake2Engine.cpp Quake2Engine.h
#TQTMovie.cpp TQTMovie.h
#TQuake2Primitives.cpp TQuake2Primitives.h
#VorbisAudioStream.cpp VorbisAudioStream.h
#VorbisFile.cpp VorbisFile.h

# Windows-specific files.
#ActiveXElement.cpp ActiveXElement.h
#BrowserElementIE.cpp BrowserElementIE.h
#resource.h