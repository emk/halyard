AM_CPPFLAGS = $(BOOST_CPPFLAGS) $(WX_CPPFLAGS) -I$(top_srcdir)/Common
# We turn off deprecated function warnings, because we're currently trying
# to compile the same code base under both wxWidgets 2.6 and 2.8, and we
# want all _other_ warnings to abort the compilation process, thanks to
# -Werror.  We can remove -Wno-deprecated-declarations once we port the
# official Windows build to 2.8.
AM_CXXFLAGS = $(FREETYPE_CFLAGS) $(MZSCHEME_CFLAGS) $(CURL_CFLAGS) \
	$(CAIRO_CFLAGS) -Wno-deprecated-declarations

if NEED_BUNDLE

# If we're building on the Mac, our application won't run unless we create
# a proper *.app bundle directory.  This can be run using "open
# ./Halyard.app" from the command-line.
HALYARD_APP = Halyard.app
BUNDLED_HALYARD = $(HALYARD_APP)/Contents/MacOS/halyard
BUNDLED_RUNTIME = $(HALYARD_APP)/Contents/SharedSupport

all: $(BUNDLED_HALYARD) $(BUNDLED_RUNTIME)

$(BUNDLED_HALYARD): halyard
	mkdir -p "`dirname $(BUNDLED_HALYARD)`"
	cp "$<" "$@"
	chmod +x "$@"

$(BUNDLED_RUNTIME):
	mkdir -p "`dirname $(BUNDLED_RUNTIME)`"
	(cd "`dirname $(BUNDLED_RUNTIME)`" && \
	 ln -sf "$(abs_top_srcdir)/runtime/" "`basename $(BUNDLED_RUNTIME)`")

check: all
	(cd Halyard.app; \
	 Contents/MacOS/halyard -c '(command-line-test-driver)' \
	     $(abs_top_srcdir)/test)

jump_to_each_card: all
	(cd Halyard.app; \
	 Contents/MacOS/halyard -c '(jump-to-each-card)' \
	     $(abs_top_srcdir)/test)

endif

resources.cpp: application.xrc
	$(WXRC) -c -o $@ $<

CLEANFILES = resources.cpp AppHeaders.h.gch

# Until we can actually compile all our source files, build a library
# instead of an application.
noinst_PROGRAMS = halyard

halyard_SOURCES = \
	AnimatedOverlay.cpp AnimatedOverlay.h \
	AppConfig.h \
	AppGlobals.h \
	AppGraphics.cpp AppGraphics.h \
	AppLog.cpp AppLog.h \
	AuiFrame.cpp AuiFrame.h \
	BrowserElement.cpp BrowserElement.h \
	BrowserElementWx.cpp BrowserElementWx.h \
	BufferSpan.cpp BufferSpan.h \
	CairoDrawing.cpp CairoDrawing.h \
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
	DrawingContextStack.cpp DrawingContextStack.h \
	EditBox.cpp EditBox.h \
	Element.cpp Element.h \
	EventDelegator.cpp EventDelegator.h \
	EventDispatcher.cpp EventDispatcher.h \
	FancyStatusBar.cpp FancyStatusBar.h \
	GuiUtil.cpp GuiUtil.h \
	HalyardApp.cpp HalyardApp.h \
	HistoryText.cpp HistoryText.h \
	UrlRequest.cpp UrlRequest.h \
	ImageCache.cpp ImageCache.h \
	InvisibleElement.h \
	LightweightAccessible.cpp LightweightAccessible.h \
	LightweightElement.cpp LightweightElement.h \
	Listener.cpp Listener.h \
	LocationBox.cpp LocationBox.h \
	MediaElement.cpp MediaElement.h \
	MovieElement.cpp MovieElement.h \
	MovieWindow.cpp MovieWindow.h \
	MovieWindowQT.cpp MovieWindowQT.h \
	Overlay.cpp Overlay.h \
	ProgramTree.cpp ProgramTree.h \
	resources.cpp \
	ScriptEditor.cpp ScriptEditor.h \
	Stage.cpp Stage.h \
	StageAccessible.cpp StageAccessible.h \
	StageFrame.cpp StageFrame.h \
	TestHarness.cpp TestHarness.h \
	Timecoder.cpp Timecoder.h \
	ToolWindow.cpp ToolWindow.h \
	TQTMovie.cpp TQTMovie.h \
	Transition.h TransitionOpt.cpp \
	TWxPrimitives.cpp TWxPrimitives.h \
	Widget.cpp Widget.h \
	Zone.cpp Zone.h
halyard_LDADD = \
	$(top_builddir)/Common/libhalyard.a \
	$(top_builddir)/Common/libimlunit.a \
	$(top_builddir)/libs/sha1/libsha1.a \
	$(top_builddir)/libs/sqlite3-plus/sqlite3-plus/libsqlite3_plus.a \
	$(BOOST_LDFLAGS) \
        $(BOOST_FILESYSTEM_LIB) \
	$(XML2_LIBS) \
	$(FREETYPE_LIBS) \
	$(SQLITE3_LDFLAGS) \
	$(LOG4CPLUS_LIBS) \
        $(MZSCHEME_LDFLAGS) \
	$(CURL_LIBS) \
	$(CAIRO_LIBS) \
	$(WX_LIBS)

# Temporarily disabled because of dependencies on wxWidgets 2.6 APIs
# missing in 2.8.
#FancyCrashReporter.cpp FancyCrashReporter.h



# Disabled because they rely on various APIs we're not ready to deal with
# right now, including portaudio, QuickTime and Quake 2.
#AudioStream.cpp AudioStream.h
#AudioStreamElement.cpp AudioStreamElement.h
#BinMsgConv.cpp BinMsgConv.h
#GeigerAudioStream.cpp GeigerAudioStream.h
#GeigerSynthElement.cpp GeigerSynthElement.h
#Quake2Engine.cpp Quake2Engine.h
#TQuake2Primitives.cpp TQuake2Primitives.h
#VorbisAudioStream.cpp VorbisAudioStream.h
#VorbisFile.cpp VorbisFile.h

# Windows-specific files.
#ActiveXElement.cpp ActiveXElement.h
#BrowserElementIE.cpp BrowserElementIE.h
#resource.h


#==========================================================================
#  Precompiled Headers
#==========================================================================

# We include AppHeaders.h.gch in CLEANFILES higher up in the file.
#CLEANFILES = AppHeaders.h.gch

if ENABLE_PRECOMPILED_HEADERS

$(halyard_OBJECTS): AppHeaders.h.gch

include ./$(DEPDIR)/AppHeaders.Po

%.h.gch: %.h
	if $(CXXCOMPILE) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.Tpo" -x c++-header -c -o $@ $<; then mv -f "$(DEPDIR)/$*.Tpo" "$(DEPDIR)/$*.Po"; else rm -f "$(DEPDIR)/$*.Tpo"; exit 1; fi

endif
