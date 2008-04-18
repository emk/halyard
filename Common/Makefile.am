# Set up our preprocessor and compiler flags.  Here, DISABLE_UNPORTED
# is a temporary way to disable parts of the code which won't build on
# non MSVC++ platforms yet.
AM_CPPFLAGS = $(BOOST_CPPFLAGS) -I../libs/sha1 \
	-I../libs/sqlite3-plus/sqlite3-plus
AM_CXXFLAGS = $(XML2_CFLAGS) $(FREETYPE_CFLAGS) $(MZSCHEME_CFLAGS)

noinst_LIBRARIES = libimlunit.a libhalyard.a

noinst_PROGRAMS = CommonTest

# Our very old test suite framework.
libimlunit_a_SOURCES = \
	ImlUnit.cpp ImlUnit.h

# Our portable runtime system.
libhalyard_a_SOURCES = \
	CaptionList.cpp CaptionList.h \
	CommandLine.cpp CommandLine.h \
	CommonHeaders.cpp CommonHeaders.h \
	CrashReporter.cpp CrashReporter.h \
	doc/Document.cpp doc/Document.h \
	doc/HalyardProgram.cpp doc/HalyardProgram.h \
	FileSystem.cpp FileSystem.h \
	GraphicsTools.cpp GraphicsTools.h \
	lang/scheme/TSchemeCallback.cpp lang/scheme/TSchemeCallback.h \
	lang/scheme/TSchemeConv.cpp lang/scheme/TSchemeConv.h \
	lang/scheme/TSchemeInterpreter.cpp lang/scheme/TSchemeInterpreter.h \
	lang/scheme/TSchemePtr.h \
	lang/scheme/TSchemeScriptEditorDB.cpp \
	lang/scheme/TSchemeScriptEditorDB.h \
	Model.cpp Model.h \
	ModelChange.cpp ModelChange.h \
	ModelView.cpp ModelView.h \
	ScriptEditorDB.cpp ScriptEditorDB.h \
	TCommon.h \
	TCommonPrimitives.cpp TCommonPrimitives.h \
	TDeveloperPrefs.cpp TDeveloperPrefs.h \
	TestCase.cpp TestCase.h \
	TException.cpp TException.h \
	TInterpreter.cpp TInterpreter.h \
	TLogger.cpp TLogger.h \
	TPlatform.h \
	TPoint.cpp TPoint.h \
	TPolygon.cpp TPolygon.h \
	TPrimitives.cpp TPrimitives.h \
	TRect.cpp TRect.h \
	TStartup.cpp TStartup.h \
	TStateDB.cpp TStateDB.h \
	TStateListenerManager.cpp TStateListenerManager.h \
	TStyleSheet.cpp TStyleSheet.h \
	TTemplateUtils.cpp TTemplateUtils.h \
	TTextConv.cpp TTextConv.h \
	TTextTransform.cpp TTextTransform.h \
	TValue.cpp TValue.h \
	TVariableManager.cpp TVariableManager.h \
	TVectorDiff.h \
	TVersion.h \
	TWarnings.h \
	Typography.cpp Typography.h \
	XmlUtils.cpp XmlUtils.h

# Our command-line test suite driver.
CommonTest_SOURCES = \
	FileSystemTests.cpp \
	lang/scheme/TSchemeInterpreterTests.cpp \
	ModelTests.cpp \
	TPolygonTests.cpp \
	TStyleSheetTests.cpp \
	TTextTransformTests.cpp \
	TVectorDiffTests.cpp \
	TypographyTests.cpp \
	TestAll.cpp
CommonTest_LDADD = \
	libhalyard.a \
	libimlunit.a \
	../libs/sha1/libsha1.a \
	../libs/sqlite3-plus/sqlite3-plus/libsqlite3_plus.a \
	$(BOOST_LDFLAGS) \
        $(BOOST_FILESYSTEM_LIB) \
	$(XML2_LIBS) \
	$(FREETYPE_LIBS) \
	$(SQLITE3_LDFLAGS) \
        $(MZSCHEME_LDFLAGS)


#==========================================================================
#  Precompiled Headers
#==========================================================================
#  Experimental support for building a precompiled header.  This is
#  highly-dependent on having a reasonably modern GNU toolchain.  If you
#  don't, just disable this code completely, and build with regular
#  headers.
#
#  We go to the trouble of setting up precompiled headers because boost
#  includes tons of templates, which slows down gcc considerably.
#
#  TODO - How do I add a dependency to all our *.cpp files?

#%.h.pcx: %.h
#	if $(CXXCOMPILE) -MT $@ -MD -MP -MF "$(DEPDIR)/$*.Tpo" -x c++-header -c -o $@ $<; then mv -f "$(DEPDIR)/$*.Tpo" "$(DEPDIR)/$*.Po"; else rm -f "$(DEPDIR)/$*.Tpo"; exit 1; fi

#include ./$(DEPDIR)/CommonHeaders.Po
