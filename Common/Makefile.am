# Set up our preprocessor and compiler flags.  Here, DISABLE_UNPORTED
# is a temporary way to disable parts of the code which won't build on
# non MSVC++ platforms yet.
AM_CPPFLAGS = $(BOOST_CPPFLAGS) -I../libs/sha1 -DDISABLE_UNPORTED=1
AM_CXXFLAGS = $(XML2_CFLAGS) $(FREETYPE_CFLAGS)

noinst_LIBRARIES = libimlunit.a libhalyard.a

# TestAll will actually work if you comment out the portions testing code
# we haven't built.
noinst_PROGRAMS = TestAll
TESTS = TestAll

# Our very old test suite framework.
libimlunit_a_SOURCES = \
	ImlUnit.cpp \
	ImlUnit.h

# Our portable runtime system.
libhalyard_a_SOURCES = \
	TException.cpp \
	TPoint.cpp \
	TRect.cpp \
	TPolygon.cpp \
	GraphicsTools.cpp \
	TTemplateUtils.cpp \
	TLogger.cpp \
	TestCase.cpp \
	TTextConv.cpp \
	TInterpreter.cpp \
	FileSystem.cpp \
	TLogger.cpp \
	CrashReporter.cpp \
	XmlUtils.cpp \
	Model.cpp \
	ModelChange.cpp \
	ModelView.cpp \
	doc/Document.cpp \
	doc/HalyardProgram.cpp \
	TTextTransform.cpp \
	Typography.cpp \
	CaptionList.cpp \
	TTextConv.cpp \
	TDeveloperPrefs.cpp \
	CommandLine.cpp

# Our command-line test suite driver.
TestAll_SOURCES = \
	TPolygonTests.cpp \
	FileSystemTests.cpp \
	ModelTests.cpp \
	TTextTransformTests.cpp \
	TypographyTests.cpp \
	TVectorDiffTests.cpp \
	TestAll.cpp
TestAll_LDADD = \
	libhalyard.a \
	libimlunit.a \
	../libs/sha1/libsha1.a \
	@BOOST_LDFLAGS@ \
        @BOOST_FILESYSTEM_LIB@ \
	@XML2_LIBS@ \
	@FREETYPE_LIBS@


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
