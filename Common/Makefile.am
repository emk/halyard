AUTOMAKE_OPTIONS = foreign

CXX_FLAGS = @CXX_FLAGS@ -Wall -Werror

SUBDIRS = . fonttools

noinst_LIBRARIES = libimlunit.a libfivel.a

noinst_PROGRAMS = TestAll

TESTS = TestAll

libimlunit_a_SOURCES = \
	ImlUnit.cpp \
	ImlUnit.h

libfivel_a_SOURCES = \
	TArray.cpp \
	TArray.h \
	TBTree.cpp \
	TBTree.h \
	TCommon.h \
	TException.cpp \
	TException.h \
	THeader.cpp \
	THeader.h \
	TObject.cpp \
	TObject.h \
	TPoint.cpp \
	TPoint.h \
	TRect.cpp \
	TRect.h \
	TString.cpp \
	TString.h \
	TURL.cpp \
	TURL.h \
	TEncoding.cpp \
	TEncoding.h \
	Typography.cpp \
	Typography.h \
	FileSystem.cpp \
	FileSystem.h \
	GraphicsTools.cpp \
	GraphicsTools.h

# These only compile on Windows.  But nobody else uses them, so it
# doesn't matter for now.
#	TLogger.cpp \
#	TLogger.h \

TestAll_SOURCES = \
	TStringTests.cpp \
	TEncodingTests.cpp \
	FileSystemTests.cpp \
	TypographyTests.cpp \
	TestAll.cpp
TestAll_LDADD = \
	libfivel.a \
	libimlunit.a \
	@FREETYPE_LDADD@

#	-lefence
