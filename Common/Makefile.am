AUTOMAKE_OPTIONS = foreign

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
	TEncoding.h

# These only compile on Windows.  But nobody else uses them, so it
# doesn't matter for now.
#	TLogger.cpp \
#	TLogger.h \

TestAll_SOURCES = \
	TStringTests.cpp \
	TEncodingTests.cpp \
	TestAll.cpp
TestAll_LDADD = \
	libfivel.a \
	libimlunit.a

#	-lefence
