AUTOMAKE_OPTIONS = foreign

CXXFLAGS = @CXXFLAGS@ -Wall -Werror -I libs/crypto

SUBDIRS = libs . fonttools

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
	TDateUtil.cpp \
	TDateUtil.h \
	TException.cpp \
	TException.h \
	THeader.cpp \
	THeader.h \
	TIndex.cpp \
	TIndex.h \
	TInterpreter.h \
	TLogger.cpp \
	TLogger.h \
	TObject.cpp \
	TObject.h \
	TParser.cpp \
	TParser.h \
	TPoint.cpp \
	TPoint.h \
	TRect.cpp \
	TRect.h \
	TString.cpp \
	TString.h \
	TStream.cpp \
	TStream.h \
	TStyleSheet.cpp \
	TStyleSheet.h \
	TURL.cpp \
	TURL.h \
	TVariable.cpp \
	TVariable.h \
	TEncoding.cpp \
	TEncoding.h \
	Typography.cpp \
	Typography.h \
	FileSystem.cpp \
	FileSystem.h \
	GraphicsTools.cpp \
	GraphicsTools.h \
	CryptStream.cpp \
	CryptStream.h \
	TInterpreter.cpp \
	TInterpreter.h \
	TPrimitives.cpp \
	TPrimitives.h \
	TCommonPrimitives.cpp \
	TCommonPrimitives.h \
	T5LPrimitives.cpp \
	T5LPrimitives.h \
	TTemplateUtils.cpp \
	TTemplateUtils.h \
	TStartup.cpp \
	TStartup.h \
	TDeveloperPrefs.cpp \
	TDeveloperPrefs.h

TestAll_SOURCES = \
	TTemplateUtilsTests.cpp \
	TStringTests.cpp \
	TStreamTests.cpp \
	TBTreeTests.cpp \
	TEncodingTests.cpp \
	FileSystemTests.cpp \
	TypographyTests.cpp \
	TStyleSheetTests.cpp \
	CryptStreamTests.cpp \
	TIndexTests.cpp \
	TestAll.cpp
TestAll_LDADD = \
	libfivel.a \
	libimlunit.a \
	libs/crypto/libfivel_crypto.a \
	@FREETYPE_LDADD@
#	-lefence
