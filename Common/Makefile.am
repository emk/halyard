CXXFLAGS = @CXXFLAGS@ -Wall -Werror \
	-I ../libs/crypto \
	-I ../libs/boost \
	-I ../libs/plt/include

SUBDIRS = . fonttools

noinst_LIBRARIES = libimlunit.a libfivel.a

noinst_PROGRAMS = TestAll

TESTS = TestAll

libimlunit_a_SOURCES = \
	ImlUnit.cpp \
	ImlUnit.h

libfivel_a_SOURCES = \
	Model.cpp \
	Model.h \
	ModelChange.cpp \
	ModelChange.h \
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
	TInterpreter.h \
	TLogger.cpp \
	TLogger.h \
	TObject.cpp \
	TObject.h \
	TPoint.cpp \
	TPoint.h \
	TRect.cpp \
	TRect.h \
	TString.cpp \
	TString.h \
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
	TInterpreter.cpp \
	TInterpreter.h \
	TPrimitives.cpp \
	TPrimitives.h \
	TCommonPrimitives.cpp \
	TCommonPrimitives.h \
	TTemplateUtils.cpp \
	TTemplateUtils.h \
	TStartup.cpp \
	TStartup.h \
	TDeveloperPrefs.cpp \
	TDeveloperPrefs.h \
	XmlUtils.cpp \
	XmlUtils.h \
	lang/old5l/TIndex.cpp \
	lang/old5l/TIndex.h \
	lang/old5l/TParser.cpp \
	lang/old5l/TParser.h \
	lang/old5l/TStream.cpp \
	lang/old5l/TStream.h \
	lang/old5l/CryptStream.cpp \
	lang/old5l/CryptStream.h \
	lang/old5l/T5LPrimitives.cpp \
	lang/old5l/T5LPrimitives.h \
	lang/scheme/TSchemeInterpreter.cpp \
	lang/scheme/TSchemeInterpreter.h

TestAll_SOURCES = \
	ModelTests.cpp \
	TTemplateUtilsTests.cpp \
	TStringTests.cpp \
	TBTreeTests.cpp \
	TEncodingTests.cpp \
	FileSystemTests.cpp \
	TypographyTests.cpp \
	TStyleSheetTests.cpp \
	lang/old5l/TStreamTests.cpp \
	lang/old5l/CryptStreamTests.cpp \
	lang/old5l/TIndexTests.cpp \
	lang/scheme/TSchemeInterpreterTests.cpp \
	TestAll.cpp
TestAll_LDADD = \
	libfivel.a \
	libimlunit.a \
	../libs/crypto/libfivel_crypto.a \
	../libs/plt/lib/libmzscheme.a \
	../libs/plt/lib/libmzgc.a \
	../libs/plt/lib/mzdyn.o \
	@FREETYPE_LDADD@ \
	@LIBXML2_LDADD@ \
	-ldl
#	-lefence
