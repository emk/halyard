CXXFLAGS=-I../ @CXXFLAGS@

noinst_PROGRAMS = pngtest

pngtest_SOURCES = \
	pngtest.cpp \
	Image.h \
	Image.cpp

pngtest_LDADD = @FREETYPE_LDADD@ -lgd ../libfivel.a
