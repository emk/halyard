CXXFLAGS=-I../ @CXXFLAGS@ -Wall -Werror

noinst_PROGRAMS = pngtest listfonts

pngtest_SOURCES = \
	pngtest.cpp \
	Image.h \
	Image.cpp

pngtest_LDADD = @FREETYPE_LDADD@ -lgd ../libfivel.a

listfonts_SOURCES = \
	listfonts.cpp

listfonts_LDADD = @FREETYPE_LDADD@ -lgd ../libfivel.a

