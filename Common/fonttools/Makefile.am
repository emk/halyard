CXXFLAGS=-I../ -I../../libs/crypto -I../../libs/boost @CXXFLAGS@ -Wall -Werror

noinst_PROGRAMS = pngtest listfonts fontspeed

pngtest_SOURCES = \
	pngtest.cpp \
	Image.h \
	Image.cpp

pngtest_LDADD = @FREETYPE_LDADD@ -lgd ../libfivel.a

listfonts_SOURCES = \
	listfonts.cpp

listfonts_LDADD = @FREETYPE_LDADD@ -lgd ../libfivel.a

fontspeed_SOURCES = \
	fontspeed.cpp

fontspeed_LDADD = @FREETYPE_LDADD@ -lgd ../libfivel.a \
	../../libs/crypto/libfivel_crypto.a \
	../../libs/plt/lib/libmzscheme.a \
	../../libs/plt/lib/libmzgc.a \
	../../libs/plt/lib/mzdyn.o \
	-ldl
#	-lefence

