CCFLAGS = @CCFLAGS@ -Wall -Werror
CXXFLAGS = @CXXFLAGS@ -Wall -Werror

noinst_PROGRAMS = md5main

noinst_LIBRARIES = libfivel_crypto.a

md5main_SOURCES = \
	md5.h \
	md5.c \
	md5main.c
md5main_LDADD = \
	-lm

libfivel_crypto_a_SOURCES = \
	blowfish.h \
	blowfish.c \
	md5.h \
	md5.c
