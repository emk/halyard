
CXXFLAGS = @CXXFLAGS@ -Wall -Werror \
	-DFIVEL_NO_MOVIES \
	`../../libs/wxWindows/wx-config --cxxflags` \
	-I ../../libs/wxWindows/include \
	-I ../../libs/wxWindows/lib/wx/include/gtk-2.3 \
	-I ../../libs/crypto \
	-I ../../libs/boost \
	-I ../../libs/plt/include \
	-I ../../Common

noinst_PROGRAMS = wx5L

wx5L_SOURCES = \
	AppGlobals.h \
	AppGraphics.cpp \
	AppGraphics.h \
	Element.cpp \
	Element.h \
	FiveLApp.cpp \
	FiveLApp.h \
	Listener.cpp \
	Listener.h \
	Log5L.cpp \
	Log5L.h \
	MovieElement.cpp \
	MovieElement.h \
	MovieWindow.cpp \
	MovieWindow.h \
	resource.h \
	Stage.cpp \
	Stage.h \
	Timecoder.cpp \
	Timecoder.h \
	ToolWindow.cpp \
	ToolWindow.h \
	TWxPrimitives.cpp \
	TWxPrimitives.h \
	Widget.cpp \
	Widget.h \
	Zone.cpp \
	Zone.h

#	TQTMovie.cpp
#	TQTMovie.h
#	MovieWindowQT.cpp
#	MovieWindowQT.h


wx5L_LDFLAGS = \
	-L../../libs/wxWindows/lib

wx5L_LDADD = \
	../../Common/libfivel.a \
	../../libs/crypto/libfivel_crypto.a \
	../../libs/plt/lib/libmzscheme.a \
	../../libs/plt/lib/libmzgc.a \
	../../libs/plt/lib/mzdyn.o \
	@FREETYPE_LDADD@ \
	-lwx_gtk-2.3 \
	-ldl
#	-lefence
