
CXXFLAGS = @CXXFLAGS@ -Wall -Werror \
	`../../libs/wxWindows/wx-config --cxxflags` \
	-I ../../libs/wxWindows/include \
	-I ../../libs/wxWindows/lib/wx/include/gtk-2.3 \
	-I ../../libs/crypto \
	-I ../../libs/boost \
	-I ../../libs/plt/include \
	-I ../../Common

noinst_PROGRAMS = wx5L

wx5L_SOURCES = \
	AppConfig.h \
	AppGlobals.h \
	AppGraphics.cpp \
	AppGraphics.h \
	Element.cpp \
	Element.h \
	FiveLApp.cpp \
	FiveLApp.h \
	Listener.cpp \
	Listener.h \
	LocationBox.cpp \
	LocationBox.h \
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
	-L../../libs/wxWindows/lib \
	`../../libs/wxWindows/wx-config --ldflags` \
	-pthread

# From 	`../../libs/wxWindows/wx-config --libs`, which automake won't let
# us call directly (and which we need to edit for now).
WXLIBS = -lwx_gtk-2.3 -L/usr/X11R6/lib -lgtk -lgdk -lgmodule -lgthread -lglib -lpthread -ldl -lXi -lXext -lX11 -lm -lpng -ljpeg -ltiff -lz -ldl -lm 

wx5L_LDADD = \
	../../Common/libfivel.a \
	../../libs/crypto/libfivel_crypto.a \
	../../libs/plt/lib/libmzscheme.a \
	../../libs/plt/lib/libmzgc.a \
	../../libs/plt/lib/mzdyn.o \
	@FREETYPE_LDADD@ \
	$(WXLIBS) \
	-ldl
#	-lefence