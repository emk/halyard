// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef fade_asm_H
#define fade_asm_H

extern void OptimizedDrawingStart();
extern void OptimizedDrawingEnd();

extern void OptimizedFadePixels(wxRawBitmapPixelRef24 inPixelsBegin, 
								wxRawBitmapPixelRef24 inPixelsEnd,
								wxRawBitmapPixelRef24 outPixelsBegin,
								uint16 inValue);

#endif // fade_asm_H
