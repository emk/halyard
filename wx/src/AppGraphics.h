// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef AppGraphics_H
#define AppGraphics_H


// We get our graphics from XPM files on non-Windows systems.
#ifndef __WXMSW__

typedef char *xpm_graphic_t[];

extern xpm_graphic_t ic_5L_xpm;
extern xpm_graphic_t ic_listener_xpm;
extern xpm_graphic_t ic_timecoder_xpm;
extern xpm_graphic_t tb_borders_xpm;
extern xpm_graphic_t tb_grid_xpm;
extern xpm_graphic_t tb_reload_xpm;
extern xpm_graphic_t tb_xy_xpm;

#endif // __WXMSW__

#endif // AppGraphics_H
