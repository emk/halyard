Portable Fonts for Use By The 5L Engine
=======================================

These are various free fonts which we should *allegedly* be allowed to ship
with our programs.  In general, these came from various allegedly DFSG- and
OSI- compliant font packages found on a typical Debian GNU/Linux system.
Descriptions and copyright information follow.

Marker Fonts
------------

These are inexpensive shareware fonts from Pat Snyder, who we can't find,
and who we need to find before we ship this fonts.


tim???.pcf
----------

Bitmapped ISO-10646-1 Times fonts from the standard X-Window/XFree86
distribution.  The Debian project removes several fonts with various
licensing restrictions from the X distribution; these remain afterward.

  Copyright (c) 1984, 1987 Adobe Systems Incorporated. All Rights Reserved.
  Copyright (c) 1988, 1991 Digital Equipment Corporation.  All Rights
  Reserved.  Times is a trademark of Linotype-Hell AG and/or its
  subsidiaries.

Tracking down the original Adobe and Digital license grants which allowed
the X project to include these fonts may take a bit of work.


*.pfb, *.afm
------------

Most of the these PostScript fonts are GPL'd clones of the standard Adobe
PostScript fonts.  They were donated by URW to the GhostScript project.
(These files can be identified by reading the copyright notices.)

c0648bt_.pfb, c0648bt_.afm
--------------------------

This is Bitstream Charter, another PostScript font from the X
distribution.  Once again, we're basically taking Debian's word that this
font is free; we should check it out in more detail.
