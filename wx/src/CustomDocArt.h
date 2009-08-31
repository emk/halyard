// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2009 Trustees of Dartmouth College
// 
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation; either version 2
// of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//
// @END_LICENSE

#ifndef CustomDocArt_H
#define CustomDocArt_H

#include <wx/aui/framemanager.h>
#include <wx/aui/dockart.h>

/// Graphical customizations for our StageFrame.  For now, we just remove
/// the border around the various panels, because we don't want to display
/// a border on the StageBackground when in full screen mode.
///
/// XXX - This produces some graphical artifacting in regular editing mode,
/// and we should undo it when we're not in full screen mode.
class CustomDocArt : public wxAuiDefaultDockArt {
    virtual void DrawBorder(wxDC& dc, wxWindow* window, const wxRect& rect,
                            wxAuiPaneInfo& pane);    
    virtual int GetMetric(int id);
};

#endif CustomDocArt_H
