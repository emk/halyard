// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Tamale - Multimedia authoring and playback system
// Copyright 1993-2004 Trustees of Dartmouth College
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

#include "TamaleHeaders.h"
#include "ActiveXElement.h"
#include <wxactivex.h>
#include "EventDispatcher.h"

/// Create a new ActiveX control on the stage.
///
/// \param inStage The stage on which to create the element.
/// \param inName The name of the element.
/// \param inDispatch The event dispatcher for this element.
/// \param inControlName The ActiveX name of the control to create.
ActiveXElement::ActiveXElement(Stage *inStage, const wxString &inName,
                               const wxRect &inBounds,
                               FIVEL_NS TCallbackPtr inDispatch,
                               const wxString &inControlName)
    : Widget(inStage, inName, inDispatch)                      
{
    mControl = new wxActiveX(inStage, inControlName, -1,
                             inBounds.GetPosition(), inBounds.GetSize(),
                             wxSIMPLE_BORDER);
    InitializeWidgetWindow(mControl);
}

/// Get an ActiveX control property.
wxVariant ActiveXElement::Prop(const wxString &name) {
    return mControl->Prop(name);
}

/// Set an ActiveX control property.
void ActiveXElement::SetProp(const wxString &name, const wxVariant &value) {
    mControl->SetProp(name, value);
}
