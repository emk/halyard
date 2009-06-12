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

#ifndef PropertyDlg_H
#define PropertyDlg_H

namespace model { class Object; };

//////////
/// A dialog which allows you to edit the properties of a model::Object.
///
class PropertyDlg : public wxDialog {
public:
    typedef unsigned long Flags;
    enum /* Flags */ {
        MULTILINE = 1
    };

    /// A mapping between a dialog item and a field of a model::Object.
    class Field {
    public:
        Field(const char *inName, const char *inLabel, Flags inFlags)
            : name(inName), label(inLabel), flags(inFlags) {}

        const char *name;
        const char *label;
        Flags flags;
    };

    /// The description of a PropertyDlg to create.
    class Description {
    public:
        Description(const char *inName);
        void AddField(const Field &inField);

        const char *name;
        std::vector<Field> fields;
    };

private:
    DECLARE_EVENT_TABLE();
    
    model::Object *mObject;
    wxSizer *mPropSizer;

    wxButton *mOkButton;
    wxButton *mCancelButton;
    wxButton *mApplyButton;
    wxButton *mHelpButton;

protected:
    PropertyDlg(wxWindow *inParent, Description *inDescription,
                model::Object *inObject);

    void AddField(Field &inField);
};

#define DECLARE_PROPERTY_DIALOG() \
    static Description *GetDialogDescription()

#define BEGIN_PROPERTY_DIALOG(CLASS, NAME) \
    PropertyDlg::Description *CLASS::GetDialogDescription() { \
        Description *result = new Description(NAME);

#define PROPERTY_FIELD(NAME,LABEL,FLAGS) \
        result->AddField(Field(NAME,LABEL,FLAGS));

#define END_PROPERTY_DIALOG() \
        return result; \
    }

#endif // PropertyDlg_H
