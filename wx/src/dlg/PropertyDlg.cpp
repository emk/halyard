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

#include "TTemplateUtils.h"
#include "PropertyDlg.h"
#include "Model.h"


//=========================================================================
//  PropertyDlg::Description Methods
//=========================================================================

PropertyDlg::Description::Description(const char *inName)
	: name(inName)
{
}

void PropertyDlg::Description::AddField(const Field &inField)
{
	fields.push_back(inField);
}


//=========================================================================
//  ModelValidator
//=========================================================================

class ModelStringValidator : public wxValidator
{
	model::Object *mObject;
	std::string mMember;

public:
	ModelStringValidator(model::Object *inObject, const std::string &inMember);
	ModelStringValidator(const ModelStringValidator &inValidator);

	virtual wxObject *Clone() const;
	virtual bool TransferToWindow();	
	virtual bool TransferFromWindow();
	virtual bool Validate(wxWindow* inParent);
};

ModelStringValidator::ModelStringValidator(model::Object *inObject,
										   const std::string &inMember)
	: mObject(inObject), mMember(inMember)
{
}

ModelStringValidator::ModelStringValidator(
	const ModelStringValidator &inValidator)
	: // XXX - wxValidator(inValidator) is private!
	  mObject(inValidator.mObject),
	  mMember(inValidator.mMember)
{
}

wxObject *ModelStringValidator::Clone() const
{
	return new ModelStringValidator(*this);
}

bool ModelStringValidator::TransferToWindow()
{
	wxTextCtrl *text_ctrl = dynamic_cast<wxTextCtrl*>(GetWindow());
	wxASSERT(text_ctrl);
	text_ctrl->SetValue(mObject->GetString(mMember).c_str());
	return true;
}

bool ModelStringValidator::TransferFromWindow()
{
	wxTextCtrl *text_ctrl = dynamic_cast<wxTextCtrl*>(GetWindow());
	wxASSERT(text_ctrl);

	// Only transfer the text back if something has changed; this
	// avoids garbaging up our Undo history and dirty bit with
	// pointless changes.
	std::string old_string = mObject->GetString(mMember);
	std::string new_string = text_ctrl->GetValue().mb_str();
	if (new_string != old_string)
		mObject->SetString(mMember, new_string);
	return true;
}

bool ModelStringValidator::Validate(wxWindow* inParent)
{
	return true;
}


//=========================================================================
//  PropertyDlg Methods
//=========================================================================

BEGIN_EVENT_TABLE(PropertyDlg, wxDialog)
END_EVENT_TABLE()

PropertyDlg::PropertyDlg(wxWindow *inParent,
						 Description *inDescription,
						 model::Object *inObject)
	: wxDialog(inParent, -1, inDescription->name, wxDefaultPosition),
	  mObject(inObject)
{
	// Configure our sizers.
	wxBoxSizer *master_sizer = new wxBoxSizer(wxVERTICAL);
	mPropSizer = new wxFlexGridSizer(inDescription->fields.size(), 2, 10, 10);
	wxFlexGridSizer *bttn_sizer = new wxFlexGridSizer(1, 4, 10, 10);
	master_sizer->Add(mPropSizer, 0 /* stretch */, wxEXPAND|wxALL, 10);
	master_sizer->Add(bttn_sizer, 0 /* no stretch */,
					  wxALIGN_RIGHT|wxLEFT|wxRIGHT|wxBOTTOM, 10);

	// Add our property controls.
	std::vector<Field> *fields = &inDescription->fields;
	std::vector<Field>::iterator i = fields->begin();
	for (; i != fields->end(); ++i)
		AddField(*i);

	// Add our buttons.
	mOkButton = new wxButton(this, wxID_OK, "OK");
	mCancelButton = new wxButton(this, wxID_CANCEL, "Cancel");
	mApplyButton = new wxButton(this, wxID_APPLY, "&Apply");
	mHelpButton = new wxButton(this, wxID_HELP, "&Help");
	bttn_sizer->Add(mOkButton, 0, 0, 0);
	bttn_sizer->Add(mCancelButton, 0, 0, 0);
	bttn_sizer->Add(mApplyButton, 0, 0, 0);
	bttn_sizer->Add(mHelpButton, 0, 0, 0);
	mOkButton->SetDefault();
	mHelpButton->Disable();

	SetSizer(master_sizer);
	Layout();
	master_sizer->SetSizeHints(this);
    Centre();
}

void PropertyDlg::AddField(Field &inField)
{
	if (inField.flags == MULTILINE)
	{
		wxStaticText *label = new wxStaticText(this, -1, inField.label);
		wxTextCtrl *edit =
			new wxTextCtrl(this, -1, "", wxDefaultPosition, wxSize(300, 200),
						   wxTE_MULTILINE,
						   ModelStringValidator(mObject, inField.name));
		mPropSizer->Add(label, 0, 0, 0);
		mPropSizer->Add(edit, 1, wxEXPAND, 0);
	}
	else
	{
		wxStaticText *label = new wxStaticText(this, -1, inField.label);
		wxTextCtrl *edit =
			new wxTextCtrl(this, -1, "", wxDefaultPosition, wxSize(300, -1), 0,
						   ModelStringValidator(mObject, inField.name));
		mPropSizer->Add(label, 0, wxALIGN_CENTRE_VERTICAL, 0);
		mPropSizer->Add(edit, 1, wxEXPAND, 0);
	}
}
