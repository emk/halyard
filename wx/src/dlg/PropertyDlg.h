// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#ifndef PropertyDlg_H
#define PropertyDlg_H

namespace model { class Object; };

//////////
// This is the dialog which pops up at startup time and asks you whether
// you want to open an existing program or create a new one.
//
class PropertyDlg : public wxDialog
{
public:
	typedef unsigned long Flags;
	enum /* Flags */ {
		MULTILINE = 1
	};

	class Field {
	public:
		Field(const char *inName, const char *inLabel, Flags inFlags)
			: name(inName), label(inLabel), flags(inFlags) {}

		const char *name;
		const char *label;
		Flags flags;
	};

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
