// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (ModelView_H)
#define ModelView_H

namespace model {

    class Object;

	//////////
	// The 'view' class of the model/view/control triad.  This View
	// associates itself with an Object in the model.  Every time
	// that Object changes the View receives an ObjectChanged
	// message.  When the Object is deleted, the view receives
	// an ObjectDeleted message.
	//
	class View {
		DISABLE_COPY_AND_ASSIGN(View);

		Object *mObject;
		bool mObjectIsLive;

	public:
		View();
		virtual ~View();

		//////////
		// Returns true if and only if there is an object attached and
		// that object is not currently deleted.
		//
		bool ObjectIsLive() { return mObjectIsLive; }

		//////////
		// Call this method to permanently attach an Object to this View.
		//
		void SetObject(Object *inObject);

		//////////
		// Get the object associated with this view.  This function can only
		// be called when ObjectIsLive is true.
		//
		Object *GetObject();

		//////////
		// This method will be called immediately after the Object is
		// first attached, every time the object changes, and when the
		// object is "undeleted" by an Undo or Redo.  ObjectIsLive
		// will always be true.
		//
		virtual void ObjectChanged() = 0;

		//////////
		// This method will be called when the object is deleted.
		// ObjectIsLive will always be false.
		//
		virtual void ObjectDeleted() = 0;

	private:
		friend class Object;

		void CallObjectChanged();
		void CallObjectDeleted();
		void ClearObject();
	};
};

#endif // ModelView_H