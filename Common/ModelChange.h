// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (ModelChange_H)
#define ModelChange_H

//////////
// API for modifying the model in an undoable/redoable fashion.
//
namespace model {

	//////////
	// A Change represents a mutation of something within the Model.  A
	// Change may be applied, or reverted.  Changes to a given Model occur
	// in a sequence, and must be applied or reverted in that sequence.
	//
	// Because of this careful sequencing, a Change is allowed to hold onto
	// pointers and other resources.  All Change objects are destroyed
	// before the corresponding Model object is destroyed.  This means
	// that half their resources will be owned by the Model, and half
	// by the Change object, depending on whether the change has been
	// applied or reverted.
	//
	class Change {
		bool mIsApplied;
		bool mIsFreed;
		
	public:
		Change();
		virtual ~Change();
		
		void Apply();
		void Revert();
		void FreeResources();
		
	protected:	
		//////////
		// If an error occurs in the constructor, free any resources
		// allocated by the object, call this function, and throw an
		// exception.
		//
		void ConstructorFailing() { mIsFreed = true; }

		//////////
		// Wrapper around CollectionDatum::DoFind which can be called
		// from subclasses of Change.  The allows us to keep the
		// low-level collection 'protected:' without relying on
		// nesting Change classes within the classes they modify
		// (which doesn't work under MSVC++).
		//
		template <typename Collection>
		Datum *Find(Collection *inCollection,
					typename Collection::ConstKeyType &inKey)
			{ return inCollection->DoFind(inKey); }

		//////////
		// Wrapper around CollectionDatum::DoRemoveKnown which can be
		// called from subclasses of Change.  See above.
		//
		template <typename Collection>
		void RemoveKnown(Collection *inCollection,
						 typename Collection::ConstKeyType &inKey,
						 Datum *inDatum)
			{ inCollection->DoRemoveKnown(inKey, inDatum); }

		//////////
		// Wrapper around CollectionDatum::DoInsert which can be called
		// from subclasses of Change.  See above.
		//
		template <typename Collection>
		void Insert(Collection *inCollection,
					typename Collection::ConstKeyType &inKey,
					Datum *inDatum)
			{ inCollection->DoInsert(inKey, inDatum); }

		//////////
		// Apply this change.  This method is called when the change first
		// occurs, and perhaps later on, when the change is redone.  This
		// method must be atomic--if it fails, it must leave the object
		// unchanged and throw an exception.
		//
		virtual void DoApply() = 0;
		
		//////////
		// Revert this change.  This method is called when the change is
		// undone.  This method must be atomic--if it fails, it must leave
		// the object unchanged and throw an exception.
		// 
		virtual void DoRevert() = 0;
		
		//////////
		// Free any resources need to apply this Change, because this
		// Change is being destroyed and will not need to be applied again.
		//
		virtual void DoFreeApplyResources() = 0;

		//////////
		// Free any resources need to revert this Change, because this
		// Change is being destroyed and will not need to be reverted
		// again.
		//
		virtual void DoFreeRevertResources() = 0;
	};

	//////////
	// This class is used to set the value associated with a given key.
	//
	template <typename KeyType>
	class SetChange : public Change {
		typedef const KeyType ConstKeyType;

		CollectionDatum<KeyType> *mCollection;
		Datum *mOldDatum;
		Datum *mNewDatum;
		ConstKeyType mKey;
		
	public:
		SetChange(CollectionDatum<KeyType> *inDatum,
				  ConstKeyType &inKey,
				  Datum *inValue);
		
	protected:
		virtual void DoApply();
		virtual void DoRevert();
		virtual void DoFreeApplyResources();
		virtual void DoFreeRevertResources();
	};

	//////////
	// This class is used to delete the value associated with a given key.
	//
	template <typename KeyType>
	class DeleteChange : public Change
	{
		typedef const KeyType ConstKeyType;

		CollectionDatum<KeyType> *mCollection;
		Datum *mOldDatum;
		ConstKeyType mKey;
		
	public:
		DeleteChange(CollectionDatum<KeyType> *inDatum,
					 ConstKeyType &inKey);
		
	protected:
		virtual void DoApply();
		virtual void DoRevert();
		virtual void DoFreeApplyResources();
		virtual void DoFreeRevertResources();
	};

	//////////
	// This class is used to insert a value into a list at the given
	// location.
	//
	class InsertChange : public Change {
		typedef const List::ConstKeyType ConstKeyType;

		List *mCollection;
		Datum *mNewDatum;
		ConstKeyType mKey;
		
	public:
		InsertChange(List *inCollection,
					 ConstKeyType &inKey,
					 Datum *inValue);
		
	protected:
		virtual void DoApply();
		virtual void DoRevert();
		virtual void DoFreeApplyResources();
		virtual void DoFreeRevertResources();
	};
};

#endif // ModelChange_H
