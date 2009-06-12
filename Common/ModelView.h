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

#if !defined (ModelView_H)
#define ModelView_H

namespace model {

    class Object;

    /// The 'view' class of the model/view/control triad.  This View
    /// associates itself with an Object in the model.  Every time
    /// that Object changes the View receives an ObjectChanged
    /// message.  When the Object is deleted, the view receives
    /// an ObjectDeleted message.
    class View : boost::noncopyable {
        Object *mObject;
        bool mObjectIsLive;

    public:
        View();
        virtual ~View();

        /// Returns true if and only if there is an object attached and
        /// that object is not currently deleted.
        bool ObjectIsLive() { return mObjectIsLive; }

        /// Call this method to permanently attach an Object to this View.
        void SetObject(Object *inObject);

        /// Get the object associated with this view.  This function can only
        /// be called when ObjectIsLive is true.
        Object *GetObject();

        /// This method will be called immediately after the Object is
        /// first attached, every time the object changes, and when the
        /// object is "undeleted" by an Undo or Redo.  ObjectIsLive
        /// will always be true.
        virtual void ObjectChanged() = 0;

        /// This method will be called when the object is deleted.
        /// ObjectIsLive will always be false.
        virtual void ObjectDeleted() = 0;

    private:
        friend class Object;

        void CallObjectChanged();
        void CallObjectDeleted();
        void ClearObject();
    };
};

#endif // ModelView_H
