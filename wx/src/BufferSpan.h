// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
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

#ifndef BufferSpan_H
#define BufferSpan_H

/// Marks a floating "span" in a text edit buffer.  This is used to
/// internally to implement various kinds of location memory for search &
/// replace, error reporting, etc.
class BufferSpan {
    friend class BufferSpanTable;

public:
    typedef int Id;  ///< Unique ID code for a span.
    typedef int Pos; ///< A buffer position.

    enum Status {
        /// The text within the span is pristine.
        UNCHANGED,
        /// The text within the span has been edited, but no edits crossed
        /// the endpoints of the span.
        EDITED,
        /// Edits have partially invalidated one or both endpoints of the
        /// span.
        CLOBBERED  
    };

private:
    Id mId;
    Pos mBeginPos;
    Pos mEndPos;
    Status mStatus;

    void ProcessInsertion(BufferSpan::Pos pos, size_t length);
    void ProcessDeletion(BufferSpan::Pos pos, size_t length);
    bool IsPosInRange(Pos pos, Pos begin, Pos end) const;

public:
    BufferSpan(Id id, Pos begin, Pos end = -1);

    Id GetId() const { return mId; }
    Pos GetBeginPos() const { return mBeginPos; }
    Pos GetEndPos() const { return mEndPos; }
    size_t GetLength() const { return mEndPos - mBeginPos; }
    Status GetStatus() const { return mStatus; }
};

/// A table of BufferSpan objects.
class BufferSpanTable : boost::noncopyable {
    typedef std::vector<BufferSpan> SpanList;
    SpanList mSpans;

public:
    BufferSpanTable();
    void SetSpan(const BufferSpan &span);
    void DeleteSpanIfExists(BufferSpan::Id id);
    const BufferSpan *FindSpan(BufferSpan::Id id) const;
    const BufferSpan *GetSpan(BufferSpan::Id id) const;

    void ProcessInsertion(BufferSpan::Pos pos, size_t length);
    void ProcessDeletion(BufferSpan::Pos pos, size_t length);
};

#endif // BufferSpan_H
