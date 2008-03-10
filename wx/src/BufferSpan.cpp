// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-
// @BEGIN_LICENSE
//
// Halyard - Multimedia authoring and playback system
// Copyright 1993-2008 Trustees of Dartmouth College
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
#include "BufferSpan.h"

using namespace Halyard;

REGISTER_TEST_CASE_FILE(BufferSpan);


//=========================================================================
//  BufferSpan Methods
//========================================================================

/// Create a new buffer span, using an \tt id value of your choice.  If you
/// omit the \tt end parameter, it defaults to the value of the \tt begin
/// parameter.
BufferSpan::BufferSpan(Id id, Pos begin, Pos end)
    : mId(id), mBeginPos(begin), mStatus(UNCHANGED)
{
    if (end == -1)
        mEndPos = mBeginPos;
    else
        mEndPos = end;
}

void BufferSpan::ProcessInsertion(BufferSpan::Pos pos, size_t length) {
    if (mEndPos <= pos) {
        // The insertion is after us.  Do nothing.  (We test this first to
        // handle insertions *at* zero-length spans the way we want.)
    } else if (pos <= mBeginPos) {
        // The insertion is before us.
        mBeginPos += length;
        mEndPos += length;
    } else if (mBeginPos < pos && pos < mEndPos) {
        // The insertion falls within our range.
        mEndPos += length;
        if (mStatus == UNCHANGED)
            mStatus = EDITED;
    } else {
        // We really shouldn't get here.
        ASSERT(0);
    }
}

void BufferSpan::ProcessDeletion(BufferSpan::Pos pos, size_t length) {
    Pos del_begin = pos;
    Pos del_end = pos + length;
    if (del_end <= mBeginPos) {
        // The deletion occurred before us. 
        mBeginPos -= length;
        mEndPos -= length;
    } else if (mEndPos <= del_begin) {
        // The deletion occurred after us.  Do nothing.
    } else if (IsPosInRange(del_begin, mBeginPos, mEndPos) &&
               IsPosInRange(del_end, mBeginPos, mEndPos)) {
        // The deletion occurred entirely within our range.
        mEndPos -= length;
        if (mStatus == UNCHANGED)
            mStatus = EDITED;
    } else if (IsPosInRange(mBeginPos, del_begin, del_end) &&
               IsPosInRange(mEndPos, del_begin, del_end)) {
        // Our range falls entirely within the deletion.
        mBeginPos = del_begin;
        mEndPos = del_begin;
        mStatus = CLOBBERED;
    } else if (IsPosInRange(mBeginPos, del_begin, del_end) &&
               !IsPosInRange(mEndPos, del_begin, del_end)) {
        // The deletion overlaps the start of our range only.
        mBeginPos = del_begin;
        mEndPos -= length;
        mStatus = CLOBBERED;
    } else if (!IsPosInRange(mBeginPos, del_begin, del_end) &&
               IsPosInRange(mEndPos, del_begin, del_end)) {
        // The deletion overlaps the end of our range only.
        mEndPos = del_begin;
        mStatus = CLOBBERED;
    } else {
        // We really shouldn't get here.
        ASSERT(0);
    }
}

/// Compare a point from one range to another range.  Even though ranges
/// normally exclude their last bound (like in C++), this particular
/// comparison includes the last bound because that what we want when
/// \tt pos is itself a range delimiter.
bool BufferSpan::IsPosInRange(Pos pos, Pos begin, Pos end) const {
    return begin <= pos && pos <= end;
}


//=========================================================================
//  BufferSpanTable Methods
//========================================================================

/// Create a new BufferSpanTable.
BufferSpanTable::BufferSpanTable() {
}

/// Store the specified span in the table, replacing any existing
/// with the same ID.
void BufferSpanTable::SetSpan(const BufferSpan &span) {
    DeleteSpanIfExists(span.GetId());
    mSpans.push_back(span);
}

/// Delete the span with the specified id, if it exists.
void BufferSpanTable::DeleteSpanIfExists(BufferSpan::Id id) {
    for (SpanList::iterator i = mSpans.begin(); i != mSpans.end(); ++i) {
        if (i->GetId() == id) {
            mSpans.erase(i);
            return;
        }
    }
}

/// Retuns the span with the specified ID, or NULL.  The returned pointer
/// is invalidated by any non-const operation on the SpanTable.
const BufferSpan *BufferSpanTable::FindSpan(BufferSpan::Id id) const {
    SpanList::const_iterator i = mSpans.begin();
    for (; i != mSpans.end(); ++i)
        if (i->GetId() == id)
            return &(*i);
    return NULL;
}

/// Returns the span with the specified ID.  The result is undefined if
/// the span does not exist.
const BufferSpan *BufferSpanTable::GetSpan(BufferSpan::Id id) const {
    const BufferSpan *span = FindSpan(id);
    ASSERT(span != NULL);
    return span;
}

/// Update all the spans to take the given insertion into account.  This
/// will typically be bound to some kind of editor callback.
void BufferSpanTable::ProcessInsertion(BufferSpan::Pos pos, size_t length) {
    for (SpanList::iterator i = mSpans.begin(); i != mSpans.end(); ++i)
        i->ProcessInsertion(pos, length);
}

/// Update all the spans to take the given deletion into account.  This
/// will typically be bound to some kind of editor callback.
void BufferSpanTable::ProcessDeletion(BufferSpan::Pos pos, size_t length) {
    for (SpanList::iterator i = mSpans.begin(); i != mSpans.end(); ++i)
        i->ProcessDeletion(pos, length);
}


//=========================================================================
//  Tests
//=========================================================================

#if BUILD_TEST_CASES

namespace {
    enum { TEST_SPAN = 42 };
    
    void CHECK_SPAN(const BufferSpanTable &table,
                    BufferSpan::Pos begin, BufferSpan::Pos end,
                    BufferSpan::Status status) {
        const BufferSpan *span = table.FindSpan(TEST_SPAN);
        CHECK_NE(span, static_cast<const BufferSpan *>(NULL));
        CHECK_EQ(span->GetBeginPos(), begin);
        CHECK_EQ(span->GetEndPos(), end);
        CHECK_EQ(span->GetStatus(), status);
    }
}

BEGIN_TEST_CASE(TestBufferSpan, TestCase) {
    BufferSpanTable table;
    const BufferSpan *span;

    // Make sure find does the right thing on an empty table.
    CHECK_EQ(table.FindSpan(TEST_SPAN),
             static_cast<const BufferSpan *>(NULL));

    // Create a zero-length span, insert it, find it, and check it out.
    table.SetSpan(BufferSpan(TEST_SPAN, 10));
    span = table.FindSpan(TEST_SPAN);
    CHECK_NE(span, static_cast<const BufferSpan *>(NULL));
    CHECK_EQ(span->GetId(), TEST_SPAN);
    CHECK_EQ(span->GetBeginPos(), 10);
    CHECK_EQ(span->GetEndPos(), 10);
    CHECK_EQ(span->GetLength(), 0);
    CHECK_EQ(span->GetStatus(), BufferSpan::UNCHANGED);

    // Create a non-zero span, insert it, find it, and check it out.
    table.SetSpan(BufferSpan(TEST_SPAN, 20, 25));
    span = table.FindSpan(TEST_SPAN);
    CHECK_NE(span, static_cast<const BufferSpan *>(NULL));
    CHECK_EQ(span->GetId(), TEST_SPAN);
    CHECK_EQ(span->GetBeginPos(), 20);
    CHECK_EQ(span->GetEndPos(), 25);
    CHECK_EQ(span->GetLength(), 5);
    CHECK_EQ(span->GetStatus(), BufferSpan::UNCHANGED);

    // Delete the span and make sure it goes away.
    table.DeleteSpanIfExists(TEST_SPAN);
    CHECK_EQ(table.FindSpan(TEST_SPAN),
             static_cast<const BufferSpan *>(NULL));

    // Test an insertion *before* our span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessInsertion(10, 7);
    CHECK_SPAN(table, 17, 27, BufferSpan::UNCHANGED);

    // Test insertions *in* our span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessInsertion(11, 7);
    CHECK_SPAN(table, 10, 27, BufferSpan::EDITED);
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessInsertion(19, 7);
    CHECK_SPAN(table, 10, 27, BufferSpan::EDITED);

    // Test insertions *after* our span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessInsertion(20, 7);
    CHECK_SPAN(table, 10, 20, BufferSpan::UNCHANGED);

    // Test insertions *at* zero-length span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10));
    table.ProcessInsertion(10, 5);
    CHECK_SPAN(table, 10, 10, BufferSpan::UNCHANGED);

    // Test deletions *before* our span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(5, 5);
    CHECK_SPAN(table, 5, 15, BufferSpan::UNCHANGED);
    
    // Test deletions *after* our span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(20, 5);
    CHECK_SPAN(table, 10, 20, BufferSpan::UNCHANGED);

    // Test deletions *within* our span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(10, 10);
    CHECK_SPAN(table, 10, 10, BufferSpan::EDITED);

    // Test deletions *surrounding* our span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(9, 12);
    CHECK_SPAN(table, 9, 9, BufferSpan::CLOBBERED);

    // Test deletions overlapping our span start.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(9, 10);
    CHECK_SPAN(table, 9, 10, BufferSpan::CLOBBERED);
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(9, 11);
    CHECK_SPAN(table, 9, 9, BufferSpan::CLOBBERED);

    // Test deletions overlapping our span end.
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(10, 11);
    CHECK_SPAN(table, 10, 10, BufferSpan::CLOBBERED);
    table.SetSpan(BufferSpan(TEST_SPAN, 10, 20));
    table.ProcessDeletion(11, 10);
    CHECK_SPAN(table, 10, 11, BufferSpan::CLOBBERED);

    // Test deletions *at* zero-length span.
    table.SetSpan(BufferSpan(TEST_SPAN, 10));
    table.ProcessDeletion(10, 5);
    CHECK_SPAN(table, 10, 10, BufferSpan::UNCHANGED);
    
    // Whew.  That should do it.

} END_TEST_CASE(TestBufferSpan);

#endif // BUILD_TEST_CASES
