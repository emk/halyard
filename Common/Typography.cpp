// -*- Mode: C++; tab-width: 4; -*-

#include <assert.h>
#include <wctype.h>

#include "Typography.h"

#define ASSERT(x) assert(x)

using namespace Typography;

inline Distance round_266 (FT_Pos in266Measurement)
{
	// XXX - Are all our values pre-fitted?
	return (in266Measurement + 32) >> 6;
}


//=========================================================================
//	Typography::Library Methods
//=========================================================================

Library::Library()
{
	Error::CheckResult(FT_Init_FreeType(&mLibrary));
}

Library::~Library()
{
	Error::CheckResult(FT_Done_FreeType(mLibrary));
}


//=========================================================================
//	Typography::Face Methods
//=========================================================================

Face::Face(Library &inLibrary, char *inFontFile, char *inMetricsFile,
		   int inSize)
	: AbstractFace(inSize)
{
	ASSERT(inFontFile != NULL);
	ASSERT(inSize > 0);

	// Open up our face, and any associated metrics.
	Error::CheckResult(FT_New_Face(inLibrary, inFontFile, 0, &mFace));
	if (inMetricsFile)
		Error::CheckResult(FT_Attach_File(mFace, inMetricsFile));
	
	// Attempt to set a Unicode charmap.
	Error::CheckResult(FT_Select_Charmap(mFace, ft_encoding_unicode));
	
	// Check to see if our font is either (1) scalable or (2) available
	// in the specified point size.	 We never attempt to scale bitmap
	// fonts; the results are gross.
	if (!FT_IS_SCALABLE(mFace))
	{
		bool found_size = false;
		for (FT_Int i = 0; i < mFace->num_fixed_sizes; i++) {
			if (mFace->available_sizes[i].height == inSize &&
				mFace->available_sizes[i].width == inSize)
			{
				found_size = true;
				break;
			}
		}
		// TODO - Add an error string here.
		if (!found_size)
			throw Error(Error::kOtherError);
	}

	// Set the size of our font.
	Error::CheckResult(FT_Set_Char_Size(mFace, inSize*64, inSize*64, 72, 72));

	// Set various font properties that we'll need later.
	mHasKerning = FT_HAS_KERNING(mFace);
}

Face::~Face()
{
	Error::CheckResult(FT_Done_Face(mFace));
}
		
GlyphIndex Face::GetGlyphIndex(CharCode inCharCode)
{
	if (inCharCode == kNoSuchCharacter)
		return 0;
	else
		return FT_Get_Char_Index(mFace, inCharCode);
}

Glyph Face::GetGlyphFromGlyphIndex(GlyphIndex inGlyphIndex)
{
	Error::CheckResult(FT_Load_Glyph(mFace, inGlyphIndex,
									 FT_LOAD_RENDER /*| FT_LOAD_MONOCHROME*/));
	return mFace->glyph;
}

Glyph Face::GetGlyph(CharCode inCharCode)
{
	return GetGlyphFromGlyphIndex(GetGlyphIndex(inCharCode));
}

Vector Face::GetKerning(CharCode inPreviousChar,
						CharCode inCurrentChar)
{
	FT_Vector delta;
	GlyphIndex previous_glyph = GetGlyphIndex(inPreviousChar);
	GlyphIndex current_glyph = GetGlyphIndex(inCurrentChar);

	if (mHasKerning && previous_glyph && current_glyph)
	{	
		// If we actually have kerning data, use it.
		Error::CheckResult(FT_Get_Kerning(mFace,
										  previous_glyph, current_glyph,
										  ft_kerning_default, &delta));
	}
	else
	{
		// Otherwise, don't kern the text.
		delta.x = 0;
		delta.y = 0;
	}

	return delta;
}

Distance Face::GetLineHeight()
{
	return round_266(mFace->size->metrics.height);
}


//=========================================================================
//	Typography::FaceStack Methods
//=========================================================================

FaceStack::FaceStack(Face *inPrimaryFace)
	: AbstractFace(inPrimaryFace->GetSize())
{
	mFaceStack.push_back(inPrimaryFace);
}

FaceStack::~FaceStack()
{
	// TODO - Figure out memory management.
}

void FaceStack::AddSecondaryFace(Face *inFace)
{
	ASSERT(GetSize() == inFace->GetSize());
	mFaceStack.push_back(inFace);
}

Glyph FaceStack::GetGlyph(CharCode inCharCode)
{
	Face *face;
	GlyphIndex glyph;
	SearchForCharacter(inCharCode, &face, &glyph);
	return face->GetGlyphFromGlyphIndex(glyph);
}

Vector FaceStack::GetKerning(CharCode inPreviousChar,
							 CharCode inCurrentChar)
{
	// TODO - Do real kerning.
	return mFaceStack.front()->GetKerning(inPreviousChar, inCurrentChar);
}

Distance FaceStack::GetLineHeight()
{
	// Use the line-height of our primary face.
	return mFaceStack.front()->GetLineHeight();
}

void FaceStack::SearchForCharacter(CharCode inCharCode,
								   Face **outFace,
								   GlyphIndex *outGlyphIndex)
{
	// Search for the first font which has a glyph for our char code.
	for (deque<Face*>::iterator iter = mFaceStack.begin();
		 iter < mFaceStack.end(); iter++)
	{
		*outFace = *iter;
		*outGlyphIndex = (*outFace)->GetGlyphIndex(inCharCode);
		if (*outGlyphIndex != 0)
			return;
	}

	// If we didn't find anything, return the default glyph from our
	// primary face.
	*outFace = mFaceStack.front();
	*outGlyphIndex = 0;
}


//=========================================================================
//	Typography::LineSegment(Iterator) Methods
//=========================================================================

bool Typography::operator==(const LineSegment &left, const LineSegment &right)
{
	return (left.begin == right.begin &&
			left.end == right.end &&
			left.isLineBreak == right.isLineBreak &&
			left.discardAtEndOfLine == right.discardAtEndOfLine &&
			left.needsHyphenAtEndOfLine == right.needsHyphenAtEndOfLine);
}

LineSegmentIterator::LineSegmentIterator(const wchar_t *inTextBegin,
										 const wchar_t *inTextEnd)
{
	ASSERT(inTextBegin != NULL);
	ASSERT(inTextEnd != NULL);

	// Initialize our iterator.
	mSegmentBegin = inTextBegin;
	mTextEnd = inTextEnd;
}

bool LineSegmentIterator::NextElement(LineSegment *outSegment)
{
	ASSERT(outSegment != NULL);
	
	// Skip past any leading soft hyphens.	(They're merely invisible
	// segment-breaking hints, and therefore meaningless at the
	// beginning of a segment.)
	while (mSegmentBegin < mTextEnd && *mSegmentBegin == kSoftHyphen)
		mSegmentBegin++;

	// If we don't have any more text, give up now.
	if (mSegmentBegin >= mTextEnd)
		return false;

	// Figure out what kind of segment to process next.
	const wchar_t *cursor = mSegmentBegin;
	if (*cursor == '\n')
	{
		// NEWLINE SEGMENT
		// Include just the newline in the segment.
		cursor++;

		// Describe our segment & update our state.
		outSegment->SetLineSegment(mSegmentBegin, cursor, true);
		mSegmentBegin = cursor; 
	}
	else if (iswspace(*cursor))
	{
		// WHITESPACE SEGMENT
		// Scan forward until we find the end of the whitespace.
		while (cursor < mTextEnd && iswspace(*cursor) && *cursor != '\n')
			cursor++;

		// Describe our segment & update our state.
		outSegment->SetLineSegment(mSegmentBegin, cursor, false, true);
		mSegmentBegin = cursor; 
	}
	else
	{
		// TEXT SEGMENT
		// Scan forward until we find the end of the current word or
		// a line-break character (e.g., '-').	Soft hyphens are tricky.
		while (cursor < mTextEnd && !iswspace(*cursor) &&
			   *cursor != '-' && *cursor != kSoftHyphen)
			cursor++;

		// Adjust our stopping point and set up some flags (as needed).
		bool needHyphenAtEndOfLine = false;
		if (cursor < mTextEnd)
		{
			needHyphenAtEndOfLine = (*cursor == kSoftHyphen);
			if (*cursor == '-')
				cursor++;
		}
		
		// Describe our segment & update our state.
		outSegment->SetLineSegment(mSegmentBegin, cursor, false, false,
								   needHyphenAtEndOfLine);
		mSegmentBegin = cursor;
	}

	return true;
}


//=========================================================================
//	Typography::GenericTextRenderingEngine Methods
//=========================================================================

// A miscellaneous local helper function to get either a pointer
// to the last element of a deque, or NULL if no such element exists.
template <class C>
static C* back_or_null(std::deque<C> &d)
{
	if (d.empty())
		return NULL;
	else
		return &d.back();
}

GenericTextRenderingEngine::
GenericTextRenderingEngine(const wchar_t *inTextBegin,
						   const wchar_t *inTextEnd,
						   Distance inLineLength,
						   Justification inJustification)
	: mIterator(inTextBegin, inTextEnd),
	  mLineLength(inLineLength), mJustification(inJustification)
{
	// We can't call RenderText from the constructor because C++ hasn't
	// bothered to initialize our vtables yet.
}
		
Distance GenericTextRenderingEngine::
CalculateHorizontalOffset(Distance inSpaceUsed)
{
	ASSERT(inSpaceUsed <= GetLineLength());
	Distance remaining = GetLineLength() - inSpaceUsed;
	switch (GetJustification())
	{
		case kLeftJustification:   return 0;
		case kCenterJustification: return remaining / 2;
		case kRightJustification:  return remaining;
	}
	ASSERT(false);
	return 0;	 
}

void GenericTextRenderingEngine::
RenderAndResetLine(std::deque<LineSegment> *ioLine)
{
	// Discard trailing whitespace segments.
	while (!ioLine->empty() && ioLine->back().discardAtEndOfLine)
		ioLine->pop_back();

	// Calculate justification for the line.
	Distance space_used = 0;
	if (!ioLine->empty())
		space_used = ioLine->back().userDistanceData;
	Distance offset = CalculateHorizontalOffset(space_used);

	// Render the line and empty our deque for the next line.
	RenderLine(ioLine, offset);
	ioLine->clear();
}

void GenericTextRenderingEngine::RenderText()
{
	LineSegment seg;
	std::deque<LineSegment> current_line;
	
	Distance space_used = 0;
	while (mIterator.NextElement(&seg)) {
		
		// Handle line breaks.
		if (seg.isLineBreak) {
			RenderAndResetLine(&current_line);
			space_used = 0;
			continue;
		}
		
		// If the segment won't fit, and we already have segments on this
		// line, then start a new line.
		Distance needed =
			MeasureSegment(back_or_null(current_line), &seg, true);
		if (needed > (GetLineLength() - space_used) && space_used > 0)
		{
			// Render what we've got.
			RenderAndResetLine(&current_line);
			space_used = 0;

			// Our segment didn't fit on the current line
			// AND it disappears at the end of a line.
			// So let's go straight to the next segment.
			if (seg.discardAtEndOfLine)
				continue;
		}

		// If the segment *still* won't fit, take drastic measures.
		// This is an ugly wart, and it isn't merged well into the
		// overall algorithm.  But isolating this ugly wart seems to
		// keep the rest of the code clean.
		while (needed > GetLineLength())
		{
			ASSERT(space_used == 0 && current_line.empty());
			LineSegment extracted;
			ExtractOneLine(&seg, &extracted);
			extracted.userDistanceData =
				MeasureSegment(back_or_null(current_line), &extracted, true);
			current_line.push_back(extracted);
			RenderAndResetLine(&current_line);
			needed = MeasureSegment(back_or_null(current_line), &seg, true);
		}
		
		// Add the segment to our current line (and cache how long the
		// line would be if we broke it right here).
		seg.userDistanceData = space_used + needed;
		current_line.push_back(seg);
	
		// Update our state.
		space_used += MeasureSegment(back_or_null(current_line), &seg, false);
	}
	RenderAndResetLine(&current_line);
}


//=========================================================================
//	Typography::TextRenderingEngine Methods
//=========================================================================

TextRenderingEngine::TextRenderingEngine(const wchar_t *inTextBegin,
										 const wchar_t *inTextEnd,
										 AbstractFace *inFace,
										 Point inPosition,
										 Distance inLineLength,
										 Justification inJustification)
	: GenericTextRenderingEngine(inTextBegin, inTextEnd,
								 inLineLength, inJustification),
	  mFace(inFace), mLineStart(inPosition)
{
	ASSERT(inFace);
	ASSERT(inPosition.x >= 0 && inPosition.y >= 0);
}

Distance TextRenderingEngine::MeasureSegment(LineSegment *inPrevious,
											 LineSegment *inSegment,
											 bool inAtEndOfLine)
{
	// Attempt to get the last glyph of the previous segment for
	// kerning purposes.  Default to 0, which is the built-in code for
	// "no glyph".
	GlyphIndex previous_char = kNoSuchCharacter;
	if (inPrevious)
	{
		ASSERT(inPrevious->begin < inPrevious->end);
		previous_char = *(inPrevious->end - 1);
	}

	// Measure the segment.
	// TODO - Merge with code below?
	Distance total = 0;
	for (const wchar_t *cp = inSegment->begin; cp < inSegment->end; cp++)
	{
		CharCode current_char = *cp;

		// Do our kerning.
		Vector delta = mFace->GetKerning(previous_char, current_char);
		total += delta.x >> 6; // Don't need round_266 (already fitted).
		ASSERT(delta.y == 0);

		// Load and measure our glyph.
		Glyph glyph = mFace->GetGlyph(current_char);
		total += round_266(glyph->advance.x);
		ASSERT(glyph->advance.y == 0);

		// Update our previous char.
		previous_char = current_char;
	}

	// If necessary, add a trailing hyphen.
	// TODO - Factor our common code shared with above loop.
	if (inAtEndOfLine && inSegment->needsHyphenAtEndOfLine)
	{
		CharCode current_char = '-';
		
		// Do our kerning.
		Vector delta = mFace->GetKerning(previous_char, current_char);
		total += delta.x >> 6; // Don't need round_266 (already fitted).
		ASSERT(delta.y == 0);

		// Load and measure our glyph.
		Glyph glyph = mFace->GetGlyph(current_char);
		total += round_266(glyph->advance.x);
		ASSERT(glyph->advance.y == 0);
	}

	ASSERT(total >= 0);
	return total;
}

void TextRenderingEngine::ExtractOneLine(LineSegment *ioRemaining,
										 LineSegment *outExtracted)
{
	// XXX - Not yet implemented.
	throw Error(Error::kOtherError);
}

void TextRenderingEngine::RenderLine(std::deque<LineSegment> *inLine,
									 Distance inHorizontalOffset)
{
	// Figure out where to start drawing text.
	Point cursor = mLineStart;
	cursor.x += inHorizontalOffset;

	// Draw each character.
	GlyphIndex previous_char = kNoSuchCharacter;
	for (std::deque<LineSegment>::iterator iter = inLine->begin();
		 iter < inLine->end(); iter++)
	{
		// TODO - Factor out common code with MeasureSegment?
		LineSegment seg = *iter;
		for (const wchar_t *cp = seg.begin; cp < seg.end; cp++)
		{
			GlyphIndex current_char = *cp;
			
			// Do our kerning.
			Vector delta = mFace->GetKerning(previous_char, current_char);
			cursor.x += delta.x >> 6; // Don't need round_266 (already fitted).
			ASSERT(cursor.x >= 0);
			ASSERT(delta.y == 0);

			// Load and draw our glyph.
			Glyph glyph = mFace->GetGlyph(current_char);
			Point loc = cursor;
			loc.x += glyph->bitmap_left;
			loc.y -= glyph->bitmap_top;
			DrawBitmap(&glyph->bitmap, loc);

			// Advance our cursor.
			cursor.x += round_266(glyph->advance.x);
			ASSERT(cursor.x >= 0);
			ASSERT(glyph->advance.y == 0);

			// Update our previous char.
			previous_char = current_char;
		}
	}

	// Draw a trailing hyphen if we need one.
	// XXX - (More ridiculous duplication.)
	if (!inLine->empty() && inLine->back().needsHyphenAtEndOfLine)
	{
		GlyphIndex current_char = '-';
			
		// Do our kerning.
		Vector delta = mFace->GetKerning(previous_char, current_char);
		cursor.x += delta.x >> 6; // Don't need round_266 (already fitted).
		ASSERT(cursor.x >= 0);
		ASSERT(delta.y == 0);

		// Load and draw our glyph.
		Glyph glyph = mFace->GetGlyph(current_char);
		Point loc = cursor;
		loc.x += glyph->bitmap_left;
		loc.y -= glyph->bitmap_top;
		DrawBitmap(&glyph->bitmap, loc);

		// Advance our cursor.
		cursor.x += round_266(glyph->advance.x);
		ASSERT(cursor.x >= 0);
		ASSERT(glyph->advance.y == 0);
	}
	
	// Update our line start for the next line.
	mLineStart.y += mFace->GetLineHeight();
}
