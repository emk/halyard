// -*- Mode: C++; tab-width: 4; -*-

#include "TCommon.h"

#include <fstream>

#include <stdlib.h>
#include <assert.h>
#include <wctype.h>

#include "Typography.h"

using namespace Typography;

inline Distance round_266 (FT_Pos in266Measurement)
{
	// XXX - Are all our values pre-fitted?
	return (in266Measurement + 32) >> 6;
}


//=========================================================================
//	Typography::Error Methods
//=========================================================================

Error::Error(int inErrorCode)
{
	SetErrorCode(inErrorCode);
	SetErrorMessage("(no error strings, yet--try fterrors.h)");
}


//=========================================================================
//	Typography::Library Methods
//=========================================================================

Library *Library::sLibrary = NULL;

Library::Library()
{
	Error::CheckResult(FT_Init_FreeType(&mLibrary));
}

Library::~Library()
{
	Error::CheckResult(FT_Done_FreeType(mLibrary));
}

Library *Library::GetLibrary()
{
	if (!sLibrary)
		sLibrary = new Library();
	return sLibrary;
}


//=========================================================================
//	Typography::Style Methods
//=========================================================================
//  The 'StyleRep' code in this class is getting out of control, and should
//  probably be factored into a standard 5L 'Representation' class.

Style::Style(const std::string &inFamily, int inSize)
{
	mRep = new StyleRep();
	try
	{
		// Set up all our fields.
		mRep->mRefCount    = 1;
		mRep->mFamily      = inFamily;
		mRep->mFaceStyle   = kRegularFaceStyle;
		mRep->mSize        = inSize;
		mRep->mColor       = Color(0, 0, 0);
		mRep->mShadowColor = Color(255, 255, 255);
		mRep->mFace        = NULL;
	}
	catch (...)
	{
		delete mRep;
		throw;
	}
}

Style::Style(const Style &inStyle)
{
	// It's safe to cast away the const here because we always call
	// 'Grab' before modifying the representation.
	mRep = const_cast<StyleRep*>(inStyle.mRep);
	mRep->mRefCount++;
}

Style::~Style()
{
	// Only throw away the rep if we're the last reference.
	mRep->mRefCount--;
	if (mRep->mRefCount < 1)
	{
		InvalidateFace();
		delete mRep;
	}
}

void Style::Grab()
{
	// If we don't have our own copy, get one.
	if (mRep->mRefCount > 1)
	{
		StyleRep *oldRep = mRep;
		mRep = new StyleRep(*mRep);
		mRep->mRefCount = 1;
		mRep->mFace = NULL;

		// It's safe to decrement this only after we've updated 'mRep'.
		oldRep->mRefCount--;
	}
}

void Style::InvalidateFace()
{
	if (mRep->mFace)
	{
		delete mRep->mFace;
		mRep->mFace = NULL;
	}
}

Style &Style::operator=(const Style &inStyle)
{
	// Watch out for self-assignment.
	if (mRep == inStyle.mRep)
		return *this;

	// Delete our reference to our representation.
	mRep->mRefCount--;
	if (mRep->mRefCount < 1)
	{
		InvalidateFace();
		delete mRep;
	}

	// Make a new reference to inStyle's representation.
	mRep = inStyle.mRep;
	mRep->mRefCount++;
	return *this;
}

Style &Style::SetFamily(const std::string &inFamily)
{
	Grab();
	InvalidateFace();
	mRep->mFamily = inFamily;
	return *this;
}

Style &Style::SetBackupFamilies(const std::list<std::string> &inBFs)
{
	Grab();
	InvalidateFace();
	mRep->mBackupFamilies = inBFs;
	return *this;	
}

Style &Style::SetFaceStyle(FaceStyle inFaceStyle)
{
	Grab();
	InvalidateFace();
	mRep->mFaceStyle = inFaceStyle;
	return *this;
}

Style &Style::SetSize(int inSize)
{
	Grab();
	InvalidateFace();
	mRep->mSize = inSize;
	return *this;
}

Style &Style::SetColor(Color inColor)
{
	Grab();
	mRep->mColor = inColor;
	return *this;
}

Style &Style::SetShadowColor(Color inColor)
{
	Grab();
	mRep->mShadowColor = inColor;
	return *this;
}

AbstractFace *Style::GetFace() const
{
	if (!mRep->mFace)
	{
		// Load a face if we don't have one already.
		FamilyDatabase *db = FamilyDatabase::GetFamilyDatabase();
		FaceStyle intrinsic =
			(FaceStyle) (mRep->mFaceStyle & kIntrisicFaceStyles);
		Face base = db->GetFace(mRep->mFamily, intrinsic, mRep->mSize);
		mRep->mFace = new FaceStack(base);
		
		// Add our backup faces.
		for (std::list<std::string>::iterator i= mRep->mBackupFamilies.begin();
			 i != mRep->mBackupFamilies.end(); i++)
		{
			// TODO - Fix Secondary vs Backup naming inconsistency.
			Face backup = db->GetFace(*i, intrinsic, mRep->mSize);
			mRep->mFace->AddSecondaryFace(backup);
		}
	}
	return mRep->mFace;
}

bool Style::GetIsUnderlined() const
{
	return (mRep->mFaceStyle & kUnderlineFaceStyle) ? true : false;
}

bool Style::GetIsShadowed() const
{
	return (mRep->mFaceStyle & kShadowFaceStyle) ? true : false;
}


//=========================================================================
//	Typography::Face Methods
//=========================================================================

StyleInformation::StyleInformation(const Style &inBaseStyle)
	: mIsBuilt(false)
{
	mStyleRuns.push_back(StyleRun(0, inBaseStyle));
}

void StyleInformation::ChangeStyleAt(int inOffset, const Style &inStyle)
{
	ASSERT(!mIsBuilt);
	ASSERT(inOffset >= mStyleRuns.back().offset);
	if (mStyleRuns.back().offset == inOffset)
		mStyleRuns.pop_back();
	mStyleRuns.push_back(StyleRun(inOffset, inStyle));
}

void StyleInformation::EndStyleAt(int inOffset)
{
	ASSERT(!mIsBuilt);
	ASSERT(inOffset >= mStyleRuns.back().offset);
	if (mStyleRuns.back().offset == inOffset)
		mStyleRuns.pop_back();
	mIsBuilt = true;
	mEnd = inOffset;
}

StyleInformation::
const_iterator::const_iterator(const StyleInformation *inStyleInfo, bool isEnd)
	: mStyleInfo(inStyleInfo)
{
	if (isEnd)
	{
		mCurrentPosition = mStyleInfo->mEnd;
		mCurrentStyle = mStyleInfo->mStyleRuns.end(); // Useless
		mEndOfRun = mStyleInfo->mEnd;
	}
	else
	{
		mCurrentPosition = 0;
		mCurrentStyle = mStyleInfo->mStyleRuns.begin();
		UpdateEndOfRun();
	}
}
	
StyleInformation::const_iterator::const_iterator()
	: mStyleInfo(NULL), mCurrentPosition(0), mEndOfRun(0)
{
}

void StyleInformation::const_iterator::UpdateEndOfRun()
{
	if (mCurrentStyle == mStyleInfo->mStyleRuns.end())
	{
		mEndOfRun = mStyleInfo->mEnd;
	}
	else
	{
		StyleRunList::const_iterator next = mCurrentStyle;
		++next;
		if (next == mStyleInfo->mStyleRuns.end())
			mEndOfRun = mStyleInfo->mEnd;
		else
			mEndOfRun = next->offset;
	}
}

void StyleInformation::const_iterator::LoadNextStyleRun()
{
	ASSERT(mCurrentStyle != mStyleInfo->mStyleRuns.end());
	++mCurrentStyle;
	UpdateEndOfRun();
}

StyleInformation::const_iterator &
StyleInformation::const_iterator::operator--()
{
	ASSERT(mCurrentPosition > 0);
	--mCurrentPosition;
	if (mCurrentPosition < mCurrentStyle->offset)
	{
		--mCurrentStyle;
		UpdateEndOfRun();
	}
}


//=========================================================================
//	Typography::AbstractFace Methods
//=========================================================================

Vector AbstractFace::Kern(CharCode inChar1, AbstractFace *inFace1,
						  CharCode inChar2, AbstractFace *inFace2)
{
	Vector result;
	result.x = 0;
	result.y = 0;
	
	if (inFace1 != NULL && inFace2 != NULL)
	{
		Face *real_face1 = inFace1->GetRealFace(inChar1);
		Face *real_face2 = inFace2->GetRealFace(inChar2);
		if (*real_face1 == *real_face2)
			result = real_face1->GetKerning(inChar1, inChar2);
	}
	return result;
}


//=========================================================================
//	Typography::Face Methods
//=========================================================================

Face::Face(const char *inFontFile, const char *inMetricsFile, int inSize)
	: AbstractFace(inSize)
{
	ASSERT(inFontFile != NULL);
	ASSERT(inSize > 0);

	// Open up our face.
	// Until our FaceRep is successfully constructed, we're in charge
	// of calling FT_Done_Face on this data.
	FT_Face face;
	Error::CheckResult(FT_New_Face(*Library::GetLibrary(),
								   inFontFile, 0, &face));

	// Allocate a new FaceRep structure.  This takes ownership
	// of the face object.  Until the constructor exits successfully,
	// we're in change of calling delete on the FaceRep.
	try
	{
		mFaceRep = new FaceRep(face);
	}
	catch (...)
	{
		// Allocation failed, so finish using our face and bail.
		FT_Done_Face(face);
		throw;
	}

	try
	{
		// Attach our metrics, if we have any.
		if (inMetricsFile)
			Error::CheckResult(FT_Attach_File(face, inMetricsFile));
		
		// Attempt to set a Unicode charmap.
		Error::CheckResult(FT_Select_Charmap(face, ft_encoding_unicode));
		
		// Check to see if our font is either (1) scalable or (2) available
		// in the specified point size.	 We never attempt to scale bitmap
		// fonts; the results are gross.
		if (!FT_IS_SCALABLE(face))
		{
			bool found_size = false;
			for (FT_Int i = 0; i < face->num_fixed_sizes; i++) {
				if (face->available_sizes[i].height == inSize &&
					face->available_sizes[i].width == inSize)
				{
					found_size = true;
					break;
				}
			}
			if (!found_size)
				throw Error("Cannot scale bitmap font");
		}
		
		// Set the size of our font.
		Error::CheckResult(FT_Set_Char_Size(face, inSize*64, inSize*64,
											72, 72));

		// Set various font properties that we'll need later.
		// (Manual conversion to true, false to avoid MSVC++ warning.
		mHasKerning = (FT_HAS_KERNING(face) ? true : false);
	}
	catch (...)
	{
		delete mFaceRep;
		throw;
	}
}

Face::Face(const Face &inFace)
	: AbstractFace(inFace.GetSize())
{
	// THREAD - Not thread safe!
	inFace.mFaceRep->mRefcount++;
	mFaceRep = inFace.mFaceRep;
	mHasKerning = inFace.mHasKerning;
}

Face::~Face()
{
	// THREAD - Not thread safe!
	mFaceRep->mRefcount--;
	if (mFaceRep->mRefcount < 1)
		delete mFaceRep;
}

GlyphIndex Face::GetGlyphIndex(CharCode inCharCode)
{
	if (inCharCode == kNoSuchCharacter)
		return 0;
	else
		return FT_Get_Char_Index(mFaceRep->mFace, inCharCode);
}

Glyph Face::GetGlyphFromGlyphIndex(GlyphIndex inGlyphIndex)
{
	Error::CheckResult(FT_Load_Glyph(mFaceRep->mFace, inGlyphIndex,
									 FT_LOAD_RENDER /*| FT_LOAD_MONOCHROME*/));
	return mFaceRep->mFace->glyph;
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
		Error::CheckResult(FT_Get_Kerning(mFaceRep->mFace,
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
	return round_266(mFaceRep->mFace->size->metrics.height);
}


//=========================================================================
//	Typography::FaceStack Methods
//=========================================================================

FaceStack::FaceStack(const Face &inPrimaryFace)
	: AbstractFace(inPrimaryFace.GetSize())
{
	mFaceStack.push_back(inPrimaryFace);
}

FaceStack::~FaceStack()
{
}

void FaceStack::AddSecondaryFace(const Face &inFace)
{
	ASSERT(GetSize() == inFace.GetSize());
	mFaceStack.push_back(inFace);
}

Glyph FaceStack::GetGlyph(CharCode inCharCode)
{
	Face *face;
	GlyphIndex glyph;
	SearchForCharacter(inCharCode, &face, &glyph);
	return face->GetGlyphFromGlyphIndex(glyph);
}

Distance FaceStack::GetLineHeight()
{
	// Use the line-height of our primary face.
	return mFaceStack.front().GetLineHeight();
}

Face *FaceStack::GetRealFace(CharCode inCharCode)
{
	Face *face;
	GlyphIndex glyph;
	SearchForCharacter(inCharCode, &face, &glyph);
	return face;
}

void FaceStack::SearchForCharacter(CharCode inCharCode,
								   Face **outFace,
								   GlyphIndex *outGlyphIndex)
{
	// Search for the first font which has a glyph for our char code.
	for (std::deque<Face>::iterator iter = mFaceStack.begin();
		 iter < mFaceStack.end(); iter++)
	{
		*outFace = &*iter;
		*outGlyphIndex = (*outFace)->GetGlyphIndex(inCharCode);
		if (*outGlyphIndex != 0)
			return;
	}

	// If we didn't find anything, return the default glyph from our
	// primary face.
	*outFace = &mFaceStack.front();
	*outGlyphIndex = 0;
}


//=========================================================================
//	Typography::StyledTextSpan Methods
//=========================================================================

StyledTextSpan::StyledTextSpan(const StyledCharIterator &inBegin,
							   const StyledCharIterator &inEnd)
	: begin(inBegin), end(inEnd)
{
	// MSVC's STL implementation uses NULL, NULL to denote the
	// begin and end iterators of the empty string.  We allow this.
	ASSERT((begin.mTextIter != NULL && end.mTextIter != NULL) ||
		   (begin.mTextIter == NULL && end.mTextIter == NULL));
}


//=========================================================================
//	Typography::LineSegment(Iterator) Methods
//=========================================================================

LineSegment::LineSegment(const StyledTextSpan &inSpan,
						 bool inIsLineBreak = false,
						 bool inDiscardAtEndOfLine = false,
						 bool inNeedsHyphenAtEndOfLine = false)
	: span(inSpan), isLineBreak(inIsLineBreak),
	  discardAtEndOfLine(inDiscardAtEndOfLine),
	  needsHyphenAtEndOfLine(inNeedsHyphenAtEndOfLine),
	  userDistanceData(0)
{
}

bool Typography::operator==(const LineSegment &left, const LineSegment &right)
{
	return (left.span.begin == right.span.begin &&
			left.span.end == right.span.end &&
			left.isLineBreak == right.isLineBreak &&
			left.discardAtEndOfLine == right.discardAtEndOfLine &&
			left.needsHyphenAtEndOfLine == right.needsHyphenAtEndOfLine);
}

LineSegmentIterator::LineSegmentIterator(const StyledTextSpan &inSpan)
	: mSegmentBegin(inSpan.begin), mTextEnd(inSpan.end)
{
	// We've initialized our iterator.
}

bool LineSegmentIterator::NextElement(LineSegment *outSegment)
{
	ASSERT(outSegment != NULL);
	
	// Skip past any leading soft hyphens.	(They're merely invisible
	// segment-breaking hints, and therefore meaningless at the
	// beginning of a segment.)
	while (mSegmentBegin != mTextEnd && (*mSegmentBegin).value == kSoftHyphen)
		++mSegmentBegin;

	// If we don't have any more text, give up now.
	if (mSegmentBegin == mTextEnd)
		return false;

	// Figure out what kind of segment to process next.
	StyledCharIterator cursor = mSegmentBegin;
	if ((*cursor).value == '\n')
	{
		// NEWLINE SEGMENT
		// Include just the newline in the segment.
		++cursor;

		// Describe our segment & update our state.
		outSegment->SetLineSegment(StyledTextSpan(mSegmentBegin, cursor),
								   true);
		mSegmentBegin = cursor; 
	}
	else if (iswspace((*cursor).value))
	{
		// WHITESPACE SEGMENT
		// Scan forward until we find the end of the whitespace.
		while (cursor != mTextEnd &&
			   iswspace((*cursor).value) &&
			   (*cursor).value != '\n')
			++cursor;

		// Describe our segment & update our state.
		outSegment->SetLineSegment(StyledTextSpan(mSegmentBegin, cursor),
								   false, true);
		mSegmentBegin = cursor; 
	}
	else
	{
		// TEXT SEGMENT
		// Scan forward until we find the end of the current word or
		// a line-break character (e.g., '-').	Soft hyphens are tricky.
		while (cursor != mTextEnd && !iswspace((*cursor).value) &&
			   (*cursor).value != '-' && (*cursor).value != kSoftHyphen)
			++cursor;

		// Adjust our stopping point and set up some flags (as needed).
		bool needHyphenAtEndOfLine = false;
		if (cursor != mTextEnd)
		{
			needHyphenAtEndOfLine = ((*cursor).value == kSoftHyphen);
			if ((*cursor).value == '-')
				++cursor;
		}
		
		// Describe our segment & update our state.
		outSegment->SetLineSegment(StyledTextSpan(mSegmentBegin, cursor),
								   false, false, needHyphenAtEndOfLine);
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
GenericTextRenderingEngine(const StyledTextSpan &inSpan,
						   Distance inLineLength,
						   Justification inJustification)
	: mIterator(inSpan), mLineLength(inLineLength),
	  mJustification(inJustification)
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
		space_used += MeasureSegment(back_or_null(current_line), &seg, false);
		current_line.push_back(seg);
	}
	RenderAndResetLine(&current_line);
}


//=========================================================================
//	Typography::TextRenderingEngine Methods
//=========================================================================

TextRenderingEngine::TextRenderingEngine(const StyledTextSpan &inSpan,
										 Point inPosition,
										 Distance inLineLength,
										 Justification inJustification,
										 Image *inImage)
	: GenericTextRenderingEngine(inSpan, inLineLength, inJustification),
	  mLineStart(inPosition), mImage(inImage)
{
	ASSERT(inPosition.x >= 0 && inPosition.y >= 0);
	ASSERT(inImage != NULL);
}

// We need to process any pixmap output by FreeType 2.  Unfortunately,
// FreeType 2 can output a *lot* of different types of pixmaps, at
// least in theory.  In practice, there are only a few kinds, all of
// which we should handle below.
void TextRenderingEngine::DrawBitmap(Point inPosition, FT_Bitmap *inBitmap,
									 Color inColor)
{
	using GraphicsTools::Channel;
	using GraphicsTools::Color;

    ASSERT(inBitmap->pitch >= 0);

	GraphicsTools::Pixmap pixmap(inBitmap->width, inBitmap->rows);
	pixmap.Clear();

    if (inBitmap->pixel_mode == ft_pixel_mode_grays)
	{
		// Convert 8-bit greyscale characters.
		ASSERT(inBitmap->num_grays == 256);
		for (int y = 0; y < inBitmap->rows; y++)
		{
			for (int x = 0; x < inBitmap->width; x++)
			{
				Channel value = inBitmap->buffer[x + inBitmap->pitch * y];
				pixmap.At(x, y) = Color(inColor.red, inColor.green,
										inColor.blue, 255 - value);
			}
		}
    }
	else
	{
		// Convert 1-bit monochrome characters.
		ASSERT(inBitmap->pixel_mode == ft_pixel_mode_mono);
		for (int y = 0; y < inBitmap->rows; y++)
		{
			for (int x = 0; x < inBitmap->width; x++)
			{
				unsigned char byte = inBitmap->buffer[(x/8) +
													  inBitmap->pitch * y];
				Channel value = ((1<<(7-(x%8))) & byte) ? 0 : 255; 
				pixmap.At(x, y) = Color(inColor.red, inColor.green,
										inColor.blue, value);
			}
		}	
    }

	// Draw our pixmap.
	mImage->DrawPixmap(inPosition, pixmap);
}

Distance TextRenderingEngine::MeasureSegment(LineSegment *inPrevious,
											 LineSegment *inSegment,
											 bool inAtEndOfLine)
{
	// Attempt to get the last glyph of the previous segment for
	// kerning purposes.  Default to 0, which is the built-in code for
	// "no glyph".
	CharCode previous_char = kNoSuchCharacter;
	AbstractFace *previous_face = NULL;
	if (inPrevious)
	{
		ASSERT(inPrevious->span.begin != inPrevious->span.end);
		StyledCharIterator previous = inPrevious->span.end;
		previous_char = (*--previous).value;
		previous_face = (*previous).style.GetFace();
	}

	// Measure the segment.
	// TODO - Merge with code below?
	Distance total = 0;
	StyledCharIterator cp = inSegment->span.begin;
	for (; cp != inSegment->span.end; ++cp)
	{
		CharCode current_char = (*cp).value;
		AbstractFace *current_face = (*cp).style.GetFace();

		// Do our kerning.
		Vector delta = AbstractFace::Kern(previous_char, previous_face,
										  current_char, current_face);
		total += delta.x >> 6; // Don't need round_266 (already fitted).
		ASSERT(delta.y == 0);

		// Load and measure our glyph.
		Glyph glyph = current_face->GetGlyph(current_char);
		total += round_266(glyph->advance.x);
		ASSERT(glyph->advance.y == 0);

		// Update our previous char & face.
		previous_char = current_char;
		previous_face = current_face;
	}

	// If necessary, add a trailing hyphen.
	// TODO - Factor our common code shared with above loop.
	if (inAtEndOfLine && inSegment->needsHyphenAtEndOfLine)
	{
		CharCode current_char = '-';
		AbstractFace *face = (*--cp).style.GetFace();
		
		// Do our kerning.
		Vector delta = AbstractFace::Kern(previous_char, previous_face,
										  current_char, face);
		total += delta.x >> 6; // Don't need round_266 (already fitted).
		ASSERT(delta.y == 0);

		// Load and measure our glyph.
		Glyph glyph = face->GetGlyph(current_char);
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
	throw Error("Cannot break overlong line (yet)");
}

void TextRenderingEngine::RenderLine(std::deque<LineSegment> *inLine,
									 Distance inHorizontalOffset)
{
	// XXX - Extremely primitive and incorrect line-height system.
	// This is wrong in *so* *many* different ways.
	int line_height = 0;

	// Figure out where to start drawing text.
	Point cursor = mLineStart;
	cursor.x += inHorizontalOffset;

	// Draw each character.
	GlyphIndex previous_char = kNoSuchCharacter;
	AbstractFace *previous_face = NULL;
	StyledCharIterator cp;
	for (std::deque<LineSegment>::iterator iter = inLine->begin();
		 iter < inLine->end(); iter++)
	{
		// TODO - Factor out common code with MeasureSegment?
		LineSegment seg = *iter;
		for (cp = seg.span.begin; cp != seg.span.end; ++cp)
		{
			GlyphIndex current_char = (*cp).value;
			AbstractFace *current_face = (*cp).style.GetFace();
			
			// Update our line-height.
			int current_height = current_face->GetLineHeight();
			if (current_height > line_height)
				line_height = current_height;

			// Do our kerning.
			Vector delta = AbstractFace::Kern(previous_char, previous_face,
											  current_char, current_face);
			cursor.x += delta.x >> 6; // Don't need round_266
			ASSERT(cursor.x >= 0);
			ASSERT(delta.y == 0);

			// Load and draw our glyph.
			Glyph glyph = current_face->GetGlyph(current_char);
			Point loc = cursor;
			loc.x += glyph->bitmap_left;
			loc.y -= glyph->bitmap_top;
			DrawBitmap(loc, &glyph->bitmap, (*cp).style.GetColor());

			// Advance our cursor.
			cursor.x += round_266(glyph->advance.x);
			ASSERT(cursor.x >= 0);
			ASSERT(glyph->advance.y == 0);

			// Update our previous char.
			previous_char = current_char;
			previous_face = current_face;
		}
	}

	// Draw a trailing hyphen if we need one.
	// XXX - (More ridiculous duplication.)
	if (!inLine->empty() && inLine->back().needsHyphenAtEndOfLine)
	{
		GlyphIndex current_char = '-';
		AbstractFace *face = (*--cp).style.GetFace();
			
		// Do our kerning.
		Vector delta = AbstractFace::Kern(previous_char, previous_face,
										  current_char, face);
		cursor.x += delta.x >> 6; // Don't need round_266 (already fitted).
		ASSERT(cursor.x >= 0);
		ASSERT(delta.y == 0);

		// Load and draw our glyph.
		Glyph glyph = face->GetGlyph(current_char);
		Point loc = cursor;
		loc.x += glyph->bitmap_left;
		loc.y -= glyph->bitmap_top;
		DrawBitmap(loc, &glyph->bitmap, (*cp).style.GetColor());

		// Advance our cursor.
		cursor.x += round_266(glyph->advance.x);
		ASSERT(cursor.x >= 0);
		ASSERT(glyph->advance.y == 0);
	}
	
	// Update our line start for the next line.
	mLineStart.y += line_height;
}


//=========================================================================
//	Typography::FamilyDatabase::AvailableFace Methods
//=========================================================================

FamilyDatabase::AvailableFace::AvailableFace(const std::string &inFileName)
	: mFileName(inFileName)
{
	// Open up our face file.
	FT_Face face;
	std::string path =
		FileSystem::GetFontFilePath(mFileName).ToNativePathString();
	Error::CheckResult(FT_New_Face(*Library::GetLibrary(),
								   path.c_str(), 0, &face));
				
	try
	{
		// Extract some useful information about the face.
		mFamilyName = face->family_name;
		mStyleName = face->style_name;

		// Extract style flags from the face.
		mIsBold = (face->style_flags & FT_STYLE_FLAG_BOLD) ? true : false;
		mIsItalic = (face->style_flags & FT_STYLE_FLAG_ITALIC) ? true : false;

		// Fix the style flags a of a few commonly-used fonts which we
		// believe to be bogus.  Feel free to add more.  Yes, font drivers
		// often need to special case particular fonts; it's ugly.
		if (mFamilyName == "URW Gothic L" &&
			(mStyleName == "Demi" || mStyleName == "Demi Oblique"))
			mIsBold = true;

		// There are no non-italic Chancery fonts in our collection, so
		// treat Chancery as regular.
		if (mFamilyName == "URW Chancery L" && mStyleName == "Medium Italic")
			mIsItalic = false;

		// Figure out the font's size.
		if (FT_IS_SCALABLE(face))
			mSize = kAnySize;
		else
		{
			// Search for a fixed size we can use.
			bool found_size = false;
			for (FT_Int i = 0; i < face->num_fixed_sizes; i++) {
				if (face->available_sizes[i].height == 
					face->available_sizes[i].width)
				{
					found_size = true;
					mSize = face->available_sizes[i].height;
					break;
				}
			}

			// If we didn't find any sizes, get cranky.
			if (!found_size)
				throw Error(FT_Err_Invalid_File_Format);
		}
	}
	catch (...)
	{
		FT_Done_Face(face);
		throw;
	}
	Error::CheckResult(FT_Done_Face(face));
}

Face FamilyDatabase::AvailableFace::OpenFace(int inSize) const
{
	ASSERT(inSize != kAnySize && inSize > 0);
	ASSERT(mSize == kAnySize || mSize == inSize);

	FileSystem::Path path = FileSystem::GetFontFilePath(mFileName);
	std::string file = path.ToNativePathString();
	FileSystem::Path metrics_path = path.ReplaceExtension("afm");
	std::string metrics_file = metrics_path.ToNativePathString();

	if (metrics_path.DoesExist())
		return Face(file.c_str(), metrics_file.c_str(), inSize);
	else
		return Face(file.c_str(), NULL, inSize);
}

void FamilyDatabase::AvailableFace::ReadSerializationHeader(std::istream &in)
{
	// Check our header information.
	std::string filetype, vers_label;
	int version;
	in >> filetype >> vers_label >> version >> std::ws;
	if (!in || filetype != "facecache" || vers_label != "vers" || version != 1)
		throw Error("Incorrectly formatted face cache");
	
	// Discard our human-readable comment line.
	std::string junk;
	std::getline(in, junk);
	if (!in)
		throw Error("Error reading face cache");
}

void FamilyDatabase::AvailableFace::WriteSerializationHeader(std::ostream &out)
{
	out << "facecache vers 1" << std::endl
		<< "FILE|FAMILY|STYLE|SIZE|IS BOLD|IS ITALIC"
		<< std::endl;
}

FamilyDatabase::AvailableFace::AvailableFace(std::istream &in)
{
	// Read in our individual fields.
	std::string has_metrics, size, is_bold, is_italic;
	std::getline(in, mFileName, '|');
	std::getline(in, mFamilyName, '|');
	std::getline(in, mStyleName, '|');
	std::getline(in, size, '|');
	std::getline(in, is_bold, '|');
	std::getline(in, is_italic);
	if (!in)
		throw Error("Error reading entry from face cache");

	// Needed so eof() will return true after last record.
	// XXX - Will cause problems if font names begin with spaces.
	in >> std::ws; 
	
	// Convert a few numeric values.  Use ternary operator to
	// convert booleans so MSVC++ doesn't whine at us.
	mSize       = atoi(size.c_str());
	mIsBold     = atoi(is_bold.c_str()) ? true : false;
	mIsItalic   = atoi(is_italic.c_str()) ? true : false;
}

void FamilyDatabase::AvailableFace::Serialize(std::ostream &out) const
{
	// XXX - This will fail if any of our strings contain '|'.
	out << mFileName << '|' << mFamilyName << '|' << mStyleName << '|'
		<< mSize << '|' << mIsBold << '|' << mIsItalic << std::endl;
}


//=========================================================================
//	Typography::FamilyDatabase::FaceSizeGroup Methods
//=========================================================================

void
FamilyDatabase::FaceSizeGroup::AddAvailableFace(const AvailableFace &inFace)
{
	int size = inFace.GetSize();
	if (mAvailableFaces.find(size) != mAvailableFaces.end())
		throw Error("Tried to add duplicate font to font database");
	mAvailableFaces.insert(std::pair<int,AvailableFace>(size, inFace));
}

Face FamilyDatabase::FaceSizeGroup::GetFace(int inSize)
{
	// First, look for an already instantiated face.
	std::map<int,Face>::iterator foundFace = mFaces.find(inSize);
	if (foundFace != mFaces.end())
		return foundFace->second;
	
	// Next, look for either (1) an available face in the exact size or
	// (2) an available face which can be displayed at any size.
	std::map<int,AvailableFace>::iterator found = mAvailableFaces.find(inSize);
	if (found == mAvailableFaces.end())
		found = mAvailableFaces.find(kAnySize);

	// If we *still* don't have a face, give up.  If we were feeling
	// very ambitious, we could look for the nearest size and use that.
	if (found == mAvailableFaces.end())
		throw Error("No matching font (did you try to scale a bitmap font?)");

	// Open the face, remember it, and return it.
	Face face = found->second.OpenFace(inSize);
	mFaces.insert(std::pair<int,Face>(inSize, face));
	return face;
}

void FamilyDatabase::FaceSizeGroup::Serialize(std::ostream &out) const
{
	for (std::map<int,AvailableFace>::const_iterator iter =
			 mAvailableFaces.begin();
		 iter != mAvailableFaces.end(); ++iter)
		iter->second.Serialize(out);
}


//=========================================================================
//	Typography::FamilyDatabase::Family Methods
//=========================================================================

void FamilyDatabase::Family::AddAvailableFace(const AvailableFace &inFace)
{
	ASSERT(mFamilyName == inFace.GetFamilyName());

	// Store the face in the appropriate group.
	if (inFace.IsBold() && inFace.IsItalic())
		mBoldItalicFaces.AddAvailableFace(inFace);
	else if (inFace.IsBold())
		mBoldFaces.AddAvailableFace(inFace);
	else if (inFace.IsItalic())
		mItalicFaces.AddAvailableFace(inFace);
	else
		mRegularFaces.AddAvailableFace(inFace);
}

Face FamilyDatabase::Family::GetFace(FaceStyle inStyle, int inSize)
{
	ASSERT((inStyle & ~kIntrisicFaceStyles) == 0);

	// We use an elaborate system of recursive fallbacks to find
	// an appropriate face.
	switch (inStyle)
	{
		case kRegularFaceStyle:
			// Fallback: Regular -> Error
			return mRegularFaces.GetFace(inSize);

		case kBoldFaceStyle:
			// Fallback: Bold -> Regular -> Error
			try { return mBoldFaces.GetFace(inSize); }
			catch (...) { return GetFace(kRegularFaceStyle, inSize); }

		case kItalicFaceStyle:
			// Fallback: Italic -> Regular -> Error
			try { return mItalicFaces.GetFace(inSize); }
			catch (...) { return GetFace(kRegularFaceStyle, inSize); }

		case kBoldItalicFaceStyle:
			// Fallback: BoldItalic -> Bold -> Italic -> Regular -> Error
			try { return mBoldItalicFaces.GetFace(inSize); }
			catch (...)
			{ 
				try { return mBoldFaces.GetFace(inSize); }
				catch (...) { return GetFace(kItalicFaceStyle, inSize); }
			}

		default:
			// Illegal style codes!
			throw Error("Unknown font style codes, giving up");
	}
	ASSERT(false);
	return *(Face*) NULL; // This code should NEVER get run.
}

void FamilyDatabase::Family::Serialize(std::ostream &out) const
{
	mRegularFaces.Serialize(out);
	mBoldFaces.Serialize(out);
	mItalicFaces.Serialize(out);
	mBoldItalicFaces.Serialize(out);
}


//=========================================================================
//	Typography::FamilyDatabase Methods
//=========================================================================

FamilyDatabase *FamilyDatabase::sFamilyDatabase = NULL;
		
FamilyDatabase *FamilyDatabase::GetFamilyDatabase()
{
	if (!sFamilyDatabase)
	{
		sFamilyDatabase = new FamilyDatabase();
		sFamilyDatabase->ReadFromFontDirectory();
	}
	return sFamilyDatabase;
}

bool FamilyDatabase::IsFontFile(const FileSystem::Path &inPath)
{
	std::string extension = inPath.GetExtension();
	return (extension == "pfb" || extension == "pcf" || extension == "ttf");
}

void FamilyDatabase::AddAvailableFace(const AvailableFace &inFace)
{
	std::string family_name = inFace.GetFamilyName();
	std::map<std::string,Family>::iterator found = mFamilyMap.find(family_name);
	if (found == mFamilyMap.end())
	{
		mFamilyMap.insert(std::pair<std::string,Family>(family_name,
												        Family(family_name)));
		found = mFamilyMap.find(family_name);
		ASSERT(found != mFamilyMap.end());
	}
	found->second.AddAvailableFace(inFace);
}

Face FamilyDatabase::GetFace(const std::string &inFamilyName,
							 FaceStyle inStyle, int inSize)
{
	std::map<std::string,Family>::iterator found = mFamilyMap.find(inFamilyName);
	if (found != mFamilyMap.end())
		return found->second.GetFace(inStyle, inSize);
	else
		throw Error("Unknown font family \"" + inFamilyName + "\"");

	ASSERT(false);
	return *(Face *) NULL; // Never run.
}

void FamilyDatabase::ReadFromFontDirectory()
{
	// If a cache file exists, attempt to read from it.
	FileSystem::Path cachePath = FileSystem::GetFontFilePath("cache.dat");
	if (cachePath.DoesExist() && cachePath.IsRegularFile())
	{
		std::ifstream cache(cachePath.ToNativePathString().c_str());
		ReadFromCache(cache);
		return;
	}

	// Otherwise, open all the fonts in the font directory.
	FileSystem::Path fontdir = FileSystem::GetFontDirectory();
	std::list<std::string> entries = fontdir.GetDirectoryEntries();
	for (std::list<std::string>::iterator iter = entries.begin();
		 iter != entries.end(); iter++)
	{
		FileSystem::Path file = fontdir.AddComponent(*iter);
		if (file.IsRegularFile() && IsFontFile(file))
		{
			// Load the face using FreeType, and add it to our database.
			AvailableFace face(*iter);
			AddAvailableFace(face);
		}
	}

	// Attempt to write out a new cache file.
	try
	{
		std::ofstream cache(cachePath.ToNativePathString().c_str());
		WriteToCache(cache);
	}
	catch (...)
	{
		// Just ignore the exception.
		// TODO - Try logging a warning?
	}
}

void FamilyDatabase::ReadFromCache(std::istream &in)
{
	FamilyDatabase::AvailableFace::ReadSerializationHeader(in);
	while (!in.eof())
		AddAvailableFace(AvailableFace(in));
}

void FamilyDatabase::WriteToCache(std::ostream &out) const
{
	FamilyDatabase::AvailableFace::WriteSerializationHeader(out);
	for (std::map<std::string,Family>::const_iterator iter = mFamilyMap.begin();
		 iter != mFamilyMap.end(); ++iter)
		iter->second.Serialize(out);	
}
