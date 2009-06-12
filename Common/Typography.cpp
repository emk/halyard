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

#include "CommonHeaders.h"

#include <fstream>

#include <stdlib.h>
#include <assert.h>
#include <wctype.h>

#include "Typography.h"

using std::min;
using std::max;
using namespace Typography;

inline Distance round_266 (FT_Pos in266Measurement) {
    // XXX - Are all our values pre-fitted?
    return (in266Measurement + 32) >> 6;
}

#define CHECK_RESULT(RESULT) Error::CheckResult(__FILE__, __LINE__, RESULT)


//=========================================================================
//  Typography::Error Methods
//=========================================================================

Error::Error(const char* inFile, int inLine, int inErrorCode)
    : TException(inFile, inLine)
{
    SetErrorCode(inErrorCode);
    SetErrorMessage("(no error strings, yet--try fterrors.h)");
}


//=========================================================================
//  Typography::Representation Methods
//=========================================================================

Representation::Representation()
    : mRefCount(1)
{
}

Representation::Representation(const Representation &obj)
    : mRefCount(1)
{
}

Representation &Representation::operator=(const Representation &obj) {
    // Leave mRefCount alone.
    return *this;
}

Representation::~Representation() {
    ASSERT(mRefCount == 0);
}

void Representation::IncRef() {
    // THREAD - Not thread safe.
    ++mRefCount;
}

void Representation::DecRef() {
    // THREAD - Not thread safe.
    --mRefCount;
    if (mRefCount < 1)
        delete this;
}


//=========================================================================
//  Typography::Library Methods
//=========================================================================

Library *Library::sLibrary = NULL;

Library::Library() {
    CHECK_RESULT(FT_Init_FreeType(&mLibrary));
}

Library::~Library() {
    CHECK_RESULT(FT_Done_FreeType(mLibrary));
}

Library *Library::GetLibrary() {
    if (!sLibrary)
        sLibrary = new Library();
    return sLibrary;
}


//=========================================================================
//  Typography::Glyph Methods
//=========================================================================

Glyph::Glyph(FT_GlyphSlot inGlyph)
    : mAdvance(inGlyph->advance), mMetrics(inGlyph->metrics),
      mGreyMapOffset(Point(inGlyph->bitmap_left,
                           -inGlyph->bitmap_top)),
      mGreyMap(inGlyph->bitmap.width,
               inGlyph->bitmap.rows)
      
{
    using GraphicsTools::Channel;

    // Attempt to convert FreeType's various funky bitmap formats into
    // GreyMap values.  This code isn't especially optimized, because
    // we cache glyphs, and don't care much about how much it costs
    // to create them.
    //
    // We need to process any pixmap output by FreeType 2.  Unfortunately,
    // FreeType 2 can output a *lot* of different types of pixmaps, at
    // least in theory.  In practice, there are only a few kinds, all of
    // which we should handle below.

    FT_Bitmap *bitmap = &inGlyph->bitmap;
    ASSERT(bitmap->pitch >= 0);
    if (bitmap->pixel_mode == ft_pixel_mode_grays) {
        // Convert 8-bit greyscale characters.
        ASSERT(bitmap->num_grays == 256);
        for (int y = 0; y < bitmap->rows; y++) {
            for (int x = 0; x < bitmap->width; x++) {
                Channel value = bitmap->buffer[x + bitmap->pitch*y];
                mGreyMap.At(x, y) = value;
            }
        }
    } else {
        // Convert 1-bit monochrome characters.
        ASSERT(bitmap->pixel_mode == ft_pixel_mode_mono);
        for (int y = 0; y < bitmap->rows; y++) {
            for (int x = 0; x < bitmap->width; x++) {
                unsigned char byte = bitmap->buffer[(x/8) + bitmap->pitch * y];
                Channel value = ((1<<(7-(x%8))) & byte) ? 255 : 0; 
                mGreyMap.At(x, y) = value;
            }
        }   
    }
}


//=========================================================================
//  Typography::Style Methods
//=========================================================================
//  The 'StyleRep' code in this class is getting out of control, and should
//  probably be factored into a standard 'Representation' class.

/// Construct a StyleRep from a family and size.
Style::StyleRep::StyleRep(const std::string &inFamily, int inSize)
    : mFamily(inFamily),
      mBackupFamilies(),
      mFaceStyle(kRegularFaceStyle),
      mSize(inSize),
      mLeading(0),
      mShadowOffset(1),
      mColor(Color(0, 0, 0)),
      mShadowColor(Color(255, 255, 255)),
      mFace(NULL)
{
}

/// Copy a StyleRep so we can modify it destructively.
Style::StyleRep::StyleRep(const StyleRep &inBase)
    : mFamily(inBase.mFamily),
      mBackupFamilies(inBase.mBackupFamilies),
      mFaceStyle(inBase.mFaceStyle),
      mSize(inBase.mSize),
      mLeading(inBase.mLeading),
      mShadowOffset(inBase.mShadowOffset),
      mColor(inBase.mColor),
      mShadowColor(inBase.mShadowColor),
      mFace(NULL)
{    
}

Style::StyleRep::~StyleRep() {
    InvalidateFace();
}

/// Release the face associated with this style, typically because some
/// property of the style has been changed and we'll need to reload the
/// face.
void Style::StyleRep::InvalidateFace() {
    if (mFace) {
        delete mFace;
        mFace = NULL;
    }
}

bool Style::StyleRep::operator==(const StyleRep &inRep) const {
    return (mFamily == inRep.mFamily &&
            mBackupFamilies == inRep.mBackupFamilies &&
            mFaceStyle == inRep.mFaceStyle &&
            mSize == inRep.mSize &&
            mLeading == inRep.mLeading &&
            mShadowOffset == inRep.mShadowOffset &&
            mColor == inRep.mColor &&
            mShadowColor == inRep.mShadowColor);
}


Style::Style(const std::string &inFamily, int inSize) {
    mRep = new StyleRep(inFamily, inSize);
}

Style::Style(const Style &inStyle) {
    // It's safe to cast away the const here because we always call
    // 'Grab' before modifying the representation.
    mRep = const_cast<StyleRep*>(inStyle.mRep);
    mRep->IncRef();
}

Style::~Style() {
    // Only throw away the rep if we're the last reference.
    mRep->DecRef();
}

void Style::Grab() {
    // If we don't have our own copy, get one.
    if (mRep->IsShared()) {
        StyleRep *oldRep = mRep;
        mRep = new StyleRep(*mRep);

        // It's safe to decrement this only after we've updated 'mRep'.
        oldRep->DecRef();
    }
}

void Style::InvalidateFace() {
    mRep->InvalidateFace();
}

Typography::Style &Style::operator=(const Style &inStyle) {
    // Watch out for self-assignment.
    if (mRep == inStyle.mRep)
        return *this;

    // Delete our reference to our representation.
    mRep->DecRef();

    // Make a new reference to inStyle's representation.
    mRep = inStyle.mRep;
    mRep->IncRef();
    return *this;
}

bool Style::operator==(const Style &inStyle) const {
    return (*mRep == *inStyle.mRep);
}

Typography::Style &Style::SetFamily(const std::string &inFamily) {
    Grab();
    InvalidateFace();
    mRep->mFamily = inFamily;
    return *this;
}

Typography::Style &Style::SetBackupFamilies(const std::list<std::string> &inBFs) {
    Grab();
    InvalidateFace();
    mRep->mBackupFamilies = inBFs;
    return *this;   
}

Typography::Style &Style::SetFaceStyle(FaceStyle inFaceStyle) {
    Grab();
    InvalidateFace();
    mRep->mFaceStyle = inFaceStyle;
    return *this;
}

Typography::Style &Style::ToggleFaceStyle(FaceStyle inToggleFlags) {
    FaceStyle toggled_mask = inToggleFlags;
    FaceStyle unchanged_mask = ~inToggleFlags;
    FaceStyle current = GetFaceStyle();
    SetFaceStyle((current & unchanged_mask) | (~current & toggled_mask));
    return *this;
}

Typography::Style &Style::SetSize(int inSize) {
    Grab();
    InvalidateFace();
    mRep->mSize = inSize;
    return *this;
}

Typography::Style &Style::SetLeading(Distance inLeading) {
    Grab();
    mRep->mLeading = inLeading;
    return *this;
}

Typography::Style &Style::SetShadowOffset(Distance inOffset) {
    // We're too lazy to fully implement negative shadow offsets
    // (particularly with regard to bounding box calculations), so we
    // disallow them for now.
    if (inOffset < 0)
        throw Error(__FILE__, __LINE__, "Cannot have negative shadow offset");

    Grab();
    mRep->mShadowOffset = inOffset;
    return *this;
}

Typography::Style &Style::SetColor(Color inColor) {
    Grab();
    mRep->mColor = inColor;
    return *this;
}

Typography::Style &Style::SetShadowColor(Color inColor) {
    Grab();
    mRep->mShadowColor = inColor;
    return *this;
}

AbstractFace *Style::GetFace() const {
    if (!mRep->mFace) {
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

bool Style::GetIsUnderlined() const {
    return (mRep->mFaceStyle & kUnderlineFaceStyle) ? true : false;
}

bool Style::GetIsShadowed() const {
    return (mRep->mFaceStyle & kShadowFaceStyle) ? true : false;
}


//=========================================================================
//  Typography::StyledText Methods
//=========================================================================

StyledText::StyledText(const Style &inBaseStyle)
    : mBaseStyle(inBaseStyle), mIsBuilt(false)
{
    mStyleRuns.insert(std::pair<size_t,Style>(0, inBaseStyle));
}

void StyledText::AppendText(const std::wstring &inText) {
    ASSERT(!mIsBuilt);
    mText += inText;
}

void StyledText::AppendText(wchar_t inText) {
    ASSERT(!mIsBuilt);
    ASSERT(inText != 0);
    mText += inText;
}

void StyledText::ChangeStyle(const Style &inStyle) {
    ASSERT(!mIsBuilt);

    // Delete any existing entry at this offset.
    size_t offset = mText.length();
    std::map<size_t,Style>::iterator existing = mStyleRuns.find(offset);
    if (existing != mStyleRuns.end())
        mStyleRuns.erase(existing);

    // Insert the new entry.
    mStyleRuns.insert(std::pair<size_t,Style>(mText.length(), inStyle));
}

void StyledText::EndConstruction() {
    ASSERT(!mIsBuilt);
    mIsBuilt = true;
    mEnd = mText.length();
}

// TODO - Cache results of this lookup somehow?  This code is probably
// performance critical, but profile it before optimizing it.
const Typography::Style *StyledText::GetStyleAt(size_t inOffset) const {
    ASSERT(mIsBuilt);
    ASSERT(0 <= inOffset && inOffset < mEnd);

    // Find the first element with a key NOT LESS THAN inOffset.
    std::map<size_t,Style>::const_iterator lower_bound =
        mStyleRuns.lower_bound(inOffset);

    // Figure out which style actually applies.
    std::map<size_t,Style>::const_iterator result = lower_bound;
    if (result == mStyleRuns.end()) {
        // CASE 1: If all the elements were less than our offset, we'll get
        // an iterator pointing to the end of the collection.  We want to
        // back up one item.
        --result;
    } else {
        std::pair<size_t,Style> item = *result;
        if (item.first == inOffset) {
            // CASE 2: We found a style begining exactly at our offset.
            // We want to use it as is.
        } else {
            // CASE 3: We found the first style greater than our offset.
            // We need to back up one element.
            ASSERT(item.first > inOffset);
            --result;
        }
    }
    
    // Return our result.
    ASSERT(result->first <= inOffset);
    return &result->second;
}


//=========================================================================
//  Typography::StyledText::value_type Methods
//=========================================================================

Distance StyledText::value_type::GetLineHeight(bool isFirstLine) const {
    Distance ascender = GetNominalAscender();
    if (isFirstLine) {
        // We don't need any inter-line spacing for the first line.
        return ascender;
    } else {
        // We need to use the full interline spacing for lines after
        // the first, and we need to adjust the leading.  Note that if
        // the ascender requires extra room, we'll bump the line height
        // up until it fits.
        Distance interline_spacing =
            style->GetFace()->GetLineHeight() + style->GetLeading();
        return max(ascender, interline_spacing);
    }
}

Distance StyledText::value_type::GetNominalAscender() const {
    Glyph *glyph = style->GetFace()->GetGlyph(value);
    Distance glyph_ascender = round_266(glyph->GetMetrics()->horiBearingY);
    return max(glyph_ascender, style->GetFace()->GetAscender());
}

Distance StyledText::value_type::GetNominalDescender() const {
    Glyph *glyph = style->GetFace()->GetGlyph(value);
    Distance glyph_descender = round_266(glyph->GetMetrics()->height -
                                         glyph->GetMetrics()->horiBearingY);
    Distance base = max(glyph_descender, style->GetFace()->GetDescender());
    return base + max(Distance(0), style->GetShadowOffset());
}

Distance StyledText::value_type::GetLeftBearing() const {
    // TODO - Should we calculate a left bearing for the face, like with do
    // with GetNominalAscender, etc.?  This would get us better-aligned
    // left-hand edges in vertically-aligned text blocks drawn in separate
    // calls to the TextRenderingEngine.
    Glyph *glyph = style->GetFace()->GetGlyph(value);
    return round_266(glyph->GetMetrics()->horiBearingX);
}


//=========================================================================
//  Typography::AbstractFace Methods
//=========================================================================

Vector AbstractFace::Kern(const StyledText::value_type &inChar1,
                          const StyledText::value_type &inChar2)
{
    Vector result;
    result.x = 0;
    result.y = 0;
    
    if (inChar1.value != kNoSuchCharacter &&
        inChar2.value != kNoSuchCharacter)
    {
        ASSERT(inChar1.style != NULL && inChar2.style != NULL);
        Face *face1 = inChar1.style->GetFace()->GetRealFace(inChar1.value);
        Face *face2 = inChar2.style->GetFace()->GetRealFace(inChar2.value);
        if (*face1 == *face2)
            result = face1->GetKerning(inChar1.value, inChar2.value);
    }
    return result;
}


//=========================================================================
//  Typography::Face Methods
//=========================================================================

size_t Face::sGlyphCacheSize = 0;
size_t Face::sGlyphCacheSizeAtLastWarning = 0;
const size_t Face::kGlyphCacheSizeWarningIncrement = 100 * 1024;

Face::FaceRep::FaceRep(FT_Face inFace)
    : mFace(inFace)
{
}

Face::FaceRep::~FaceRep() {
    CHECK_RESULT(FT_Done_Face(mFace));

    // Delete our cached glyph objects.
    std::map<GlyphIndex,Glyph*>::iterator cursor = mGlyphCache.begin();
    for (; cursor != mGlyphCache.end(); ++cursor)
        delete cursor->second;
}

void Face::UpdateGlyphCacheSize(const Glyph *inGlyph) {
    // Update our cache size.
    size_t size = sizeof(Glyph) + inGlyph->GetGreyMap()->EstimatedMemoryUse();
    sGlyphCacheSize += size;

    // Print as many warnings as we need.
    while (sGlyphCacheSize >
           (sGlyphCacheSizeAtLastWarning + kGlyphCacheSizeWarningIncrement))
    {
        Halyard::gLog.Trace("halyard", "Typography: glyph cache is now %dK.",
                            sGlyphCacheSize / 1024);
        sGlyphCacheSizeAtLastWarning += kGlyphCacheSizeWarningIncrement;
    }
}

Face::Face(const char *inFontFile, const char *inMetricsFile, int inSize)
    : AbstractFace(inSize), mFaceRep(NULL)
{
    ASSERT(inFontFile != NULL);
    ASSERT(inSize > 0);

    // Open up our face.
    // Until our FaceRep is successfully constructed, we're in charge
    // of calling FT_Done_Face on this data.
    FT_Face face;
    CHECK_RESULT(FT_New_Face(*Library::GetLibrary(),
                             inFontFile, 0, &face));

    // Allocate a new FaceRep structure.  This takes ownership
    // of the face object.  Until the constructor exits successfully,
    // we're in change of calling delete on the FaceRep.
    try {
        mFaceRep = new FaceRep(face);
    } catch (std::exception &) {
        // Allocation failed, so finish using our face and bail.
        FT_Done_Face(face);
        throw;
    }

    try {
        // Attach our metrics, if we have any.
        if (inMetricsFile)
            CHECK_RESULT(FT_Attach_File(face, inMetricsFile));
        
        // Attempt to set a Unicode charmap.
        CHECK_RESULT(FT_Select_Charmap(face, ft_encoding_unicode));
        
        // Check to see if our font is either (1) scalable or (2) available
        // in the specified point size.  We never attempt to scale bitmap
        // fonts; the results are gross.
        if (!FT_IS_SCALABLE(face)) {
            bool found_size = false;
            for (FT_Int i = 0; i < face->num_fixed_sizes; i++) {
                if (round_266(face->available_sizes[i].y_ppem) == inSize) {
                    found_size = true;
                    break;
                }
            }
            if (!found_size)
                throw Error(__FILE__, __LINE__, "Cannot scale bitmap font");
        }
        
        // Set the size of our font.
        CHECK_RESULT(FT_Set_Char_Size(face, inSize*64, inSize*64,
                                      72, 72));

        // Set various font properties that we'll need later.
        // (Manual conversion to true, false to avoid MSVC++ warning.
        mHasKerning = (FT_HAS_KERNING(face) ? true : false);
    } catch (std::exception &) {
        mFaceRep->DecRef();
        mFaceRep = NULL;
        throw;
    }
}

Face::Face(const Face &inFace)
    : AbstractFace(inFace.GetSize())
{
    inFace.mFaceRep->IncRef();
    mFaceRep = inFace.mFaceRep;
    mHasKerning = inFace.mHasKerning;
}

Face::~Face() {
    mFaceRep->DecRef();
}

Face &Face::operator=(const Face &inFace) {
    // Check for self-assignment.
    if (mFaceRep == inFace.mFaceRep)
        return *this;

    // Call inherited assignment.
    AbstractFace::operator=(inFace);

    mFaceRep->DecRef();
    mFaceRep = inFace.mFaceRep;
    mFaceRep->IncRef();
    return *this;
}

GlyphIndex Face::GetGlyphIndex(CharCode inCharCode) {
    if (inCharCode == kNoSuchCharacter)
        return 0;
    else
        return FT_Get_Char_Index(mFaceRep->mFace, inCharCode);
}

Glyph *Face::GetGlyphFromGlyphIndex(GlyphIndex inGlyphIndex) {
    // Look for a glyph in our cache.
    std::map<GlyphIndex,Glyph*>::iterator found =
        mFaceRep->mGlyphCache.find(inGlyphIndex);

    if (found != mFaceRep->mGlyphCache.end()) {
        // Return the cached glyph glyph.
        return found->second;
    } else {
        // Load and cache a new glyph.
        CHECK_RESULT(FT_Load_Glyph(mFaceRep->mFace, inGlyphIndex,
                                   FT_LOAD_RENDER));
        Glyph *glyph = new Glyph(mFaceRep->mFace->glyph);
        mFaceRep->mGlyphCache.insert(std::pair<GlyphIndex,Glyph*>(inGlyphIndex,
                                                                  glyph));
        UpdateGlyphCacheSize(glyph);
        return glyph;
    }
}

Glyph *Face::GetGlyph(CharCode inCharCode) {
    return GetGlyphFromGlyphIndex(GetGlyphIndex(inCharCode));
}

Vector Face::GetKerning(CharCode inPreviousChar,
                        CharCode inCurrentChar)
{
    FT_Vector delta;
    GlyphIndex previous_glyph = GetGlyphIndex(inPreviousChar);
    GlyphIndex current_glyph = GetGlyphIndex(inCurrentChar);

    if (mHasKerning && previous_glyph && current_glyph) {
        // If we actually have kerning data, use it.
        CHECK_RESULT(FT_Get_Kerning(mFaceRep->mFace,
                                    previous_glyph, current_glyph,
                                    ft_kerning_default, &delta));
    } else {
        // Otherwise, don't kern the text.
        delta.x = 0;
        delta.y = 0;
    }

    return delta;
}

Distance Face::GetAscender() {
    // The obvious way to implement this function is to call
    // 'round_266(mFaceRep->mFace->size->metrics.ascender)', but this would
    // produce slightly weird results.  In particular, 'ascender' is based
    // on the height of the tallest character in the face, which is usually
    // some ridiculously oversized character (such as an integral sign,
    // which extends above the tallest capital letters, and below the
    // baseline).  Just to add insult to injury, the FreeType 2 reference
    // manual says the ascender value doesn't take kerning into account,
    // and might be off by as much as a pixel.
    //
    // We want to compute a height which is greater than or equal to the
    // commonly-used characters in the font.  We're not interested in
    // "overhigh" characters such as integral signs; those are better
    // handled by adjusting the inter-line spacing.
    const char *candidates = "MTCl1";
    Distance result = 0;
    for (const char *cp = candidates; *cp; cp++) {
        GlyphIndex index = GetGlyphIndex(*cp);
        if (index) {
            Glyph *glyph = GetGlyphFromGlyphIndex(index);
            Distance asc = round_266(glyph->GetMetrics()->horiBearingY);
            result = max(asc, result);
        }
    }

    if (result == 0) {
        // We don't have any candidate characters in this font, so use the
        // approximate height of the tallest character.
        result = round_266(mFaceRep->mFace->size->metrics.ascender);
    }

    return result;
}

Distance Face::GetDescender() {
    // For descenders, we measure a variety of letters, for the reasons
    // discussed above.
    const char *candidates = "gpqyj7";
    Distance result = 0;
    for (const char *cp = candidates; *cp; cp++) {
        GlyphIndex index = GetGlyphIndex(*cp);
        if (index) {
            Glyph *glyph = GetGlyphFromGlyphIndex(index);
            Distance dsc = round_266(glyph->GetMetrics()->height -
                                     glyph->GetMetrics()->horiBearingY);
            result = max(dsc, result);
        }
    }

    if (result == 0) {
        // We don't have any of the candidate characters in this font, so
        // use the approximate maximum descender.
        result = round_266(mFaceRep->mFace->size->metrics.descender);
    }

    return result;
}

Distance Face::GetLineHeight() {
    return round_266(mFaceRep->mFace->size->metrics.height);
}


//=========================================================================
//  Typography::FaceStack Methods
//=========================================================================

FaceStack::FaceStack(const Face &inPrimaryFace)
    : AbstractFace(inPrimaryFace.GetSize())
{
    mFaceStack.push_back(inPrimaryFace);
}

FaceStack::~FaceStack() {
}

void FaceStack::AddSecondaryFace(const Face &inFace) {
    ASSERT(GetSize() == inFace.GetSize());
    mFaceStack.push_back(inFace);
}

Glyph *FaceStack::GetGlyph(CharCode inCharCode) {
    Face *face;
    GlyphIndex glyph;
    SearchForCharacter(inCharCode, &face, &glyph);
    return face->GetGlyphFromGlyphIndex(glyph);
}

Distance FaceStack::GetAscender() {
    // Use the ascender of our primary face.
    // (This makes glyph substitution prettier.)
    return mFaceStack.front().GetAscender();
}

Distance FaceStack::GetDescender() {
    // Use the ascender of our primary face.
    // (This makes glyph substitution prettier.)
    return mFaceStack.front().GetDescender();
}

Distance FaceStack::GetLineHeight() {
    // Use the line-height of our primary face.
    // (This makes glyph substitution prettier.)
    return mFaceStack.front().GetLineHeight();
}

Face *FaceStack::GetRealFace(CharCode inCharCode) {
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
    for (std::vector<Face>::iterator iter = mFaceStack.begin();
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
//  Typography::LineSegment(Iterator) Methods
//=========================================================================

LineSegment::LineSegment(const StyledText::const_iterator &inBegin,
                         const StyledText::const_iterator &inEnd,
                         bool inIsLineBreak /*= false*/,
                         bool inDiscardAtEndOfLine /*= false*/,
                         bool inNeedsHyphenAtEndOfLine /*= false*/)
    : begin(inBegin), end(inEnd),
      isLineBreak(inIsLineBreak),
      discardAtEndOfLine(inDiscardAtEndOfLine),
      needsHyphenAtEndOfLine(inNeedsHyphenAtEndOfLine),
      userDistanceData(0)
{
}

bool Typography::operator==(const LineSegment &left, const LineSegment &right) {
    return (left.begin == right.begin &&
            left.end == right.end &&
            left.isLineBreak == right.isLineBreak &&
            left.discardAtEndOfLine == right.discardAtEndOfLine &&
            left.needsHyphenAtEndOfLine == right.needsHyphenAtEndOfLine);
}

LineSegmentIterator::LineSegmentIterator(const StyledText &inText)
    : mSegmentBegin(inText.begin()), mTextEnd(inText.end())
{
}

bool LineSegmentIterator::NextElement(LineSegment *outSegment) {
    ASSERT(outSegment != NULL);
    
    // Skip past any leading soft hyphens.  (They're merely invisible
    // segment-breaking hints, and therefore meaningless at the
    // beginning of a segment.)
    while (mSegmentBegin != mTextEnd && mSegmentBegin->value == kSoftHyphen)
        ++mSegmentBegin;

    // If we don't have any more text, give up now.
    if (mSegmentBegin == mTextEnd)
        return false;

    // Figure out what kind of segment to process next.
    StyledText::const_iterator cursor = mSegmentBegin;
    if (cursor->value == '\n') {
        // NEWLINE SEGMENT
        // Include just the newline in the segment.
        ++cursor;

        // Describe our segment & update our state.
        outSegment->SetLineSegment(mSegmentBegin, cursor, true);
        mSegmentBegin = cursor; 
    } else if (iswspace(cursor->value)) {
        // WHITESPACE SEGMENT
        // Scan forward until we find the end of the whitespace.
        while (cursor != mTextEnd &&
               iswspace(cursor->value) &&
               cursor->value != '\n')
            ++cursor;

        // Describe our segment & update our state.
        outSegment->SetLineSegment(mSegmentBegin, cursor, false, true);
        mSegmentBegin = cursor; 
    } else {
        // TEXT SEGMENT
        // Scan forward until we find the end of the current word or
        // a line-break character (e.g., '-').  Soft hyphens are tricky.
        while (cursor != mTextEnd && !iswspace(cursor->value) &&
               cursor->value != '-' && cursor->value != kSoftHyphen)
            ++cursor;

        // Adjust our stopping point and set up some flags (as needed).
        bool needHyphenAtEndOfLine = false;
        if (cursor != mTextEnd) {
            needHyphenAtEndOfLine = (cursor->value == kSoftHyphen);
            if (cursor->value == '-')
                ++cursor;
        }
        
        // Describe our segment & update our state.
        outSegment->SetLineSegment(mSegmentBegin, cursor,
                                   false, false, needHyphenAtEndOfLine);
        mSegmentBegin = cursor;
    }

    return true;
}


//=========================================================================
//  Typography::BoundingBox Methods
//=========================================================================

void BoundingBox::ExpandToInclude(Distance inLeft, Distance inTop,
                                  Distance inRight, Distance inBottom)
{
    if (mHasValue) {
        if (inLeft < mLeft)
            mLeft = inLeft;
        if (inTop < mTop)
            mTop = inTop;
        if (inRight > mRight)
            mRight = inRight;
        if (inBottom > mBottom)
            mBottom = inBottom;
    } else {
        mHasValue = true;
        mLeft = inLeft;
        mTop = inTop;
        mRight = inRight;
        mBottom = inBottom;
    }
    ASSERT(mLeft <= mRight);
    ASSERT(mTop <= mBottom);
}

bool BoundingBox::ExtendsBeyond(const BoundingBox &other) const {
    if (!mHasValue)
        // We have no value, so we can't extend beyond anything.
        return false;
    else if (!other.mHasValue)
        // We have a value but the other doesn't, so we extend beyond it.
        return true;
    else if (other.mLeft <= mLeft && mRight <= other.mRight &&
             other.mTop <= mTop && mBottom <= other.mBottom)
        // All our pixels fall within the other box.
        return false;
    else
        // Some of our pixels are outside the other box.
        return true;
}


//=========================================================================
//  Typography::GenericTextRenderingEngine Methods
//=========================================================================

// A miscellaneous local helper function to get either a pointer
// to the last element of a deque, or NULL if no such element exists.
template <class C>
static C* back_or_null(std::vector<C> &d) {
    if (d.empty())
        return NULL;
    else
        return &d.back();
}

GenericTextRenderingEngine::
GenericTextRenderingEngine(const StyledText &inText,
                           Distance inLineLength,
                           Justification inJustification)
    : mText(inText),
      mIterator(inText),
      mDefaultStyle(inText.GetDefaultStyle()),
      mLineLength(inLineLength),
      mJustification(inJustification)
{
    // We can't call RenderText from the constructor because C++ hasn't
    // bothered to initialize our vtables yet.
}
        
Distance GenericTextRenderingEngine::
CalculateHorizontalOffset(Distance inSpaceUsed) {
    ASSERT(inSpaceUsed <= GetLineLength());
    Distance remaining = GetLineLength() - inSpaceUsed;
    Distance offset;
    switch (GetJustification()) {
        case kLeftJustification:   offset = 0; break;
        case kCenterJustification: offset = remaining / 2; break;
        case kRightJustification:  offset = remaining; break;
        default: ASSERT(false); abort();
    }
    return offset;
}

void GenericTextRenderingEngine::
RenderAndResetLine(std::vector<LineSegment> *ioLine) {
    // Discard trailing whitespace segments.
    while (!ioLine->empty() && ioLine->back().discardAtEndOfLine)
        ioLine->pop_back();

    // Calculate justification for the line.
    Distance space_used = 0;
    if (!ioLine->empty())
        space_used = ioLine->back().userDistanceData;
    Distance offset = CalculateHorizontalOffset(space_used);

    // Render the line and empty our deque for the next line.
    RenderLine(ioLine, offset, space_used);
    ioLine->clear();
}

void GenericTextRenderingEngine::RenderText() {
    LineSegment seg;
    std::vector<LineSegment> current_line;
    
    // If any of our letters extend to the left of their origins, indent
    // all text by that amount, to guarantee that all letters get drawn.
    mInitialIndent = -GetMinimumLeftBearing(mText);

    Distance space_used = mInitialIndent;
    while (mIterator.NextElement(&seg)) {
        
        // Handle line breaks.
        if (seg.isLineBreak) {
            RenderAndResetLine(&current_line);
            space_used = mInitialIndent;
            continue;
        }
        
        // If the segment won't fit, and we already have segments on this
        // line, then start a new line.
        Distance needed =
            MeasureSegment(back_or_null(current_line), &seg, true);
        if (needed > (GetLineLength() - space_used) &&
            space_used > mInitialIndent)
        {
            // Render what we've got.
            RenderAndResetLine(&current_line);
            space_used = mInitialIndent;

            // Our segment didn't fit on the current line
            // AND it disappears at the end of a line.
            // So let's go straight to the next segment.
            if (seg.discardAtEndOfLine)
                continue;
        }

        // If the segment still won't fit, but we're supposed to discard it
        // at the end of a line, go ahead and discard it.  This is needed
        // to draw whitespace-only strings (" ", " "...) onto zero-width
        // lines.
        if (needed > GetUsableLineLength() && seg.discardAtEndOfLine)
            continue;

        // If the segment *still* won't fit, take drastic measures.
        // This is an ugly wart, and it isn't merged well into the
        // overall algorithm.  But isolating this ugly wart seems to
        // keep the rest of the code clean.
        while (needed > GetUsableLineLength()) {
            ASSERT(space_used == mInitialIndent && current_line.empty());
            LineSegment extracted;
            ExtractOneLine(&seg, &extracted);
            extracted.userDistanceData = mInitialIndent +
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
//  Typography::TextRenderingEngine Methods
//=========================================================================

TextRenderingEngine::TextRenderingEngine(const StyledText &inText,
                                         Point inPosition,
                                         Distance inLineLength,
                                         Justification inJustification,
                                         Image *inImage)
    : GenericTextRenderingEngine(inText, inLineLength, inJustification),
      mImage(inImage), mIsFirstLine(true),
      mLineStart(inPosition)
{
    
}

void TextRenderingEngine::DrawGreyMap(Point inPosition,
                                      const GreyMap *inGreyMap,
                                      Color inColor)
{
    // Record the actual BoundingBox of the letter.
    mDrawnBounds.ExpandToInclude(inPosition.x, inPosition.y,
                                 inPosition.x + inGreyMap->width,
                                 inPosition.y + inGreyMap->height);

    // If we have a destination image, draw to it.
    if (mImage)
        mImage->DrawGreyMap(inPosition, inGreyMap, inColor);
}

void TextRenderingEngine::ProcessCharacter(StyledText::value_type *ioPrevious,
                                           StyledText::value_type inCurrent,
                                           Point *ioPosition,
                                           Distance *ioRightBound,
                                           bool inShouldDraw)
{
    // Remember our previous position.
    Point previous_position = *ioPosition;

    // Do our kerning.
    Vector delta = AbstractFace::Kern(*ioPrevious, inCurrent);
    ioPosition->x += delta.x >> 6; // Don't need round_266 (already fitted).
    ASSERT(delta.y == 0);
        
    // Load our glyph.
    Glyph *glyph = inCurrent.style->GetFace()->GetGlyph(inCurrent.value);

    // Draw our glyph (if requested).
    if (inShouldDraw) {
        Point loc = *ioPosition + glyph->GetGreyMapOffset();
        if (inCurrent.style->GetIsShadowed()) {
            Distance offset = inCurrent.style->GetShadowOffset();
            DrawGreyMap(loc + Point(offset, offset), glyph->GetGreyMap(),
                        inCurrent.style->GetShadowColor());
        }
        DrawGreyMap(loc, glyph->GetGreyMap(), inCurrent.style->GetColor());
    }

    // Advance our right edge. This is the sum of the kerned origin, the 
    // bearingX (which is the distance from the origin to the left edge of 
    // the glyph) and the width of the glyph. If this doesn't work, we'll 
    // probably just need to use the size of the greymap. 
    Distance right_shadow =
        max(Distance(0), inCurrent.style->GetShadowOffset());
    Distance new_right_bound = ioPosition->x 
                                 + round_266(glyph->GetMetrics()->horiBearingX 
                                               + glyph->GetMetrics()->width)
                                 + right_shadow;

    // Make sure our new right bound is actually to the right of the old 
    // right bound. 
    *ioRightBound = max(new_right_bound, *ioRightBound);

    // Advance our cursor.
    ioPosition->x += round_266(glyph->GetAdvance().x);
    ASSERT(glyph->GetAdvance().y == 0);


    // Update our previous char.
    *ioPrevious = inCurrent;

    // Make sure that kerning plus advance didn't actually move our
    // position backwards.  This can happen for character combinations
    // such as 'T' and '.' in heavily kerned fonts.  If we omit this
    // calculation, it causes our right bound to be off slightly,
    // and produces visually odd results.
    if (ioPosition->x < previous_position.x) {
        ioPosition->x = previous_position.x;
        // We should probably set *ioPrevious to (kNoSuchCharacter, NULL)
        // here, but intersegment kerning in MeasureSegment only has one
        // character of lookback, so that will break things.
    }
}

Distance
TextRenderingEngine::GetMinimumLeftBearing(const StyledText &inText) const {
    Distance result = 0;
    StyledText::const_iterator cp = inText.begin();
    for (; cp != inText.end(); ++cp)
        result = min(result, cp->GetLeftBearing());
    return result;
}

Distance TextRenderingEngine::MeasureSegment(LineSegment *inPrevious,
                                             LineSegment *inSegment,
                                             bool inAtEndOfLine)
{
    // Attempt to get the last glyph of the previous segment for
    // kerning purposes.  Default to kNoSuchCharacter.
    StyledText::value_type previous(kNoSuchCharacter, NULL);
    if (inPrevious) {
        ASSERT(inPrevious->begin != inPrevious->end);
        StyledText::const_iterator previous = inPrevious->end;
        --previous;
    }

    // Measure the segment.
    Point total(0, 0);
    Distance right_bound = 0;
    StyledText::const_iterator cp = inSegment->begin;
    for (; cp != inSegment->end; ++cp)
        ProcessCharacter(&previous, *cp, &total, &right_bound, false);

    // If necessary, add a trailing hyphen.
    if (inAtEndOfLine && inSegment->needsHyphenAtEndOfLine) {
        StyledText::value_type current(L'-', previous.style);
        ProcessCharacter(&previous, current, &total, &right_bound, false);
    }
        
    ASSERT(total.x >= 0);
    ASSERT(right_bound >= 0);
    // We think this is correct right now, but if something breaks
    // with respect to text measurement, line breaking, or anything
    // related, this may be the culprit. The reason this can return
    // two different values is that the way it is called, by
    // RenderText, it has two different behaviors. In one case,
    // RenderText is trying to see if a box will fit on the current
    // line. In that case, we're testing to see if it will fit at the
    // end of the line, so we care about whether it spills over the
    // bounding box, and thus we care about the absolute right
    // extent. In the other case, we've already measured to see if it
    // fits on the line, and now we need to find out how much space it
    // takes up on the current line. That means we want to get the
    // advancement along the baseline, which is what total.x tracks.
    return inAtEndOfLine ? right_bound : total.x;
}

void TextRenderingEngine::ExtractOneLine(LineSegment *ioRemaining,
                                         LineSegment *outExtracted)
{
    // This routine isn't especially fast, and it produces ugly
    // results, but it's better than nothing.
    ASSERT(ioRemaining != NULL);
    ASSERT(outExtracted != NULL);
    
    Halyard::gLog.Warn("halyard", "Breaking line in middle of word");
    
    // Back up one character at a time until we fit.
    // This code runs in O(N^2) time (with small values of N).
    LineSegment seg = *ioRemaining;
    seg.needsHyphenAtEndOfLine = true; // We'll have to hyphenate.
    do {
        --seg.end;
        if (seg.begin == seg.end)
            throw Error(__FILE__, __LINE__,
                        "Trying to break line in the middle of a character");
    } while (MeasureSegment(NULL, &seg, true) > GetUsableLineLength());
    ASSERT(seg.end != ioRemaining->end);
    
    // Update our line segments.
    *outExtracted = *ioRemaining;
    outExtracted->end = seg.end;
    outExtracted->needsHyphenAtEndOfLine = true;
    ioRemaining->begin = seg.end;
}

void TextRenderingEngine::RenderLine(std::vector<LineSegment> *inLine,
                                     Distance inHorizontalOffset,
                                     Distance inLineLength)
{
    // Figure out how far above and below the baseline our characters will
    // stretch.  This allows us to deal with mixed-sized text, and a few
    // oversized characters (such as the integral sign) which need custom
    // line spacing.
    StyledText::value_type dummy(L' ', GetDefaultStyle());
    Distance line_height = dummy.GetLineHeight(mIsFirstLine);
    Distance line_descender = dummy.GetNominalDescender();
    for (std::vector<LineSegment>::iterator iter1 = inLine->begin();
         iter1 < inLine->end(); ++iter1)
    {
        for (StyledText::const_iterator cp = iter1->begin;
             cp != iter1->end; ++cp)
        {
            // Increase the line height if we have any oversized characters.
            Distance current_height = cp->GetLineHeight(mIsFirstLine);
            if (current_height > line_height)
                line_height = current_height;

            // Increase the line descender if we have any oversized
            // descenders.
            Distance current_descender = cp->GetNominalDescender();
            if (current_descender > line_descender)
                line_descender = current_descender;
        }
    }

    // Figure out where to start drawing text.
    mLineStart.y += line_height;
    Distance line_left_bound = mLineStart.x + inHorizontalOffset;
    Point cursor = mLineStart;
    cursor.x += inHorizontalOffset + GetInitialIndent();
    Distance line_right_bound = line_left_bound;

    // Update our official BoundingBox.  Notice that we use the
    // inLineLength value that we get from the line-breaking code, not the
    // value that we compute below, because we want to return a value that
    // produces identical line-breaking results if used to draw this text
    // again (and not necessarily the tightest visual fit).
    mComputedBounds.ExpandToInclude(line_left_bound,
                                    mLineStart.y - line_height,
                                    line_left_bound + inLineLength,
                                    mLineStart.y + line_descender);
    
    // Draw each character.
    StyledText::value_type previous(kNoSuchCharacter, NULL);
    for (std::vector<LineSegment>::iterator iter2 = inLine->begin();
         iter2 < inLine->end(); ++iter2)
    {
        for (StyledText::const_iterator cp = iter2->begin;
             cp != iter2->end; ++cp)
            ProcessCharacter(&previous, *cp, &cursor, &line_right_bound, true);
    }

    // Draw a trailing hyphen if we need one.
    if (!inLine->empty() && inLine->back().needsHyphenAtEndOfLine) {
        StyledText::value_type current(L'-', previous.style);
        ProcessCharacter(&previous, current, &cursor, &line_right_bound, true);
    }

    // Make sure that our computed bounds are large enough to actually
    // contain the glyphs we've drawn.  For now, this is a hard assertion,
    // because we're fine-tuning the algorithm and we want crash reports if
    // we've overlooked any cases.  Depending on what we ultimately discover,
    // we may or may not turn this off in the future.
    ASSERT(!mDrawnBounds.ExtendsBeyond(mComputedBounds));
    
    // Update our drawing state for the next line.
    mIsFirstLine = false;
}


//=========================================================================
//  Typography::FamilyDatabase::AvailableFace Methods
//=========================================================================

FamilyDatabase::AvailableFace::AvailableFace(const std::string &inRelPath)
    : mRelPath(inRelPath)
{
    // Open up our face file.
    FT_Face face;
    std::string path =
        FileSystem::ResolveFontPath(inRelPath).ToNativePathString();
    CHECK_RESULT(FT_New_Face(*Library::GetLibrary(),
                             path.c_str(), 0, &face));
                
    try {
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
                if (face->available_sizes[i].y_ppem) {
                    found_size = true;
                    mSize = round_266(face->available_sizes[i].y_ppem);
                    break;
                }
            }

            // If we didn't find any sizes, get cranky.
            if (!found_size)
                throw Error(__FILE__, __LINE__, FT_Err_Invalid_File_Format);
        }
    } catch (std::exception &) {
        FT_Done_Face(face);
        throw;
    }
    CHECK_RESULT(FT_Done_Face(face));
}

Face FamilyDatabase::AvailableFace::OpenFace(int inSize) const {
    ASSERT(inSize != kAnySize && inSize > 0);
    ASSERT(mSize == kAnySize || mSize == inSize);

    FileSystem::Path path = FileSystem::ResolveFontPath(mRelPath);
    std::string file = path.ToNativePathString();
    FileSystem::Path metrics_path = path.ReplaceExtension("afm");
    std::string metrics_file = metrics_path.ToNativePathString();

    if (metrics_path.DoesExist())
        return Face(file.c_str(), metrics_file.c_str(), inSize);
    else
        return Face(file.c_str(), NULL, inSize);
}

void FamilyDatabase::AvailableFace::ReadSerializationHeader(std::istream &in) {
    // Check our header information.
    std::string filetype, vers_label;
    int version;
    in >> filetype >> vers_label >> version >> std::ws;
    if (!in || filetype != "facecache" || vers_label != "vers" || version != 2)
        throw Error(__FILE__, __LINE__, "Incorrectly formatted face cache");
    
    // Discard our human-readable comment line.
    std::string junk;
    std::getline(in, junk);
    if (!in)
        throw Error(__FILE__, __LINE__, "Error reading face cache");
}

void FamilyDatabase::AvailableFace::WriteSerializationHeader(std::ostream &out) {
    out << "facecache vers 2" << std::endl
        << "FILE|FAMILY|STYLE|SIZE|IS BOLD|IS ITALIC"
        << std::endl;
}

FamilyDatabase::AvailableFace::AvailableFace(std::istream &in) {
    // Read in our individual fields.
    std::string has_metrics, size, is_bold, is_italic;
    std::getline(in, mRelPath, '|');
    std::getline(in, mFamilyName, '|');
    std::getline(in, mStyleName, '|');
    std::getline(in, size, '|');
    std::getline(in, is_bold, '|');
    std::getline(in, is_italic);
    if (!in)
        throw Error(__FILE__, __LINE__,
                    "Error reading entry from face cache");

    // Needed so eof() will return true after last record.
    // XXX - Will cause problems if font names begin with spaces.
    in >> std::ws; 
    
    // Convert a few numeric values.  Use ternary operator to
    // convert booleans so MSVC++ doesn't whine at us.
    mSize       = atoi(size.c_str());
    mIsBold     = atoi(is_bold.c_str()) ? true : false;
    mIsItalic   = atoi(is_italic.c_str()) ? true : false;
}

void FamilyDatabase::AvailableFace::Serialize(std::ostream &out) const {
    // XXX - This will fail if any of our strings contain '|'.
    out << mRelPath << '|' << mFamilyName << '|' << mStyleName << '|'
        << mSize << '|' << mIsBold << '|' << mIsItalic << std::endl;
}


//=========================================================================
//  Typography::FamilyDatabase::FaceSizeGroup Methods
//=========================================================================

void
FamilyDatabase::FaceSizeGroup::AddAvailableFace(const AvailableFace &inFace) {
    int size = inFace.GetSize();
    if (mAvailableFaces.find(size) != mAvailableFaces.end())
        throw Error(__FILE__, __LINE__,
                    "Tried to add duplicate font to font database");
    mAvailableFaces.insert(std::pair<int,AvailableFace>(size, inFace));
}

Face FamilyDatabase::FaceSizeGroup::GetFace(int inSize) {
    // First, look for an already instantiated face.
    std::map<int,Face>::iterator foundFace = mFaces.find(inSize);
    if (foundFace != mFaces.end())
        return foundFace->second;
    
    // Next, look for either (1) an available face in the exact size or
    // (2) an available face which can be displayed at any size.
    std::map<int,AvailableFace>::iterator found =
        mAvailableFaces.find(inSize);
    if (found == mAvailableFaces.end())
        found = mAvailableFaces.find(kAnySize);
    
    if (found != mAvailableFaces.end()) {
        // Open the face, remember it, and return it.
        Face face(found->second.OpenFace(inSize));
        mFaces.insert(std::pair<int,Face>(inSize, face));
        return face;
    }
    
    // If (after all that) we *still* don't have a face, give up.  If we
    // were feeling very ambitious, we could look for the nearest size and
    // use that.
    throw Error(__FILE__, __LINE__,
                "No matching font (did you try to scale a bitmap font?)");
}

void FamilyDatabase::FaceSizeGroup::Serialize(std::ostream &out) const {
    for (std::map<int,AvailableFace>::const_iterator iter =
             mAvailableFaces.begin();
         iter != mAvailableFaces.end(); ++iter)
        iter->second.Serialize(out);
}


//=========================================================================
//  Typography::FamilyDatabase::Family Methods
//=========================================================================

void FamilyDatabase::Family::AddAvailableFace(const AvailableFace &inFace) {
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

Face FamilyDatabase::Family::GetFace(FaceStyle inStyle, int inSize) {
    ASSERT((inStyle & ~kIntrisicFaceStyles) == 0);

    // Fallback lists.  Because not all faces are available in all styles,
    // we often need to fall back from what we have to less appropriate
    // options.  (We used to do this with exceptions, but it appears that
    // returning a 'Face' object from a 'catch' handler may crash the
    // Microsoft C++.NET runtime.)
    //
    // Each list must begin with the prefered style and end with
    // kRegularFaceStyle.
    const FaceStyle fallback_regular[] = {
        kRegularFaceStyle
    };
    const FaceStyle fallback_bold[] = {
        kBoldFaceStyle, kRegularFaceStyle
    };
    const FaceStyle fallback_italic[] = {
        kItalicFaceStyle, kRegularFaceStyle
    };
    const FaceStyle fallback_bolditalic[] = {
        kBoldItalicFaceStyle, kBoldFaceStyle,
        kItalicFaceStyle, kRegularFaceStyle
    };

    // Figure out which fallback list to use.
    const FaceStyle *fallback = NULL;
    switch (inStyle) {
        case kRegularFaceStyle:    fallback = fallback_regular; break;
        case kBoldFaceStyle:       fallback = fallback_bold; break;
        case kItalicFaceStyle:     fallback = fallback_italic; break;
        case kBoldItalicFaceStyle: fallback = fallback_bolditalic; break;
        default:
            // Illegal style codes!
            throw Error(__FILE__, __LINE__,
                        "Unknown font style codes, giving up");        
    }
    
    // Try all options but kRegularFaceStyle.
    for (; *fallback != kRegularFaceStyle; ++fallback) {
        try {
            switch (*fallback) {
                case kRegularFaceStyle:
                    return mRegularFaces.GetFace(inSize);
                case kBoldFaceStyle:
                    return mBoldFaces.GetFace(inSize);
                case kItalicFaceStyle:
                    return mItalicFaces.GetFace(inSize);
                case kBoldItalicFaceStyle:
                    return mBoldItalicFaces.GetFace(inSize);
                default:
                    // We shouldn't be able to get here.
                    ASSERT(false);
            }
        } catch (std::exception &) {
            // Well, that didn't work.
        }
    }

    // OK, if this fails, we really do want to throw an error.
    ASSERT(*fallback == kRegularFaceStyle);
    return mRegularFaces.GetFace(inSize);
}

void FamilyDatabase::Family::Serialize(std::ostream &out) const {
    mRegularFaces.Serialize(out);
    mBoldFaces.Serialize(out);
    mItalicFaces.Serialize(out);
    mBoldItalicFaces.Serialize(out);
}


//=========================================================================
//  Typography::FamilyDatabase Methods
//=========================================================================

FamilyDatabase *FamilyDatabase::sFamilyDatabase = NULL;
        
FamilyDatabase *FamilyDatabase::GetFamilyDatabase() {
    if (!sFamilyDatabase) {
        sFamilyDatabase = new FamilyDatabase();
        sFamilyDatabase->ReadFromFontDirectory();
    }
    return sFamilyDatabase;
}

bool FamilyDatabase::IsFontFile(const FileSystem::Path &inPath) {
    std::string extension = inPath.GetExtension();
    return (extension == "pfb" || extension == "pcf" || extension == "ttf");
}

void FamilyDatabase::AddAvailableFace(const AvailableFace &inFace) {
    std::string family_name = inFace.GetFamilyName();
    std::map<std::string,Family>::iterator found =
        mFamilyMap.find(family_name);
    if (found == mFamilyMap.end()) {
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
    std::map<std::string,Family>::iterator found =
        mFamilyMap.find(inFamilyName);
    if (found != mFamilyMap.end())
        return found->second.GetFace(inStyle, inSize);
    else
        throw Error(__FILE__, __LINE__,
                    "Unknown font family \"" + inFamilyName + "\"");

    ASSERT(false);
    return *(Face *) NULL; // Never run.
}

void FamilyDatabase::ReadFromFontDirectory() {
    // If a cache file exists, attempt to read from it.
    FileSystem::Path cachePath =
        FileSystem::GetScriptDataDirectory().AddComponent("fontcache.dat");
    if (cachePath.DoesExist() && cachePath.IsRegularFile()) {
        std::ifstream cache(cachePath.ToNativePathString().c_str());
        ReadFromCache(cache);
        return;
    }

    // Otherwise, open all the fonts in the font directory, recursively.
    ScanForFonts("");

    // Attempt to write out a new cache file.
    try {
        std::ofstream cache(cachePath.ToNativePathString().c_str());
        WriteToCache(cache);
    } catch (std::exception &) {
        // Just ignore the exception.
        // TODO - Try logging a warning?
    }
}

void FamilyDatabase::ScanForFonts(const std::string &rel_path) {
    FileSystem::Path path(FileSystem::ResolveFontPath(rel_path));
    if (path.IsRegularFile() && IsFontFile(path)) {
        // Load the face using FreeType, and add it to our database.
        AvailableFace face(rel_path);
        AddAvailableFace(face);
    } else if (path.IsDirectory()) {
        // Scan the directory recursively.
        std::list<std::string> entries = path.GetDirectoryEntries();
        std::list<std::string>::iterator iter = entries.begin();
        for (; iter != entries.end(); iter++) {
            // Make sure we skip invisible files, version control
            // byproducts, and anything else beginning with ".".
            if (iter->length() > 0 && (*iter)[0] != '.') {
                std::string dir_path(rel_path);
                if (dir_path != "")
                    dir_path = dir_path + "/";
                ScanForFonts(dir_path + *iter);
            }
        }
    }
}

void FamilyDatabase::ReadFromCache(std::istream &in) {
    FamilyDatabase::AvailableFace::ReadSerializationHeader(in);
    while (!in.eof())
        AddAvailableFace(AvailableFace(in));
}

void FamilyDatabase::WriteToCache(std::ostream &out) const {
    FamilyDatabase::AvailableFace::WriteSerializationHeader(out);
    for (std::map<std::string,Family>::const_iterator iter =
             mFamilyMap.begin();
         iter != mFamilyMap.end(); ++iter)
        iter->second.Serialize(out);    
}
