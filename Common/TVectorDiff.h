// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; -*-

#if !defined (TVectorDiff_H)
#define TVectorDiff_H

BEGIN_NAMESPACE_FIVEL

//////////
// Compute the "largest common subsequence" (LCS) of two vectors.  The LCS
// of two vectors v_1 and v_2 is defined as the largest vector which can
// obtained by deleting individual items from v_1 and v_2 ("A Fast
// Algorithm for Computing Longest Common Subsequences of Small Alphabet
// Size", Francis&Poon).  This the heart of most primitive 'diff' operations.
// An example in Scheme:
//
// (define v1 '(a b c d e f))
// (define v2 '(a b f c d e))
// (lcs v1 v2) => '(a b c d e)
//
// An extremely inefficient recursive implementation in Scheme:
//
// (define (lcs s1 s2)
//   (cond
//     [(or (null? s1) (null? s2))
//      '()]
//     [(eq? (car s1) (car s2)) 
//      ;; This element matches in both lists, so assume it's OK and compute
//      ;; the LCS of the element tails.
//      (cons (car s1) (lcs (cdr s1) (cdr s2)))]
//     [else
//      ;; Try deleting the first element of each list, compute the LCS
//      ;; recursively for the results, and see which gives us the
//      ;; largest LCS.
//      (let [[lcs1 (lcs (cdr s1) s2)]
//            [lcs2 (lcs s1 (cdr s2))]]
//        (if (>= (length lcs1) (length lcs2))
//            lcs1
//            lcs2))]))
//
// Apparently D.S. Hirschberg figured out how to solve this problem using
// dynamic programming in the 1970s.  I can't find his paper online, so
// I've decided to puzzle out an algorithm from the hints in later papers.
//
template <typename T> inline
void LargestCommonSubsequence(const std::vector<T> &v1,
                              const std::vector<T> &v2,
                              std::vector<T> &outLCS)
{
    using std::vector;

    size_t i, j;
    size_t size1 = v1.size();
    size_t size2 = v2.size();

    // Create a two-dimensional array.  We'll use this to record
    // intermediate results (if we used recursion, we'd have to calculate
    // the same answers over and over).  This technique is known as
    // "dynamic programming" or "memoization".
    vector< vector<size_t> > lcs_len(size1 + 1,
                                     vector<size_t>(size2 + 1, 0));

    // Starting with the first characters of v1 and v2, calculate the
    // length of the LCS for progressively longer prefixes.
    for (i = 1; i <= size1; i++)
        for (j = 1; j <= size2; j++)
            if (v1[i-1] == v2[j-1])
                // Our elements match, so add 1 to the previous result.
                lcs_len[i][j] = lcs_len[i-1][j-1] + 1;
            else
                // Our elements don't match, so propagate the largest result.
                lcs_len[i][j] = std::max(lcs_len[i][j-1], lcs_len[i-1][j]);

    // Empty our output vector and reserve enough space to hold our results.
    outLCS.clear();
    outLCS.reserve(lcs_len[size1][size2]);

    // Walk backwards from our final result, building a "decision history"
    // of which elements were hypothetically chosen at each stage.
    i = size1;
    j = size2;
    size_t cur = lcs_len[i][j];
    while (cur > 0)
    {
        size_t prev1 = lcs_len[i-1][j];
        size_t prev2 = lcs_len[i][j-1];
        if (cur == prev1)
            i--;
        else if (cur == prev2)
            j--;
        else if (cur == prev1 + 1)
            outLCS.push_back(v1[--i]);
        else if (cur == prev2 + 1)
            outLCS.push_back(v2[--j]);
        else
            ASSERT(false);

        cur = lcs_len[i][j];
    }

    // Reverse the entries in our vector.
    std::reverse(outLCS.begin(), outLCS.end());
}

END_NAMESPACE_FIVEL

#endif // TVectorDiff_H
