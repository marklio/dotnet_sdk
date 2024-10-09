// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Microsoft.CodeAnalysis;

namespace Microsoft.DotNet.ApiSymbolExtensions.Filtering
{
    /// <summary>
    /// Implements the logic of filtering out api.
    /// Reads the file with the list of attributes, types, members in DocId format.
    /// </summary>
    public class DocIdSymbolFilter(string[] docIds, bool docIdsAreIncludeOnly) : ISymbolFilter
    {
        //constructor for compat.
        public DocIdSymbolFilter(string[] docIdsToExcludeFiles) : this(docIdsToExcludeFiles, false) { }

        private readonly HashSet<string> _docIds = new(ReadDocIdsAttributes(docIds));
        private readonly bool _docIdsAreIncludeOnly = docIdsAreIncludeOnly;

        /// <summary>
        ///  Determines whether the <see cref="ISymbol"/> should be included.
        /// </summary>
        /// <param name="symbol"><see cref="ISymbol"/> to evaluate.</param>
        /// <returns>True to include the <paramref name="symbol"/> or false to filter it out.</returns>
        public bool Include(ISymbol symbol)
        {
            string? docId = symbol.GetDocumentationCommentId();
            //if there's no doc id, always include it
            if (docId is null) return true;
            //if the doc id is in the list, return the meaning of the list
            if (_docIds.Contains(docId))
            {
                return _docIdsAreIncludeOnly;
            }

            return !_docIdsAreIncludeOnly;
        }

        private static IEnumerable<string> ReadDocIdsAttributes(IEnumerable<string> docIdsToExcludeFiles)
        {
            foreach (string docIdsToExcludeFile in docIdsToExcludeFiles)
            {
                foreach (string id in File.ReadAllLines(docIdsToExcludeFile))
                {
#if NET
                    if (!string.IsNullOrWhiteSpace(id) && !id.StartsWith('#') && !id.StartsWith("//"))
#else
                    if (!string.IsNullOrWhiteSpace(id) && !id.StartsWith("#") && !id.StartsWith("//"))
#endif
                    {
                        yield return id.Trim();
                    }
                }
            }
        }
    }
}
