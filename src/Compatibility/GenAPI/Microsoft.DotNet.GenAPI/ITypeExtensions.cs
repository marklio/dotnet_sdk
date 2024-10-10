// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Microsoft.CodeAnalysis;

namespace Microsoft.DotNet.GenAPI
{
    internal static class ITypeExtensions
    {
        /// <summary>
        /// Returns the "least" accessibility WRT the type and any enclosing type.
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static Accessibility GetAccessibilityWithParent(this ITypeSymbol type)
        {
            return (Accessibility)Math.Min((int)type.DeclaredAccessibility, (int)(type.ContainingType?.GetAccessibilityWithParent() ?? Accessibility.Public));
        }
    }
}
