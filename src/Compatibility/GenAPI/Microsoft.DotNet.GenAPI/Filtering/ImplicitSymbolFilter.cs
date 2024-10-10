// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Microsoft.CodeAnalysis;
using Microsoft.DotNet.ApiSymbolExtensions;
using Microsoft.DotNet.ApiSymbolExtensions.Filtering;

namespace Microsoft.DotNet.GenAPI.Filtering
{
    /// <summary>
    /// Filter out implicitly generated members for properties, events, etc.
    /// </summary>
    public class ImplicitSymbolFilter : ISymbolFilter
    {
        /// <summary>
        /// Determines whether implicitly generated symbols <see cref="ISymbol"/> should be included.
        /// </summary>
        /// <param name="symbol"><see cref="ISymbol"/> to evaluate.</param>
        /// <returns>True to include the <paramref name="symbol"/> or false to filter it out.</returns>
        public bool Include(ISymbol symbol)
        {
            if (symbol is IMethodSymbol method)
            {
                if (method.IsImplicitlyDeclared ||
                    method.Kind == SymbolKind.NamedType ||
                    method.MethodKind == MethodKind.PropertyGet ||
                    method.MethodKind == MethodKind.PropertySet ||
                    method.MethodKind == MethodKind.EventAdd ||
                    method.MethodKind == MethodKind.EventRemove ||
                    method.MethodKind == MethodKind.EventRaise ||
                    method.MethodKind == MethodKind.DelegateInvoke)
                {
                    return false;
                }

                // If the method is an explicitly implemented getter or setter, exclude it.
                // https://github.com/dotnet/roslyn/issues/53911
                if (method.MethodKind == MethodKind.ExplicitInterfaceImplementation &&
                    method.ExplicitInterfaceImplementations.Any(m => m is { MethodKind: MethodKind.PropertyGet or MethodKind.PropertySet }))
                {
                    return false;
                }
            }

            if (symbol is ITypeSymbol type)
            {
                if (type.GetAccessibilityWithParent() == Accessibility.Internal && (
                    // exclude the compiler generated `<Module>` type
                    type.Name == "<Module>" ||
                    // exclude any types which the compiler embedded - marked with EmbeddedAttribute.
                    // these will be generated by the compiler when compiling C# syntax that requires them.
                    type.GetAttributes().Any(a => a.AttributeClass?.Name == "EmbeddedAttribute" && a.AttributeClass?.ContainingNamespace.ToDisplayString() == "Microsoft.CodeAnalysis") ||
                    // exclude any types which the compiler generated - marked with CompilerGeneratedAttribute.
                    // these will be generated by the compiler when compiling C# syntax that requires them.
                    type.GetAttributes().Any(a => a.AttributeClass?.Name == "CompilerGeneratedAttribute" && a.AttributeClass?.ContainingNamespace.ToDisplayString() == "System.Runtime.CompilerServices")))
                {
                    return false;
                }
            }

            // exclude compiler-synthesized members on record
            if (symbol.ContainingType is { IsRecord: true })
            {
                if (symbol.IsCompilerGenerated())
                {
                    return false;
                }

                // see if we can identify the record parameter syntax by locating the compiler generated constructor
                if (symbol.ContainingType.TryGetRecordConstructor(out IMethodSymbol? recordConstructor))
                {
                    // exclude the compiler generated constructor
                    if (SymbolEqualityComparer.Default.Equals(symbol, recordConstructor))
                    {
                        return false;
                    }

                    // exclude the compiler generated properties
                    if (symbol is IPropertySymbol)
                    {
                        // Exclude members with the same name as the record constructor's parameters
                        return !recordConstructor.Parameters.Any(p => p.Name == symbol.Name);
                    }
                }
            }

            return true;
        }
    }
}
