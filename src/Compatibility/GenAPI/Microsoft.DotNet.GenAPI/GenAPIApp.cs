// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

#if !NET
using System.Text.RegularExpressions;
#endif
using Microsoft.CodeAnalysis;
using Microsoft.DotNet.ApiSymbolExtensions;
using Microsoft.DotNet.ApiSymbolExtensions.Filtering;
using Microsoft.DotNet.ApiSymbolExtensions.Logging;
using Microsoft.DotNet.GenAPI.Filtering;

namespace Microsoft.DotNet.GenAPI
{
    /// <summary>
    /// Class to standardize initialization and running of GenAPI tool.
    /// Shared between CLI and MSBuild tasks frontends.
    /// </summary>
    public static class GenAPIApp
    {
        /// <summary>
        /// Initialize and run Roslyn-based GenAPI tool.
        /// </summary>
        public static void Run(ILog logger,
            string[] assemblies,
            string[]? assemblyReferences,
            string? outputPath,
            string[]? headerFiles,
            string? exceptionMessage,
            string[]? excludeApiFiles,
            string[]? includeApiFiles, //adding this is a breaking change. What's the compat promise here? Do we need an overload or some other pattern to allow improvements here without too much complexity?
            string[]? excludeAttributesFiles,
            bool respectInternals,
            bool includeAssemblyAttributes)
        {
            bool resolveAssemblyReferences = assemblyReferences?.Length > 0;

            // Create, configure and execute the assembly loader.
            AssemblySymbolLoader loader = new(resolveAssemblyReferences, respectInternals);
            if (assemblyReferences is not null)
            {
                loader.AddReferenceSearchPaths(assemblyReferences);
            }
            IReadOnlyList<IAssemblySymbol?> assemblySymbols = loader.LoadAssemblies(assemblies);

            string headerFileText = ReadHeaderFiles(headerFiles);

            AccessibilitySymbolFilter accessibilitySymbolFilter = new(
                respectInternals,
                includeEffectivelyPrivateSymbols: true,
                includeExplicitInterfaceImplementationSymbols: true);

            // Configure the symbol filter
            CompositeSymbolFilter symbolFilter = new();
            if (excludeApiFiles is not null)
            {
                symbolFilter.Add(new DocIdSymbolFilter(excludeApiFiles));
            }
            if (includeApiFiles is not null && includeApiFiles.Length > 0)
            {
                symbolFilter.Add(new DocIdSymbolFilter(includeApiFiles, docIdsAreIncludeOnly: true));
            }
            symbolFilter.Add(new ImplicitSymbolFilter());
            symbolFilter.Add(accessibilitySymbolFilter);

            // Configure the attribute data symbol filter
            CompositeSymbolFilter attributeDataSymbolFilter = new();
            if (excludeAttributesFiles is not null)
            {
                attributeDataSymbolFilter.Add(new DocIdSymbolFilter(excludeAttributesFiles));
            }
            attributeDataSymbolFilter.Add(accessibilitySymbolFilter);

            // Invoke the CSharpFileBuilder for each directly loaded assembly.
            foreach (IAssemblySymbol? assemblySymbol in assemblySymbols)
            {
                if (assemblySymbol is null)
                    continue;

                using TextWriter textWriter = GetTextWriter(outputPath, assemblySymbol.Name);
                textWriter.Write(headerFileText);

                using CSharpFileBuilder fileBuilder = new(logger,
                    symbolFilter,
                    attributeDataSymbolFilter,
                    textWriter,
                    exceptionMessage,
                    includeAssemblyAttributes,
                    loader.MetadataReferences);

                fileBuilder.WriteAssembly(assemblySymbol);
            }

            if (loader.HasRoslynDiagnostics(out IReadOnlyList<Diagnostic> roslynDiagnostics))
            {
                foreach (Diagnostic warning in roslynDiagnostics)
                {
                    logger.LogWarning(warning.Id, warning.ToString());
                }
            }

            if (loader.HasLoadWarnings(out IReadOnlyList<AssemblyLoadWarning> loadWarnings))
            {
                foreach (AssemblyLoadWarning warning in loadWarnings)
                {
                    logger.LogWarning(warning.DiagnosticId, warning.Message);
                }
            }
        }

        // Creates a TextWriter capable of writing into Console or a cs file.
        private static TextWriter GetTextWriter(string? outputDirPath, string assemblyName)
        {
            if (outputDirPath is null)
            {
                return Console.Out;
            }

            string fileName = assemblyName + ".cs";
            if (Directory.Exists(outputDirPath))
            {
                return File.CreateText(Path.Combine(outputDirPath, fileName));
            }

            return File.CreateText(outputDirPath);
        }

        // Read the header file if specified, or use default one.
        private static string ReadHeaderFiles(string[]? headerFiles)
        {
            const string defaultFileHeader = """
            //------------------------------------------------------------------------------
            // <auto-generated>
            //     This code was generated by a tool.
            //
            //     Changes to this file may cause incorrect behavior and will be lost if
            //     the code is regenerated.
            // </auto-generated>
            //------------------------------------------------------------------------------

            """;

            string header = headerFiles is not null && headerFiles.Length > 0 ?
                String.Join(Environment.NewLine, from headerFile in headerFiles select File.ReadAllText(headerFile)) :
                defaultFileHeader;

#if NET
            header = header.ReplaceLineEndings();
#else
            header = Regex.Replace(header, @"\r\n|\n\r|\n|\r", Environment.NewLine);
#endif

            return header;
        }
    }
}
