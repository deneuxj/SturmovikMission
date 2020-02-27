namespace System
open System.Reflection

module AssemblyVersionInformation =
    let [<Literal>] AssemblyTitle = "SturmovikMission.DataProvider.Runtime"
    let [<Literal>] AssemblyProduct = "SturmovikMission.DataProvider"
    let [<Literal>] AssemblyDescription = "Type provider for IL-2 Sturmovik: Great Battles mission files"
    let [<Literal>] AssemblyVersion = "6.0.0"
    let [<Literal>] AssemblyFileVersion = "6.0.0"

[<assembly: AssemblyTitleAttribute(AssemblyVersionInformation.AssemblyTitle)>]
[<assembly: AssemblyProductAttribute(AssemblyVersionInformation.AssemblyProduct)>]
[<assembly: AssemblyDescriptionAttribute(AssemblyVersionInformation.AssemblyDescription)>]
[<assembly: AssemblyVersionAttribute(AssemblyVersionInformation.AssemblyVersion)>]
[<assembly: AssemblyInformationalVersionAttribute(AssemblyVersionInformation.AssemblyVersion)>]
[<assembly: AssemblyFileVersionAttribute(AssemblyVersionInformation.AssemblyFileVersion)>]
do ()