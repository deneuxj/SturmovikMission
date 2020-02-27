
This is an F# type provider for IL-2 Sturmovik: Great Battles mission files.  It has separate design-time and runtime assemblies.

Paket is used to acquire the type provider SDK and build the nuget package (you can remove this use of paket if you like)

Building:

    dotnet tool restore
    dotnet paket update
    dotnet build -c release

    dotnet paket pack nuget --version 6.0.0 # Replace 6.0.0 by the proper version number, which should match AssemblyInfo.cs
