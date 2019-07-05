// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"

// SLFormat  (not on nuget.org)
#I @"C:\Users\stephen\.nuget\packages\slformat\1.0.2-alpha-20190616\lib\netstandard2.0"
#r @"SLFormat.dll"

#load "..\src\SLFilestore\Base.fs"
#load "..\src\SLFilestore\DirectoryListing.fs"
#load "..\src\SLFilestore\Operations.fs"
open SLFilestore

let localFile (relativePath : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", relativePath)

let demo01 () = 
    match localFile @"data\factx.txt" |> readDirRecurseOutput with
    | Error msg -> printfn "%s" msg
    | Ok store -> drawFilestore store |> printfn "%s"