// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\FParsec\1.0.4-rc3\lib\netstandard1.6"
#r "FParsec"
#r "FParsecCS"

#load "..\src\SLFilestore\Base.fs"
#load "..\src\SLFilestore\DirectoryListing.fs"
open SLFilestore.DirectoryListing

let localFile (relativePath : string) : string = 
    Path.Combine(__SOURCE_DIRECTORY__, "..", relativePath)

let demo01 () = 
    localFile @"data\factx.txt"
        |> readDirRecurseOutput