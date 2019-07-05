// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLFilestore




[<AutoOpen>]
module Operations = 

    open System.IO

    open SLFilestore

    let directoryName (directory : DirectoryObject) : string = 
        match directory with
        | DirectoryObject(name, _, _) -> name

    let directoryProperties (directory : DirectoryObject) : FoProperties = 
        match directory with
        | DirectoryObject(_, props, _) -> props


    let directoryKids (directory : DirectoryObject) : FsObject list = 
        match directory with
        | DirectoryObject(_, _, kids) -> kids


    let fileName (file : FileObject) : string = 
        match file with
        | FileObject(name, _, _) -> name

    let fileProperties (file : FileObject) : FoProperties = 
        match file with
        | FileObject(_, props, _) -> props

    let fileSize (file : FileObject) : int64 = 
        match file with
        | FileObject(_, _, size) -> size
    /// Returns empty string for no extension.
    let fileExtension (file : FileObject) : string = 
        Path.GetExtension(fileName file)

