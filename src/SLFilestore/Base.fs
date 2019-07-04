// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLFilestore




[<AutoOpen>]
module Base = 

    open System

    // Mode is not currently interpreted
    type FileInfo = 
        { Mode : string
          ModificationTime : DateTime
        }

    /// File + Directory (not "folder")


    type FileObject = 
        | FileObject of name : string * info :  FileInfo * size : int64

    and DirectoryObject = 
        | DirectoryObject of name : string * info :  FileInfo * kids : FsObject list

    and FsObject = 
        | FsFile of FileObject
        | FsDirectory of DirectoryObject

    
    type Filestore = 
        | Filestore of pathTo : string * kids : FsObject list



