// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLFilestore




[<AutoOpen>]
module Base = 

    open System

    open SLFormat.RoseTree


    // Mode is not currently interpreted
    type FoProperties = 
        { Mode : string
          ModificationTime : DateTime
        }

    /// File + Directory (not "folder")


    type FileObject = 
        | FileObject of name : string * info : FoProperties * size : int64

    and DirectoryObject = 
        | DirectoryObject of name : string * info : FoProperties * kids : FsObject list

    and FsObject = 
        | FsFile of FileObject
        | FsDirectory of DirectoryObject

    
    type Filestore = 
        | Filestore of pathTo : string * kids : FsObject list
        
        member x.PathTo 
            with get () = match x with | Filestore(path, _ ) -> path

        member x.Kids
            with get () = match x with | Filestore(_, kids) -> kids


    
    let private toRoseTree (store : Filestore) : RoseTree<string> = 
        let rec work fsobj cont = 
            match fsobj with
            | FsFile(FileObject(name, _, _)) ->
                cont (RoseTree(name, []))
            | FsDirectory(DirectoryObject(name, _, kids)) -> 
                workList kids (fun ks ->
                cont (RoseTree(name, ks)))
        and workList xs cont = 
            match xs with
            | [] -> cont []
            | k1 :: rest -> 
                work k1 (fun v1 ->
                workList rest (fun acc ->
                cont (v1 :: acc)))

        workList store.Kids (fun kids -> RoseTree(store.PathTo, kids))

    let drawFilestore (store : Filestore) : string =
        store |> toRoseTree |> drawTree



