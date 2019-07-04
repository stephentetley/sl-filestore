// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLFilestore




[<AutoOpen>]
module DirectoryListing = 

    open System
    open System.IO

    open FParsec


    type Name = string
    type FilePath = string
    type Size = int64


    // Mode is not currently interpreted
    type Properties = 
        { Mode : string option
          ModificationTime : DateTime option
        }
 
    type FileObjectRow = 
        | FolderRow of Name * Properties * FilePath
        | FileRow of Name * Properties * Size * FilePath
        member x.Name 
            with get () : string = 
                match x with
                | FolderRow(name,_,_) -> name
                | FileRow(name,_,_,_) -> name

        member x.Properties 
            with get () :Properties = 
                match x with
                | FolderRow(_,props,_) -> props
                | FileRow(_,props,_,_) -> props

        member x.Path
            with get () : string = 
                match x with
                | FolderRow(_,_,path) -> path
                | FileRow(_,_,_,path) -> path


    type Block = 
        { SubPath: FilePath 
          Rows: FileObjectRow list }
        




    let makeDateTime (year:int) (month:int) (day:int) (hour:int) (minute:int) (second:int) : DateTime = 
        new DateTime(year=year, month=month, day=day, hour=hour, minute=minute, second=second)



    
    // *************************************
    // PARSER

    // Parsing output of "dir" or "dir -Recurse" (Windows)

    // Utility combinators
    let private ws : Parser<string,unit> = manyChars (pchar ' ' <|> pchar '\t')
    let private ws1 : Parser<string,unit> = many1Chars (pchar ' ' <|> pchar '\t')

    let private symbol (p:Parser<'a,unit>)      : Parser<'a,unit> = p .>> ws

    let private keyword (s:string) : Parser<string,unit> = pstring s .>> ws
    let private keyword1 (s:string) : Parser<string,unit> = pstring s .>> ws1


    let private emptyLine : Parser<unit,unit> = newline >>. preturn ()

    // Names may span multiple lines
    let private pName : Parser<Name,unit> = 
        let line1 = restOfLine true
        let linesK = many1 (pchar ' ') >>. restOfLine true
        parse { 
            let! s = line1 
            let! ss = many linesK 
            let name1 = String.concat "" (s::ss)
            return name1.Trim()
            }


    // Note this is UK centric    
    let private pDateTime : Parser<DateTime,unit> = 
        pipe5   pint32 
                (pchar '/' >>. pint32) 
                (pchar '/' >>. symbol pint32) 
                pint32 
                (pchar ':' >>. pint32)
                (fun dd dm dy th tm -> makeDateTime dy dm dd th tm 0)
    
    let private pMode : Parser<string,unit> = many1Chars (lower <|> pchar '-') 

    let private isDir (mode:string) : bool = mode.StartsWith("d")



    let private pDirectoryDirective : Parser<Name,unit> = 
        let indent = manyChars (pchar ' ')
        indent >>. keyword1 "Directory:" >>. pName

    let private pHeadings : Parser<string list,unit> = 
        let columns = pipe4 (keyword "Mode")
                            (keyword "LastWriteTime")
                            (keyword "Length")
                            (keyword "Name")
                            (fun a b c d -> [a;b;c;d])
        let underline = restOfLine false
        columns .>> newline .>> underline


    let private pFolder (pathTo:string) (mode:string) : Parser<FileObjectRow, unit> = 
        parse { 
            let! timestamp = symbol pDateTime 
            let! name = pName 
            return (FolderRow (name, { Mode = Some mode; ModificationTime = Some timestamp}, pathTo))
            }

    let private pFile (pathTo:string) (mode:string) : Parser<FileObjectRow, unit> = 
        parse { 
            let! timestamp = symbol pDateTime
            let! size = symbol pint64
            let! name = pName 
            return (FileRow (name, { Mode = Some mode; ModificationTime = Some timestamp}, size, pathTo))
            }

    // Note - file store is flat at parse time (represented as a "Row")
    // It needs postprocessing to build.
    let private pRow (pathTo : string) : Parser<FileObjectRow, unit> = 
        let parseK mode = 
            if isDir mode then pFolder pathTo mode else pFile pathTo mode
        (symbol pMode) >>= parseK





    let private pBlock : Parser<Block, unit> = 
        parse { 
            let! parent = (spaces >>. pDirectoryDirective) 
            do! emptyLine
            do! emptyLine
            let! _ = pHeadings .>> newline
            let! rows = many1 (pRow parent)
            return { SubPath = parent; Rows = rows }
            }



    let private pListing : Parser<Block list,unit> = many (pBlock .>> spaces)

    let readDirRecurseOutput (inputPath:string) : Choice<string,Block list> = 
        let source = File.ReadAllText(inputPath)
        match runParserOnString pListing () inputPath source with
        | Success(a,_,_) -> Choice2Of2 a
        | Failure(s,_,_) -> Choice1Of2 s



    // *************************************
    // Build from flat.


    // Note a Unix ``ls -lR`` listing does not a 'pathTo' pendant.
    
    type InitialKids = Map<string, FileObjectRow list>
    

    let private makeInitialKids (blocks : Block list) :  InitialKids = 
        let step acc (block : Block) = Map.add block.SubPath block.Rows acc
        List.fold step Map.empty blocks

    /// Root is always first
    let private getRoot (blocks : Block list) : Block option = 
        List.tryHead blocks


    /// let recurBuild 

        (* 
        
        
makeRecur :: Level1Kids -> String -> Element -> Content
makeRecur _     _       (WinFile _ mtime sz name)  =
    let props = Properties { access_time = Nothing
                           , modification_time = Just mtime }
    in FsFile name props sz

makeRecur store pathto  (WinFolder _ mtime name)   = 
   let fullname = pathto ++ ('\\':name)
       props    = Properties { access_time = Nothing
                             , modification_time = Just mtime }
       kids1    = MAP.findWithDefault [] fullname store
       kids2    = map (makeRecur store fullname) kids1
   in FsFolder name props kids2
        
        *)
    
    let buildCPS (store : InitialKids) 
                 (rootPath : string) 
                 (rows : FileObjectRow list) : FsObject list = 
        let rec work (fullPath : string) (row : FileObjectRow) cont = 
            match row with
            | FolderRow (name, props, path) -> failwith "todo"
            | FileRow (name, props, size, path) -> failwith "todo"
        and workList (fullPath : string) (rows : FileObjectRow list) cont = 
            match rows with
            | [] -> cont []
            | r1 :: rest -> 
                work fullPath r1 (fun v1 -> 
                workList fullPath rest (fun acc ->
                cont (v1 :: acc)))
        workList rootPath rows (fun xs -> xs)

    let buildTopDown (listing : Block list) : Filestore option = 
        match getRoot listing with
        | None -> None
        | Some root -> 
            let initialKids = makeInitialKids listing
            let kids = buildCPS initialKids root.SubPath root.Rows
            Filestore(root.SubPath, kids) |> Some

