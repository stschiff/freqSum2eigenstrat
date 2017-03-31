module OrderedZip

open System.Collections.Generic

let rec orderedZipWithEnum (e1:IEnumerator<'T1>) isValid1 (e2:IEnumerator<'T2>) isValid2 func = 
    seq {
        match (isValid1, isValid2) with
            | (true, true) ->
                let c1 = e1.Current
                let c2 = e2.Current
                match func c1 c2 with
                    | x when x < 0 ->
                        yield (Some c1, None)
                        let v = e1.MoveNext()
                        yield! orderedZipWithEnum e1 v e2 isValid2 func
                    | x when x > 0 ->
                        yield (None, Some c2)
                        let v = e2.MoveNext()
                        yield! orderedZipWithEnum e1 isValid1 e2 v func
                    | _ ->
                        yield (Some c1, Some c2)
                        let v1 = e1.MoveNext()
                        let v2 = e2.MoveNext()
                        yield! orderedZipWithEnum e1 v1 e2 v2 func
            | (true, false) ->
                let c1 = e1.Current
                yield (Some c1, None)
                let v = e1.MoveNext()
                yield! orderedZipWithEnum e1 v e2 isValid2 func
            | (false, true) -> 
                let c2 = e2.Current
                yield (None, Some c2)
                let v = e2.MoveNext()
                yield! orderedZipWithEnum e1 isValid1 e2 v func
            | _ -> ()
    }

let orderedZip (seq1:'T1 seq) (seq2:'T2 seq) compareFunc =
    let enum1 = seq1.GetEnumerator()
    let isValid1 = enum1.MoveNext()
    let enum2 = seq2.GetEnumerator()
    let isValid2 = enum2.MoveNext()
    orderedZipWithEnum enum1 isValid1 enum2 isValid2 compareFunc
