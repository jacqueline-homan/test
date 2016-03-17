type Range = 
    | Single of int 
    | Range of int * int
        
let folder s item =
    match s with
    | [] -> [(item,item)]
    | (xmin,xmax)::xs ->
        let delta = item - xmax
        if delta > 1 then
            List.concat [[(item,item)]; s]
        else
            List.concat [[(xmin,item)]; xs]

let make_ranges xs =
    Seq.fold folder [] xs
    |> Seq.map (fun (min,max) -> if min = max then Single min else Range(min,max) )
    |> Seq.rev
 
printf "%A" (make_ranges [| -2; 0; 1; 2; 2; 3; 5; 5; 8; 9; 10; 12 |])

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

