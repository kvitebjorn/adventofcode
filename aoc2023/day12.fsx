open System.IO

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun line ->
        let part = line.Split(' ')
        let line = part[0]
        let nums = part[1].Split(',') |> Array.map int
        (line, nums))

let isCandidate (str: string) num =
    str.Length > num
    && (str[num] = '.' || str[num] = '?')
    && str[.. num - 1] |> Seq.forall (fun ch -> ch = '?' || ch = '#')

let doTheThing (line: string) (nums: int array) =
    let line = line + "."
    let nums = Array.append nums [| 0 |] |> Array.rev
    let (m, n) = (nums.Length, line.Length)
    let table = Array2D.create m (n + 1) 0L
    let mutable j = n - 1

    while j >= 0 && (line[j] = '?' || line[j] = '.') do
        table[0, j + 1] <- 1
        j <- j - 1

    let op i j = table[i, j + 1]

    let dmg i j =
        let slice = line.Substring(j, n - j)

        if isCandidate slice nums[i] then
            table[i - 1, j + nums[i] + 1]
        else
            0

    for i = 1 to m - 1 do
        for j = n - 2 downto 0 do
            match line[j] with
            | '.' -> table[i, j] <- op i j
            | '#' -> table[i, j] <- dmg i j
            | '?' -> table[i, j] <- dmg i j + op i j
            | _ -> ()

    table[m - 1, 0] |> int64

let arrangements i =
    Array.map (fun (line, nums) -> doTheThing line nums) i |> Array.sum

let total = arrangements input
printfn $"{total}"

let newInput =
    Array.map
        (fun (line, nums) ->
            let newLine = sprintf "%s?%s?%s?%s?%s" line line line line line
            let newNums = Array.replicate 5 nums |> Array.concat
            (newLine, newNums))
        input

let totalPt2 = arrangements newInput
printfn $"{totalPt2}"
