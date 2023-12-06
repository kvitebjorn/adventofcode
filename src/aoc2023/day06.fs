module Day6

let times     = [38L;94L;79L;70L]
let distances = [241L;1549L;1074L;1091L]

let rec race timeLimit recordDistance speed acc =
    match speed = timeLimit with
    | true -> acc
    | _    -> 
        let timeRemaining = timeLimit - speed
        let distance = speed * timeRemaining
        let toAcc = if distance > recordDistance then 1 else 0
        race timeLimit recordDistance (speed + 1L) (acc + toAcc)

let solvePt1 =
    let product = List.fold2 (fun acc t d -> acc * race t d 0 0) 1 times distances
    $"{product.ToString()}"

let answerPt1 = solvePt1

let solvePt2 =
    let wins = race 38947970L 241154910741091L 0 0
    $"{wins.ToString()}"

let answerPt2 = solvePt2