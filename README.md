# adventofcode

To run a solution...
`dotnet run m n` where m is the day # and n is the part #
for example `dotnet run 1 2` will run the solution for day 1, part 2

To start a new year...

1. cd to this root directory
2. run `dotnet new console -lang "F#" -o src/aoc2023`
3. run `dotnet sln add src/aoc2023/aoc2023.fsproj`
4. copy the `main` mechanism from a previous year's Program.fs to this year's Program.fs
    - subject to change in the future, this is kind of disgusting lol

To add a new day...
1. create a new day file in the format `day{n}.fs`, append a 0 to single digit days
2. name the top level module Day n, where n is the day # without an appended 0
    - just to be confusing ;) 
3. add an entry for this new file in the project .fsproj
    - it MUST be listed before Program.fs!
3. define `answerPt1` and `answerPt2` while working on the solutions!