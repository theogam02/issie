// to run this script do: "dotnet fsi .\testParser.fsx"

// maybe add code to generate parser
// need to run parser.js somehow
// if it says wrong Fable version, check the version Issie uses and update it here
// #r "nuget: Fable, 4.0.0-theta-018"
// #load "../NearleyBindings.fs"
module TestParser

open NearleyBindings
printfn "Starting Verilog parser tests"

NearleyBindings.importGrammar
NearleyBindings.importFix
NearleyBindings.importParser

parseFromFile("")
