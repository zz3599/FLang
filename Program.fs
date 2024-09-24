module HelloSquare

/// Squares a value.
let square x = x * x

/// Adds 1 to a value.
let addOne x = x + 1

/// Tests if an integer value is odd via modulo.
///
/// '<>' is a binary comparison operator that means "not equal to".
let isOdd x = x % 2 <> 0

/// A list of 5 numbers.  More on lists later.
let numbers = [ 1; 2; 3; 4; 5 ]

/// Given a list of integers, it filters out the even numbers,
/// squares the resulting odds, and adds 1 to the squared odds.
let squareOddValuesAndAddOne values =
    let odds = List.filter isOdd values
    let squares = List.map square odds
    let result = List.map addOne squares
    List.map
    result

let swapElems (a, b) = (b, a)


let rnd = System.Random()

/// This is an infinite sequence which is a random walk.
/// This example uses yield! to return each element of a subsequence.
let rec randomWalk x =
    seq { yield x
          yield! randomWalk (x + rnd.NextDouble() - 0.5) }

/// This example shows the first 100 elements of the random walk.
let first100ValuesOfRandomWalk =
    randomWalk 5.0
    |> Seq.truncate 100
    |> Seq.toList

printfn $"First 100 elements of a random walk: {first100ValuesOfRandomWalk}"

module DiscriminatedUnions =

    /// The following represents the suit of a playing card.
    type Suit =
        | Hearts
        | Clubs
        | Diamonds
        | Spades

    /// A Discriminated Union can also be used to represent the rank of a playing card.
    type Rank =
        /// Represents the rank of cards 2 .. 10
        | Value of int
        | Ace
        | King
        | Queen
        | Jack

        /// Discriminated Unions can also implement object-oriented members.
        static member GetAllRanks() =
            [ yield Ace
              for i in 2 .. 10 do yield Value i
              yield Jack
              yield Queen
              yield King ]

    /// This is a record type that combines a Suit and a Rank.
    /// It's common to use both Records and Discriminated Unions when representing data.
    type Card = { Suit: Suit; Rank: Rank }

    /// This computes a list representing all the cards in the deck.
    let fullDeck =
        [ for suit in [ Hearts; Diamonds; Clubs; Spades] do
              for rank in Rank.GetAllRanks() do
                  yield { Suit=suit; Rank=rank } ]

    /// This example converts a 'Card' object to a string.
    let showPlayingCard (c: Card) =
        let rankString =
            match c.Rank with
            | Ace -> "Ace"
            | King -> "King"
            | Queen -> "Queen"
            | Jack -> "Jack"
            | Value n -> string n
        let suitString =
            match c.Suit with
            | Clubs -> "clubs"
            | Diamonds -> "diamonds"
            | Spades -> "spades"
            | Hearts -> "hearts"
        rankString  + " of " + suitString

    /// This example prints all the cards in a playing deck.
    let printAllCards() =
        for card in fullDeck do
            printfn $"{showPlayingCard card}"




[<EntryPoint>]
let main argv =
    printfn "%d squared is: %d!" 12 (square 12)
    let tuple = (1, 2)
    printfn $"The result of swapping {tuple} is {(swapElems tuple)}"
    printfn $"processing {numbers} through 'squareOddValuesAndAddOne' produces: {squareOddValuesAndAddOne numbers}"
    DiscriminatedUnions.printAllCards()
    printfn $"{Lang.eval (Lang.Example.incrApp 42)}"
    0 // Return an integer exit code