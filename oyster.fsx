module TestFramework =
  let test desc x =
    if x then printfn "%s passed" desc
    else printfn "%s failed" desc

  let testf desc x y =
    if x = y then printfn "%s passed" desc
    else printfn "%s failed value was %M" desc x


module Oyster =
  type Zone =
    | ZoneOne
    | ZoneTwo
    | ZoneThree

  type Station =
    | Holborn
    | EarlesCourt
    | Wimbledon
    | Hammersmith
    | Chelsea

  type Event =
    | EnterTube of Station
    | LeaveTube of Station
    | BusJourney

  type Trip =
    | Trip of Event List

  type Transaction =
    | TopUp of decimal
    | DefaultCharge
    | Charge of decimal
    | Refund of decimal

  type Card =
    | Card of decimal


  let zones = function
    | Holborn -> [ZoneOne]
    | EarlesCourt -> [ZoneOne;ZoneTwo]
    | Wimbledon -> [ZoneThree]
    | Hammersmith -> [ZoneTwo]
    | _ -> failwith "don't know the zone for this"

  let allzones (src,dest) =
    zones src @ zones dest |> List.distinct

  let tubecost zones =
    match zones with
      | [_;_;_] -> 3.2m
      | [ZoneOne;_] | [_;ZoneOne] -> 3m
      | [_;_] -> 2.25m
      | [ZoneOne] -> 2.5m
      | [_] -> 2m
      | _ -> failwithf "missed one %A" zones

  let fold events =
    let first = function
      | BusJourney -> Charge 1.8m
      | _ -> DefaultCharge

    let ptn prev current =
      match prev, current with
        | EnterTube(src),LeaveTube(dest) ->
          [Refund 30m
           allzones (src,dest) |> tubecost |> Charge] //only time when refund and charge
        | _,LeaveTube(_) -> [DefaultCharge]           //leaving without entering causes default charge
        | _,EnterTube(_) -> [DefaultCharge]           //Entering always causes default charge
        | _,BusJourney -> [Charge 1.8m]


    let rec pattern prev events =
      match prev, events with
        | prev,current::tail ->
          (ptn prev current) @ (pattern current tail)
        | _,[] -> []

    match events with
      | [head] -> [head |> first]
      | head::tail -> (head |> first) :: (pattern head tail)
      | [] -> failwith "no events"


  let sum transactions =
    transactions
    |> List.map (fun t -> match t with
                          | TopUp(m) -> m
                          | DefaultCharge -> -30m
                          | Charge(m) -> -m
                          | Refund(m) -> m)
    |> List.sum


  let apply txn (Card(balance)) =
    balance + (txn |> sum) |> Card

  let charge card (Trip(events)) =
    card |> apply (events |> fold)

open TestFramework
open Oyster

test "Any three zones £3.20" ([ZoneOne;ZoneTwo;ZoneThree] |> tubecost = 3.2m)
test "Any two zones excluding zone one £2.25" ([ZoneTwo;ZoneThree] |> tubecost = 2.25m)
test "Any two zones including zone one £3.00" ([ZoneOne;ZoneTwo] |> tubecost = 3m)
test "Any one zone outside zone one" ([ZoneTwo] |> tubecost = 2m)
test "Anywhere in Zone One" ([ZoneOne] |> tubecost = 2.5m)

let card = Card(0m) |> apply [TopUp(30m)]

test "bus journey" (Trip [BusJourney] |> charge card = Card(28.2m))
test "enter no leave" (Trip [(EnterTube(Holborn))] |> charge card = Card(0m))
test "leave no enter" (Trip [(LeaveTube(Holborn))] |> charge card = Card(0m))


//Tube Holborn to Earles Court £3.00
//328 Bus from Earles Court to Chelsea £1.80
//Tube Earles Court To Hammersmith £3.00

//total 7.8
//balance 22.2

let trip = Trip [EnterTube Holborn
                 LeaveTube EarlesCourt
                 BusJourney
                 EnterTube EarlesCourt
                 LeaveTube Hammersmith]


test "full trip" (trip |> charge card = Card(22.2m))
