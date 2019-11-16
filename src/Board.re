[@deriving (eq, show)]
type location = {
  x: int,
  y: int,
};
[@deriving (eq, show)]
type card = (location, string);

[@deriving show]
type t = {
  closedCards: list(card),
  foundCards: list(card),
  openCardLocation: option(location),
};

[@deriving eq]
type boardEvents =
  | CardTurned(card)
  | PairFound(card, card)
  | FinalPairFound(card, card)
  | IllegalMove(location);
let create = closedCards => {
  closedCards,
  foundCards: [],
  openCardLocation: None,
};
let findClosedCard = ({closedCards, _}, location) =>
  List.find_exn(closedCards, ~f=((cardLocation, _)) =>
    equal_location(location, cardLocation)
  );
let turnCard = ({closedCards, openCardLocation, _} as board, ~location) => {
  let (_closedCards, cardsToTurn) =
    List.partition_tf(closedCards, ~f=((cardLocation, _)) =>
      !equal_location(location, cardLocation)
    );
  switch (cardsToTurn, openCardLocation) {
  | ([(cardLocation, _value)], None) =>
    let board = {...board, openCardLocation: Some(cardLocation)};
    let card = findClosedCard(board, location);
    (board, CardTurned(card));
  | ([(cardLocation, _value)], Some(turnLocation))
      when equal_location(cardLocation, turnLocation) => (
      board,
      IllegalMove(turnLocation),
    )
  | ([(cardLocation, _value)], Some(turnLocation)) =>
    let (_, val1) as card = findClosedCard(board, cardLocation);
    let (_, val2) as openCard = findClosedCard(board, turnLocation);
    if (String.equal(val1, val2)) {
      let closedCards =
        List.filter(board.closedCards, ~f=((closedCardLocation, _)) =>
          !equal_location(closedCardLocation, cardLocation)
          && !equal_location(closedCardLocation, turnLocation)
        );
      let board = {...board, openCardLocation: None, closedCards};
      switch (closedCards) {
      | [] => (board, FinalPairFound(openCard, card))
      | _ => (board, PairFound(openCard, card))
      };
    } else {
      let board = {...board, openCardLocation: None};
      (board, CardTurned(card));
    };
  | ([], None) => (board, IllegalMove(location))
  | _ => assert(false)
  };
};
