open Revery;
open Revery.UI;
open Revery.UI.Components;

module Card = {
  let make = (~picture: string, ~show: bool, ~onClick, ()) => {
    let wrapperStyle =
      Style.[
        backgroundColor(Color.rgba(1., 1., 1., 0.1)),
        border(~width=2, ~color=Colors.white),
        margin(5),
        width(50),
        height(50),
        flexGrow(1),
        alignItems(`Center),
        justifyContent(`Center),
      ];

    let textHeaderStyle =
      Style.[
        color(Colors.white),
        fontFamily("Roboto-Regular.ttf"),
        fontSize(40),
      ];

    let text = if (show) {picture} else {""};

    <Clickable onClick>
      <View style=wrapperStyle> <Text style=textHeaderStyle text /> </View>
    </Clickable>;
  };
};

type action =
  | TurnCard(int, int);

let reducer = (action, state) =>
  switch (action) {
  | TurnCard(x, y) => state
  };

let cards = [
  ({Board.x: 0, y: 0}, "A"),
  ({x: 1, y: 0}, "A"),
  ({x: 0, y: 1}, "B"),
  ({x: 1, y: 1}, "B"),
];
let board = Board.create(cards);

module Memory = {
  let%component make = () => {
    let%hook (state, dispatch) = Hooks.reducer(~initialState=board, reducer);

    let containerStyle =
      Style.[
        position(`Absolute),
        justifyContent(`Center),
        alignItems(`Center),
        bottom(0),
        top(0),
        left(0),
        right(0),
      ];

    let innerStyle = Style.[flexDirection(`Row), alignItems(`FlexEnd)];

    <View style=containerStyle>
      <View style=innerStyle>
        <Card
          picture="x"
          show=true
          onClick={_ => dispatch(TurnCard(0, 0))}
        />
        <Card
          picture="x"
          show=false
          onClick={_ => dispatch(TurnCard(0, 0))}
        />
        <Card
          picture="x"
          show=false
          onClick={_ => dispatch(TurnCard(0, 0))}
        />
      </View>
    </View>;
  };
};

let init = app => {
  let _ = Revery.Log.listen((_, msg) => print_endline("LOG: " ++ msg));

  let win = App.createWindow(app, "Same picture game");

  let element = <Memory />;

  let _ = UI.start(win, element);
  ();
};

App.start(init);
