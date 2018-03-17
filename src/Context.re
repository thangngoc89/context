module type Config = {
  type state;
  type action;
  let reducer: (action, state) => state;
  let name: string;
  let defaultValue: state;
};

/* This allows for children pass through without coupling to the DOM with ReasonReact.createDomElement */
module RenderChildren = {
  let passThrough: ReasonReact.reactClass = [%bs.raw
    {| props => props.children |}
  ];
  let make = children =>
    ReasonReact.wrapJsForReason(
      ~reactClass=passThrough,
      ~props=Js.Obj.empty(),
      children,
    );
};

module CreateContext = (C: Config) => {
  type componentAction =
    | ChangeState(C.state)
    | SendAction(C.action);
  let componentState: ref(C.state) = ref(C.defaultValue);
  let subscriptions = ref([]);
  let addSubscription = f => {
    subscriptions := [f, ...subscriptions^];
    () => subscriptions := List.filter(sub => sub !== f, subscriptions^);
  };
  let updateState: option(C.state) => unit =
    newStateOpt => {
      let newState =
        switch (newStateOpt) {
        | None => C.defaultValue
        | Some(newValue) => newValue
        };
      componentState := newState;
      List.iter(f => f(newState), subscriptions^);
    };
  module Consumer = {
    let component = ReasonReact.reducerComponent(C.name ++ "ContextConsumer");
    let make = children => {
      ...component,
      initialState: () => componentState^,
      reducer: (componentAction, componentState) =>
        switch (componentAction) {
        | ChangeState(newState) => ReasonReact.Update(newState)
        | SendAction(configAction) =>
          ReasonReact.SideEffects(
            (
              _self =>
                updateState(Some(C.reducer(configAction, componentState)))
            ),
          )
        },
      subscriptions: ({send}) => [
        Sub(
          () => addSubscription(newState => send(ChangeState(newState))),
          unSub => unSub(),
        ),
      ],
      render: ({state, send}) => {
        let dispatch = configAction => send(SendAction(configAction));
        children(~dispatch, state);
      },
    };
  };
};
