type panelKind =
  | PatientAdd
  | PatientUpdate(string);

module Context =
  Context.CreateContext(
    {
      type state =
        | PanelState_Open(panelKind)
        | PanelState_Close;
      type action =
        | Action_Open(panelKind)
        | Action_Close;
      let reducer = (action, state) =>
        switch (action) {
        | Action_Open(kind) => PanelState_Open(kind)
        | Action_Close => PanelState_Close
        };
      let name = "Panel";
      let defaultValue = PanelState_Close;
    },
  );
