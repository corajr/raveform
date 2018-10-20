open Audio;
open AudioGraph;

type state = {
  oscillatorRef: ref(option(oscillator)),
  gainRef: ref(option(gainNode)),
};

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~audioCtx,
      ~audioGraph,
      ~nodeKey,
      ~frequency=440.0,
      ~gain=0.5,
      ~type_=Sine,
      ~output="defaultAnalyser",
      _children,
    ) => {
  ...component,
  initialState: () => {oscillatorRef: ref(None), gainRef: ref(None)},
  didMount: self => {
    let gainNode = createGain(audioCtx);
    setValue(gain_Get(gainNode), gain);
    self.state.gainRef := Some(gainNode);

    let osc = makeOscillator(~audioCtx, ~frequency, ~type_);
    self.state.oscillatorRef := Some(osc);
    connect(osc, gainNode);
    startOscillator(osc);

    audioGraph :=
      audioGraph^
      |> addNode((nodeKey, unwrapGain(gainNode)))
      |> addEdge((nodeKey, output, 0, 0))
      |> updateConnections;
    self.onUnmount(() =>
      switch (self.state.oscillatorRef^) {
      | Some(osc) => stopOscillator(osc)
      | None => ()
      }
    );
  },
  reducer: ((), _state) => ReasonReact.NoUpdate,
  willUpdate: ({newSelf}) =>
    switch (newSelf.state.oscillatorRef^, newSelf.state.gainRef^) {
    | (Some(osc), Some(gainNode)) =>
      let t = currentTimeGet(audioCtx);
      oscillatorTypeSet(osc, string_of_oscillatorType(type_));
      setValueAtTime(gain_Get(gainNode), gain, t);
      setValueAtTime(oscillatorFrequencyGet(osc), frequency, t);
    | _ => ()
    },
  render: self =>
    <div> (ReasonReact.string(string_of_float(frequency) ++ " Hz")) </div>,
};
