open Audio;
open AudioGraph;

type state = {
  audioCtx: option(audioContext),
  audioGraph: ref(audioGraph),
};

type action =
  | SetAudioContext(audioContext)
  | ResumeAudio
  | SuspendAudio;

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~render, _children) => {
  ...component,
  initialState: () => {audioCtx: None, audioGraph: ref(emptyAudioGraph)},
  didMount: self => self.send(SetAudioContext(makeDefaultAudioCtx())),
  reducer: (action, state) =>
    switch (action) {
    | SetAudioContext(ctx) =>
      ReasonReact.UpdateWithSideEffects(
        {...state, audioCtx: Some(ctx)},
        (
          self =>
            self.state.audioGraph :=
              self.state.audioGraph^
              |> addNode(("sink", defaultSink(ctx)))
              |> updateConnections
        ),
      )
    | ResumeAudio =>
      ReasonReact.SideEffects(
        (
          self =>
            switch (self.state.audioCtx) {
            | Some(audioCtx) => resume(audioCtx)
            | None => ()
            }
        ),
      )
    | SuspendAudio =>
      ReasonReact.SideEffects(
        (
          self =>
            switch (self.state.audioCtx) {
            | Some(audioCtx) => suspend(audioCtx)
            | None => ()
            }
        ),
      )
    },
  render: self =>
    switch (self.state.audioCtx) {
    | Some(ctx) => render((ctx, self.state.audioGraph))
    | None => ReasonReact.null
    },
};
