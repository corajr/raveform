type state = {
  canvasRef: ref(option(Dom.element)),
  animCallbackRef: ref(option((Dom.element, float) => unit)),
  rafRef: ref(option(Webapi.rafId)),
};

let setCanvasRef = (theRef, {ReasonReact.state}) =>
  state.canvasRef := Js.Nullable.toOption(theRef);

let component = ReasonReact.reducerComponent(__MODULE__);

let make = (~animCallbackRef, ~width, ~height, _children) => {
  ...component,
  initialState: () => {
    canvasRef: ref(None),
    rafRef: ref(None),
    animCallbackRef,
  },
  didMount: self => {
    let rec anim = t => {
      switch (self.state.canvasRef^, self.state.animCallbackRef^) {
      | (Some(canvas), Some(animCallback)) => animCallback(canvas, t)
      | _ => ()
      };
      self.state.rafRef :=
        Some(Webapi.requestCancellableAnimationFrame(anim));
    };
    self.onUnmount(() =>
      switch (self.state.rafRef^) {
      | Some(rafId) => Webapi.cancelAnimationFrame(rafId)
      | None => ()
      }
    );

    anim(0.0);
  },
  reducer: ((), _state) => ReasonReact.NoUpdate,
  render: self =>
    <canvas
      ref=(self.handle(setCanvasRef))
      width=(Js.Int.toString(width))
      height=(Js.Int.toString(height))
    />,
};
