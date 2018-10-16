[%bs.raw {|require('./app.css')|}];

let component = ReasonReact.statelessComponent("App");

let make = _children => {
  ...component,
  render: _self =>
    <div className="App">
      <AudioContext
        render=(
          ((audioCtx, audioGraph)) =>
            <div>

                <Raveform audioCtx audioGraph width=512 height=512 />
                <StatefulOscillator nodeKey="osc1" audioCtx audioGraph />
              </div>
              /* <StatefulOscillator nodeKey="osc2" audioCtx audioGraph /> */
            /* <Wavetable audioCtx audioGraph /> */
        )
      />
    </div>,
};
