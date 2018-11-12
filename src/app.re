[%bs.raw {|require('./app.css')|}];

let component = ReasonReact.statelessComponent("App");

let make = _children => {
  ...component,
  render: _self =>
    <div className="App">
      <AudioContext
        render=(
          ((audioCtx, audioGraph)) =>
            <div
              style=(
                ReactDOMRe.Style.make(
                  ~display="flex",
                  ~flexDirection="row",
                  (),
                )
              )>
              <Raveform audioCtx audioGraph width=512 height=512 />
              <div style=(ReactDOMRe.Style.make(~marginLeft="24px", ()))>
                <StatefulOscillator nodeKey="osc1" audioCtx audioGraph />
                <StatefulWavetable audioCtx audioGraph />
              </div>
            </div>
        )
      />
    </div>,
};
