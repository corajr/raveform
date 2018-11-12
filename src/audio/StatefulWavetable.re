open Wavetable;

type wavetablePreset =
  | Sine512
  | Saw512
  | Custom;

module WavetableSelect =
  AdtSelect.Make({
    type t = wavetablePreset;

    let default = Sine512;
    let options = [|Sine512, Saw512, Custom|];

    let toString =
      fun
      | Sine512 => "sine-512"
      | Saw512 => "saw-512"
      | Custom => "custom";

    let fromString =
      fun
      | "custom" => Custom
      | "saw-512" => Saw512
      | "sine-512"
      | _ => Sine512;
  });

let toWavetable: wavetablePreset => wavetable =
  fun
  | Sine512 => sineSamples
  | Saw512 => sawSamples
  | Custom => sineSamples;

type state = {
  wavetable,
  wavetablePreset,
  playbackRate: float,
  gain: float,
};

type action =
  | SetWavetablePreset(wavetablePreset)
  | SetWavetable(array(float))
  | SetPlaybackRate(float)
  | SetGain(float);

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~audioCtx,
      ~audioGraph,
      ~key="defaultWavetable",
      ~output="defaultAnalyser",
      _children,
    ) => {
  ...component,
  initialState: () => {
    wavetablePreset: Sine512,
    wavetable: sineSamples,
    playbackRate: 1.0,
    gain: 0.25,
  },
  reducer: (action, state) =>
    switch (action) {
    | SetWavetablePreset(wavetablePreset) =>
      ReasonReact.Update({
        ...state,
        wavetable: toWavetable(wavetablePreset),
        wavetablePreset,
      })
    | SetWavetable(wavetable) => ReasonReact.Update({...state, wavetable})
    | SetPlaybackRate(playbackRate) =>
      ReasonReact.Update({...state, playbackRate})
    | SetGain(gain) => ReasonReact.Update({...state, gain})
    },
  render: self =>
    <div>
      <h2> (ReasonReact.string("Wavetable: " ++ key)) </h2>
      <fieldset>
        <WavetableSelect
          currentType=self.state.wavetablePreset
          onChange=(v => self.send(SetWavetablePreset(v)))
        />
        (
          switch (self.state.wavetablePreset) {
          | Custom =>
            <div>
              <input
                type_="text"
                value=(
                  Js.Json.stringify(
                    Js.Json.numberArray(self.state.wavetable),
                  )
                )
                onChange=(
                  evt => {
                    let s = ReactEvent.Form.target(evt)##value;
                    switch (Js.Json.parseExn(s)) {
                    | json =>
                      let a =
                        Belt.Option.getWithDefault(
                          Js.Json.decodeArray(json),
                          [||],
                        );
                      self.send(
                        SetWavetable(
                          Array.map(
                            x =>
                              Belt.Option.getWithDefault(
                                Js.Json.decodeNumber(x),
                                0.0,
                              ),
                            a,
                          ),
                        ),
                      );
                    | exception e => Js.log(e)
                    };
                  }
                )
              />
            </div>
          | _ => ReasonReact.null
          }
        )
      </fieldset>
      <fieldset>
        <label> (ReasonReact.string("gain")) </label>
        <input
          type_="number"
          step=0.01
          value=(Js.Float.toString(self.state.gain))
          onChange=(
            self.handle((evt, {ReasonReact.send}) => {
              let s = ReactEvent.Form.target(evt)##value;
              let v = float_of_string(s);
              send(SetGain(v));
            })
          )
        />
        <label> (ReasonReact.string("playback rate")) </label>
        <input
          type_="number"
          step=0.01
          value=(Js.Float.toString(self.state.playbackRate))
          onChange=(
            self.handle((evt, {ReasonReact.send}) => {
              let s = ReactEvent.Form.target(evt)##value;
              let v = float_of_string(s);
              send(SetPlaybackRate(v));
            })
          )
        />
      </fieldset>
      <Wavetable
        audioCtx
        audioGraph
        key
        output
        playbackRate=self.state.playbackRate
        wavetable=self.state.wavetable
        gain=self.state.gain
      />
    </div>,
};
