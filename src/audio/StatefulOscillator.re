open Audio;

module OscillatorTypeSelect =
  AdtSelect.Make({
    type t = oscillatorType;
    let default = Sine;
    let options = [|Sine, Triangle, Sawtooth, Square|];
    let toString = string_of_oscillatorType;
    let fromString = oscillatorType_of_string;
  });

let shiftedMidiCps = (sampleRate, i) =>
  sampleRate /. 512.0 *. 2.0 ** ((float_of_int(i) -. 41.0) /. 12.0);

let freqToMidiNote = (_sampleRate, v) => {
  let note = 12.0 *. (log(v /. 440.0) /. log(2.0)) +. 69.0;
  if (note -. floor(note) <= 0.5) {
    int_of_float(note);
  } else {
    int_of_float(note) + 1;
  };
};

/* let midiNoteToFreq = (_sampleRate, i) => noteToFrequency(float_of_int(i)); */
let midiNoteToFreq = shiftedMidiCps;

type waveProperty =
  | LinearFrequency
  | LinearPeriod;

module FrequencyOrPeriodSelect =
  AdtSelect.Make({
    type t = waveProperty;
    let default = LinearPeriod;
    let options = [|LinearFrequency, LinearPeriod|];

    let toString =
      fun
      | LinearFrequency => "frequency"
      | LinearPeriod => "period";

    let fromString =
      fun
      | "frequency" => LinearFrequency
      | "period" => LinearPeriod
      | _ => default;
  });

type state = {
  oscillatorType,
  selectorType: FrequencyOrPeriodSelect.t,
  frequency: float,
  gain: float,
};

type action =
  | SetOscillatorType(oscillatorType)
  | SetSelectorType(FrequencyOrPeriodSelect.t)
  | SetFrequency(float)
  | SetPeriod(float)
  | SetMidiNote(int)
  | SetGain(float);

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (~nodeKey, ~audioCtx, ~audioGraph, ~output="defaultAnalyser", _children) => {
  ...component,
  initialState: () => {
    oscillatorType: OscillatorTypeSelect.default,
    selectorType: FrequencyOrPeriodSelect.default,
    frequency: sampleRateGet(audioCtx) /. 512.0,
    gain: 0.25,
  },
  reducer: (action, state) =>
    switch (action) {
    | SetOscillatorType(oscillatorType) =>
      ReasonReact.Update({...state, oscillatorType})
    | SetFrequency(frequency) => ReasonReact.Update({...state, frequency})
    | SetSelectorType(selectorType) =>
      ReasonReact.Update({...state, selectorType})
    | SetPeriod(period) =>
      ReasonReact.Update({
        ...state,
        frequency: sampleRateGet(audioCtx) /. period,
      })
    | SetMidiNote(i) =>
      ReasonReact.Update({
        ...state,
        frequency: midiNoteToFreq(sampleRateGet(audioCtx), i),
      })

    | SetGain(gain) => ReasonReact.Update({...state, gain})
    },
  render: self =>
    <div>
      <h2> (ReasonReact.string("Oscillator: " ++ nodeKey)) </h2>
      <fieldset>
        <OscillatorTypeSelect
          currentType=self.state.oscillatorType
          onChange=(
            self.handle((v, {ReasonReact.send}) =>
              send(SetOscillatorType(v))
            )
          )
        />
      </fieldset>
      <fieldset>
        <FrequencyOrPeriodSelect
          currentType=self.state.selectorType
          onChange=(
            self.handle((v, {ReasonReact.send}) =>
              send(SetSelectorType(v))
            )
          )
        />
        (
          switch (self.state.selectorType) {
          | LinearFrequency =>
            <input
              type_="number"
              step=1.0
              value=(Js.Float.toString(self.state.frequency))
              onChange=(
                self.handle((evt, {ReasonReact.send}) => {
                  let s = ReactEvent.Form.target(evt)##value;
                  let v = float_of_string(s);
                  if (v > 0.0) {
                    send(SetFrequency(v));
                  };
                })
              )
            />

          | LinearPeriod =>
            <input
              type_="number"
              step=1.0
              value=(
                Js.Float.toString(
                  sampleRateGet(audioCtx) /. self.state.frequency,
                )
              )
              onChange=(
                self.handle((evt, {ReasonReact.send}) => {
                  let s = ReactEvent.Form.target(evt)##value;
                  let v = float_of_string(s);
                  if (v > 0.0) {
                    send(SetPeriod(v));
                  };
                })
              )
            />
          /* | MidiNote => */
          /* <input */
          /*   type_="number" */
          /*   step=1.0 */
          /*   value=( */
          /*     Js.Int.toString( */
          /*     freqToMidiNote( */
          /*       sampleRateGet(audioCtx), */
          /*       self.state.frequency, */
          /*     ), */
          /*   ) */
          /*   ) */
          /*   onChange=( */
          /*     self.handle((evt, {ReasonReact.send}) => { */
          /*     let s = ReactEvent.Form.target(evt)##value; */
          /*     let i = int_of_string(s); */
          /*     send(SetMidiNote(i)); */
          /*   }) */
          /*   ) */
          /*   /> */
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
      </fieldset>
      <Oscillator
        nodeKey
        audioCtx
        audioGraph
        output
        frequency=self.state.frequency
        gain=self.state.gain
        type_=self.state.oscillatorType
      />
    </div>,
};
