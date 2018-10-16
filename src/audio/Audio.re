open UserMedia;

type audioTime = float; /* time in milliseconds */

[@bs.deriving abstract]
type audioContext = {
  currentTime: audioTime,
  sampleRate: float,
};

let makeDefaultAudioCtx: unit => audioContext = [%bs.raw
  () => {|return new (window.AudioContext || window.webkitAudioContext)()|}
];

type audioParam;

type frequency = float;
type qFactor = float;
type filterGain = float;
type freqFunc = int => frequency;

type float32array = array(float);

type filterTypeField;

type filterType =
  | LowPass(frequency, qFactor)
  | HighPass(frequency, qFactor)
  | BandPass(frequency, qFactor)
  | LowShelf(frequency, filterGain)
  | HighShelf(frequency, filterGain)
  | Peaking(frequency, qFactor, filterGain)
  | Notch(frequency, qFactor)
  | AllPass(frequency, qFactor);

[@bs.deriving abstract]
type biquadFilter =
  pri {
    [@bs.as "type"]
    mutable type_: string,
    frequency: audioParam,
    detune: audioParam,
    gain: audioParam,
    [@bs.as "Q"]
    qualityFactor: audioParam,
  };

type periodicWave;

type periodicWaveDescription = {
  real: array(float),
  imag: array(float),
};

type oscillatorType =
  | Sine
  | Square
  | Sawtooth
  | Triangle
  | Custom(periodicWaveDescription);

let string_of_oscillatorType =
  fun
  | Sine => "sine"
  | Square => "square"
  | Sawtooth => "sawtooth"
  | Triangle => "triangle"
  | Custom(_) => "custom";

let oscillatorType_of_string =
  fun
  | "sine" => Sine
  | "square" => Square
  | "sawtooth" => Sawtooth
  | "triangle" => Triangle
  | _ => Sine;

[@bs.deriving abstract]
type oscillator =
  pri {
    [@bs.as "type"]
    mutable oscillatorType: string,
    [@bs.as "frequency"]
    oscillatorFrequency: audioParam,
    [@bs.as "detune"]
    oscillatorDetune: audioParam,
  };

[@bs.deriving abstract]
type gainNode =
  pri {
    [@bs.as "gain"]
    gain_: audioParam,
  };

[@bs.deriving abstract]
type analyser =
  pri {
    mutable fftSize: int,
    frequencyBinCount: int,
    mutable minDecibels: float,
    mutable maxDecibels: float,
    mutable smoothingTimeConstant: float,
  };

[@bs.deriving abstract]
type audioBuffer =
  pri {
    [@bs.as "sampleRate"]
    bufferSampleRate: float,
    [@bs.as "length"]
    bufferLength: int,
    duration: float,
    numberOfChannels: int,
  };

[@bs.deriving abstract]
type audioBufferSourceNode =
  pri {
    mutable buffer: audioBuffer,
    [@bs.as "detune"]
    bufferDetune: audioParam,
    mutable loop: bool,
    mutable loopStart: float,
    mutable loopEnd: float,
    playbackRate: audioParam,
  };

[@bs.deriving abstract]
type compressor = {
  threshold: audioParam,
  knee: audioParam,
  ratio: audioParam,
  attack: audioParam,
  release: audioParam,
};

type channelMerger;
type channelSplitter;
type stereoPanner;

type compressorParamValues = {
  threshold: float,
  knee: float,
  ratio: float,
  attack: float,
  release: float,
};

type audioNode =
  | BiquadFilter(biquadFilter)
  | Gain(gainNode)
  | Compressor(compressor)
  | Oscillator(oscillator)
  | Analyser(analyser)
  | ChannelSplitter(channelSplitter)
  | StereoPanner(stereoPanner)
  | BufferSource(audioBufferSourceNode)
  | Node;

external unwrapAnalyser : analyser => audioNode = "%identity";
external unwrapBufferSourceNode : audioBufferSourceNode => audioNode =
  "%identity";
external unwrapFilter : biquadFilter => audioNode = "%identity";
external unwrapGain : gainNode => audioNode = "%identity";
external unwrapOscillator : oscillator => audioNode = "%identity";
external unwrapCompressor : compressor => audioNode = "%identity";
external unwrapChannelMerger : channelMerger => audioNode = "%identity";
external unwrapStereoPanner : stereoPanner => audioNode = "%identity";

type bank('a) = {
  input: option(gainNode),
  nodes: array('a),
  gains: array(gainNode),
  output: gainNode,
  audioCtx: audioContext,
};

type filterBank = bank(biquadFilter);

/**
   https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode/getFrequencyResponse
   getFrequencyResponse(frequencyArray, magResponseOutput, phaseResponseOutput);

   magResponseOutput and phaseResponseOutput must be pre-initialized to have the
   same length as frequencyArray. */
[@bs.send]
external getFrequencyResponse :
  (biquadFilter, float32array, float32array, float32array) => audioParam =
  "";

[@bs.get] external getAudioContext : audioNode => audioContext = "context";

[@bs.send] external suspend : audioContext => unit = "";
[@bs.send] external resume : audioContext => unit = "";

[@bs.send] external createAnalyser : audioContext => analyser = "";
[@bs.send] external createBiquadFilter : audioContext => biquadFilter = "";

/* ctx, numOfChannels, length, sampleRate */
[@bs.send]
external createBuffer : (audioContext, int, int, int) => audioBuffer = "";

[@bs.send]
external createBufferSource : audioContext => audioBufferSourceNode = "";

[@bs.send]
external getChannelData : (audioBuffer, int) => Js.Typed_array.Float32Array.t =
  "";

[@bs.send]
external copyToChannel :
  (audioBuffer, Js.Typed_array.Float32Array.t, int, int) => unit =
  "";

[@bs.send] external createGain : audioContext => gainNode = "";
[@bs.send] external createDynamicsCompressor : audioContext => compressor = "";

[@bs.send] external createStereoPanner : audioContext => stereoPanner = "";
[@bs.send]
external createChannelSplitter : audioContext => channelSplitter = "";

[@bs.send] external createChannelMerger : audioContext => channelMerger = "";
[@bs.send]
external createMediaStreamSource : (audioContext, mediaStream) => audioNode =
  "";

[@bs.send]
external createMediaElementSource : (audioContext, Dom.element) => audioNode =
  "";

[@bs.send] external createOscillator : audioContext => oscillator = "";

[@bs.send]
external createPeriodicWave :
  (audioContext, array(float), array(float)) => periodicWave =
  "";

[@bs.send] external setPeriodicWave : (oscillator, periodicWave) => unit = "";
[@bs.send] external startOscillator : oscillator => unit = "start";
[@bs.send] external stopOscillator : oscillator => unit = "stop";

/* https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode/start */
[@bs.send]
external startAudioBufferSourceNode : audioBufferSourceNode => unit = "start";
[@bs.send]
external stopAudioBufferSourceNode : audioBufferSourceNode => unit = "stop";

[@bs.set] external setValue : (audioParam, float) => unit = "value";

[@bs.send] external setValueAtTime : (audioParam, float, float) => unit = "";

[@bs.send]
external linearRampToValueAtTime : (audioParam, float, float) => unit = "";

[@bs.send]
external connectFilterToGain : (biquadFilter, gainNode) => unit = "connect";
[@bs.send]
external connectGainToFilter : (gainNode, biquadFilter) => unit = "connect";
[@bs.send]
external connectFilterToFilter : (biquadFilter, biquadFilter) => unit =
  "connect";
[@bs.send]
external connectGainToGain : (gainNode, gainNode) => unit = "connect";
[@bs.send]
external connectGainToCompressor : (gainNode, compressor) => unit = "connect";
[@bs.send]
external connectNodeToGain : (audioNode, gainNode) => unit = "connect";
[@bs.send]
external connectNodeToAnalyser : (audioNode, analyser) => unit = "connect";
[@bs.send]
external connectGainToNode : (gainNode, audioNode) => unit = "connect";
[@bs.send]
external connectCompressorToNode : (compressor, audioNode) => unit = "connect";

[@bs.send]
external connectNodeToStereoPanner : (audioNode, stereoPanner) => unit =
  "connect";

/* TODO: figure out how to type these better. */
[@bs.send] external connect : ('a, 'b) => unit = "connect";
[@bs.send] external connectWithOutputIndex : ('a, 'b, int) => unit = "connect";
[@bs.send]
external connectWithOutputAndInputIndex : ('a, 'b, int, int) => unit =
  "connect";
[@bs.send] external disconnect : ('a, 'b) => unit = "disconnect";

[@bs.send]
external disconnectWithOutputAndInputIndex : ('a, 'b, int, int) => unit =
  "disconnect";

let midiNoteA440Hz = 69.0;

let noteToFrequency: float => float =
  note => 440.0 *. Js.Math.pow_float(2.0, (note -. midiNoteA440Hz) /. 12.0);

let yToFrequency: (int, int, int, int) => float =
  (binsPerSemitone, offset, height) => {
    let fBinsPerSemitone = float_of_int(binsPerSemitone);
    let offset = float_of_int(offset);

    y => {
      let note = float_of_int(height - y) /. fBinsPerSemitone +. offset;
      noteToFrequency(note);
    };
  };

let qForBinsPerOctave: int => float =
  binsPerOctave =>
    1.0
    /. (Js.Math.pow_float(2.0, 1.0 /. float_of_int(binsPerOctave)) -. 1.0);

/* In principle, Q should be 1 / (2^(1/12) - 1) = 16.817 */
/* but a higher Q sounds better. */

let defaultQ = qForBinsPerOctave(48);

let defaultCompressorValues: compressorParamValues = {
  threshold: (-12.0),
  knee: 0.0,
  ratio: 20.0,
  attack: 0.01,
  release: 0.05,
};

let makeCompressor =
    (
      ~audioCtx: audioContext,
      ~paramValues: compressorParamValues=defaultCompressorValues,
    )
    : compressor => {
  let compressor = createDynamicsCompressor(audioCtx);
  let t = currentTime(audioCtx);
  setValueAtTime(compressor |. threshold, paramValues.threshold, t);
  setValueAtTime(compressor |. knee, paramValues.knee, t);
  setValueAtTime(compressor |. attack, paramValues.attack, t);
  setValueAtTime(compressor |. release, paramValues.release, t);
  compressor;
};

/* http://www.musicdsp.org/files/pink.txt */
let pinkNoiseFull: audioContext => audioNode = [%bs.raw
  audioCtx => {|
     var bufferSize = 4096;
     return (function() {
     var b0, b1, b2, b3, b4, b5, b6;
     b0 = b1 = b2 = b3 = b4 = b5 = b6 = 0.0;
     var node = audioCtx.createScriptProcessor(bufferSize, 1, 1);
     node.onaudioprocess = function(e) {
     var output = e.outputBuffer.getChannelData(0);
     for (var i = 0; i < bufferSize; i++) {
     var white = Math.random() * 2 - 1;
     b0 = 0.99886 * b0 + white * 0.0555179;
     b1 = 0.99332 * b1 + white * 0.0750759;
     b2 = 0.96900 * b2 + white * 0.1538520;
     b3 = 0.86650 * b3 + white * 0.3104856;
     b4 = 0.55000 * b4 + white * 0.5329522;
     b5 = -0.7616 * b5 - white * 0.0168980;
     output[i] = b0 + b1 + b2 + b3 + b4 + b5 + b6 + white * 0.5362;
     output[i] *= 0.11; // (roughly) compensate for gain
     b6 = white * 0.115926;
     }
     }
     return node;
     })();
     |}
];

let cheaperPinkNoise: audioContext => audioNode = [%bs.raw
  audioCtx => {|
     var bufferSize = 4096;
     return (function() {
     var b0, b1, b2;
     b0 = b1 = b2 = 0.0;
     var node = audioCtx.createScriptProcessor(bufferSize, 1, 1);
     node.onaudioprocess = function(e) {
     var output = e.outputBuffer.getChannelData(0);
     for (var i = 0; i < bufferSize; i++) {
     var white = Math.random() * 2 - 1;
     b0 = 0.99765 * b0 + white * 0.0990460;
     b1 = 0.96300 * b1 + white * 0.2965164;
     b2 = 0.57000 * b2 + white * 1.0526913;
     output[i] = b0 + b1 + b2 + white * 0.1848;
     }
     }
     return node;
     })();
     |}
];

let pinkNoise = pinkNoiseFull;

let whiteNoise: audioContext => audioNode = [%bs.raw
  audioCtx => {|
     var bufferSize = 4096;
     return (function() {
     var node = audioCtx.createScriptProcessor(bufferSize, 1, 1);
     node.onaudioprocess = function(e) {
     var output = e.outputBuffer.getChannelData(0);
     for (var i = 0; i < bufferSize; i++) {
     var white = Math.random() * 2 - 1;
     output[i] = white;
     }
     }
     return node;
     })();
     |}
];

[@bs.send]
external getFloatFrequencyData :
  (analyser, Js.Typed_array.Float32Array.t) => unit =
  "";
[@bs.send]
external getByteFrequencyData : (analyser, Js.Typed_array.Uint8Array.t) => unit =
  "";

[@bs.send]
external getFloatTimeDomainData :
  (analyser, Js.Typed_array.Float32Array.t) => unit =
  "";

[@bs.send]
external getByteTimeDomainData :
  (analyser, Js.Typed_array.Uint8Array.t) => unit =
  "";

let makeAnalyser =
    (
      ~fftSize: int=2048,
      ~minDecibels: float=(-100.0),
      ~maxDecibels: float=(-30.0),
      ~smoothingTimeConstant: float=0.8,
      audioContext: audioContext,
    )
    : analyser => {
  let analyser = createAnalyser(audioContext);
  fftSizeSet(analyser, fftSize);
  minDecibelsSet(analyser, minDecibels);
  maxDecibelsSet(analyser, maxDecibels);
  smoothingTimeConstantSet(analyser, smoothingTimeConstant);
  analyser;
};

let setOscillatorType =
    (~audioCtx: audioContext, ~oscillator: oscillator, ~type_: oscillatorType) => {
  oscillatorTypeSet(oscillator, string_of_oscillatorType(type_));
  switch (type_) {
  | Custom({real, imag}) =>
    let periodicWave = createPeriodicWave(audioCtx, real, imag);
    setPeriodicWave(oscillator, periodicWave);
  | _ => ()
  };
};

let makeOscillator =
    (~frequency: float=440.0, ~type_=Sine, ~audioCtx: audioContext)
    : oscillator => {
  let oscillator = createOscillator(audioCtx);
  let t = currentTime(audioCtx);
  setValueAtTime(oscillator |. oscillatorFrequencyGet, frequency, t);
  setOscillatorType(audioCtx, oscillator, type_);
  oscillator;
};

let string_of_filterType = filterType =>
  switch (filterType) {
  | LowPass(_, _) => "lowpass"
  | HighPass(_, _) => "highpass"
  | BandPass(_, _) => "bandpass"
  | LowShelf(_, _) => "lowshelf"
  | HighShelf(_, _) => "highshelf"
  | Peaking(_, _, _) => "peaking"
  | Notch(_, _) => "notch"
  | AllPass(_, _) => "allpass"
  };

let makeFilter =
    (~audioCtx: audioContext, ~filterType: filterType)
    : biquadFilter => {
  let filter = createBiquadFilter(audioCtx);
  type_Set(filter, string_of_filterType(filterType));

  let t = currentTime(audioCtx);

  switch (filterType) {
  | BandPass(f, q)
  | LowPass(f, q)
  | HighPass(f, q)
  | BandPass(f, q)
  | Notch(f, q)
  | AllPass(f, q) =>
    setValueAtTime(frequency(filter), f, t);
    setValueAtTime(qualityFactor(filter), q, t);
  | LowShelf(f, g)
  | HighShelf(f, g) =>
    setValueAtTime(frequency(filter), f, t);
    setValueAtTime(gain(filter), g, t);
  | Peaking(f, q, g) =>
    setValueAtTime(frequency(filter), f, t);
    setValueAtTime(qualityFactor(filter), q, t);
    setValueAtTime(gain(filter), g, t);
  };
  filter;
};

let makeBankOf:
  (
    ~audioCtx: audioContext,
    ~n: int,
    ~hasInput: bool,
    ~f: (audioContext, int) => 'a
  ) =>
  bank('a) =
  (~audioCtx, ~n, ~hasInput, ~f) => {
    let input = hasInput ? Some(createGain(audioCtx)) : None;
    let output = createGain(audioCtx);
    let t = currentTime(audioCtx);

    let createNode =
      switch (input) {
      | Some(inputNode) => (
          i => {
            let node = f(audioCtx, i);
            connect(input, node);
            node;
          }
        )
      | None => f(audioCtx)
      };

    let nodes = Array.init(n, createNode);

    let gains =
      Array.map(
        node => {
          let gainNode = createGain(audioCtx);
          setValueAtTime(gainNode |. gain_Get, 0.0, t);
          connect(node, gainNode);
          connect(gainNode, output);
          gainNode;
        },
        nodes,
      );

    {nodes, gains, input, output, audioCtx};
  };

let makeFilterBank =
    (~audioCtx: audioContext, ~filterN: int, ~q: float, ~freqFunc: freqFunc)
    : filterBank => {
  let createNode = (audioCtx, i) =>
    makeFilter(audioCtx, BandPass(freqFunc(filterN - i), q));

  makeBankOf(~audioCtx, ~n=filterN, ~hasInput=true, ~f=createNode);
};

let makeOscillatorBank =
    (
      ~audioCtx: audioContext,
      ~n: int,
      ~type_: oscillatorType,
      ~freqFunc: freqFunc,
    )
    : bank(oscillator) => {
  let createNode = (audioCtx, i) =>
    makeOscillator(~audioCtx, ~type_, ~frequency=freqFunc(n - i));

  let bank = makeBankOf(~audioCtx, ~n, ~hasInput=false, ~f=createNode);
  let t = currentTime(audioCtx);
  setValueAtTime(bank.output |. gain_Get, 0.007, t);
  bank;
};

let getAudioSource: audioContext => Js.Promise.t(option(audioNode)) =
  ctx =>
    switch (getAudioStream()) {
    | None => Js.Promise.resolve(None)
    | Some(streamPromise) =>
      streamPromise
      |> Js.Promise.then_(mediaStream =>
           Js.Promise.resolve(
             Some(createMediaStreamSource(ctx, mediaStream)),
           )
         )
      |> Js.Promise.catch(err => {
           Js.log(err);
           Js.Promise.resolve(None);
         })
    };

[@bs.get] external defaultSink : audioContext => audioNode = "destination";

let connectFilterBank = (noise, filterBank, merger, channel) => {
  switch (filterBank.input) {
  | Some(input) => connectNodeToGain(noise, input)
  | None => ()
  };
  connectWithOutputAndInputIndex(filterBank.output, merger, 0, channel);
};

let disconnectFilterBank = (noise, filterBank, merger) => {
  switch (filterBank.input) {
  | Some(input) => disconnect(noise, input)
  | None => ()
  };
  disconnect(filterBank.output, compressor);
};

type filterValues =
  | Mono(array(float))
  | Stereo(array(float), array(float));

type filterBanks =
  | MonoBank(filterBank)
  | StereoBanks(filterBank, filterBank);

let updateBankGains = (~bank: bank('a), ~gainValues: array(float)) => {
  let t = currentTime(bank.audioCtx);
  let n = Array.length(gainValues);
  for (i in 0 to n - 1) {
    let gainI = n - i - 1;
    linearRampToValueAtTime(
      bank.gains[gainI] |. gain_Get,
      gainValues[i],
      t +. 0.05,
    );
  };
};

let updateFilterBank =
    (
      ~inputGain: float=1.0,
      ~outputGain: float=0.1,
      ~filterBank: filterBank,
      ~filterValues: array(float),
    ) => {
  let currentTime = currentTime(filterBank.audioCtx);
  switch (filterBank.input) {
  | Some(input) => setValueAtTime(input |. gain_Get, inputGain, currentTime)
  | None => ()
  };
  setValueAtTime(filterBank.output |. gain_Get, outputGain, currentTime);
  updateBankGains(~bank=filterBank, ~gainValues=filterValues);
};

let updateFilterBankDefinition =
    (~filterBank: filterBank, ~freqFunc: int => float, ~q: float) => {
  Js.log("updating filter bank definitions (costly!)");
  let currentTime = currentTime(filterBank.audioCtx);
  let n = Array.length(filterBank.nodes);
  Array.iteri(
    (i, filter) => {
      setValueAtTime(filter |. qualityFactor, q, currentTime);
      setValueAtTime(filter |. frequency, freqFunc(n - i - 1), currentTime);
    },
    filterBank.nodes,
  );
};

external castFloat32 : Js.Typed_array.Float32Array.t => array(float) =
  "%identity";

external castUint8 : Js.Typed_array.Uint8Array.t => array(int) = "%identity";
