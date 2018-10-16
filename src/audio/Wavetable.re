open Audio;
open AudioGraph;
open Wavelet;

type wavetable =
  | Function(float => float, int)
  | Samples(array(float));

let makeBuffer =
    (audioCtx: audioContext, sampleData: array(float))
    : audioBuffer => {
  let sampleRate = sampleRateGet(audioCtx);
  let samples = Array.length(sampleData);
  let buffer = createBuffer(audioCtx, 1, samples, int_of_float(sampleRate));
  let data = castFloat32(getChannelData(buffer, 0));
  for (i in 0 to samples - 1) {
    data[i] = sampleData[i];
  };
  buffer;
};

let generateSamplesFromFunction = (f: float => float, samples: int) =>
  Array.init(
    samples,
    i => {
      let t = float_of_int(i) /. float_of_int(samples);
      f(t);
    },
  );

let makeBufferFromFunction =
    (audioCtx: audioContext, f: float => float, samples: int) => {
  let sampleData = generateSamplesFromFunction(f, samples);
  makeBuffer(audioCtx, sampleData);
};

let sineF = t => sin(2.0 *. Js.Math._PI *. t);
let squareF = t => copysign(1.0, sineF(t));
let sawF = t => 2.0 *. (t -. floor(0.5 +. t));
let triangleF = t => 2.0 *. abs_float(sawF(t)) -. 1.0;

let fractal1d = (seed, generations) => {
  let n = List.length(seed);
  let output = ref([|1.0|]);

  for (i in 0 to generations - 1) {
    output :=
      Array.concat(List.map(v => Array.map(x => v *. x, output^), seed));
  };
  output^;
};

let sineSamples = generateSamplesFromFunction(sineF, 512);
let squareSamples = generateSamplesFromFunction(squareF, 512);
let triangleSamples = generateSamplesFromFunction(triangleF, 512);
let sawSamples = generateSamplesFromFunction(sawF, 512);

let fromText: string => array(float) =
  s => {
    let n = String.length(s);
    Array.init(n, i => float_of_int(Char.code(s.[i])) /. 128.0 -. 1.0);
  };

let zeroPad = (x, length) => {
  let out = Array.make(length, 0.0);
  let len = min(Array.length(x) - 1, length - 1);
  Array.blit(x, 1, out, 1, len);
  out;
};

let cycle = (x, length) => {
  let n = Array.length(x);
  Array.init(length, i => x[i mod n]);
};

/*
 let spread = (x, length) => {
   let n = Array.length(x);
   let step = length / n;
   Array.init(length, i => i mod step == 0 ? x[i / step] : 0.0);
 };
    */

let swapDiff = (x, y) => {
  let n = Array.length(x);
  let offset = n / 2;
  let y = cycle(y, offset);
  Array.blit(y, 0, x, offset, offset);
  x;
};

let component = ReasonReact.statelessComponent(__MODULE__);

let make =
    (
      ~audioCtx,
      ~audioGraph,
      ~key="defaultWavetable",
      ~output="defaultAnalyser",
      ~playbackRate=1.0,
      /* ~wavetable=Function(triangleF, 512), */
      ~wavetable=Samples(sawSamples),
      /* ~wavetable=Samples( */
      /*              inverseHaar( */
      /*                swapDiff(haar(sineSamples), fromText("no")), */
      /*              ), */
      /*            ), */
      /* ~wavetable=Samples(inverseHaar(zeroPad(fromText("hi"), 512))), */
      /* ~wavetable=Samples( */
      /*              fractal1d( */
      /*                Array.to_list(generateSamplesFromFunction(sawF, 7)), */
      /*                5, */
      /*              ), */
      /*            ), */
      _children,
    ) => {
  ...component,
  didMount: self => {
    let buffer =
      switch (wavetable) {
      | Function(f, samples) => makeBufferFromFunction(audioCtx, f, samples)
      | Samples(data) => makeBuffer(audioCtx, data)
      };
    let bufferSource = createBufferSource(audioCtx);
    bufferSet(bufferSource, buffer);
    loopSet(bufferSource, true);
    setValue(playbackRateGet(bufferSource), playbackRate);

    let gain = createGain(audioCtx);
    setValue(gain_Get(gain), 0.5);
    connect(bufferSource, gain);
    startAudioBufferSourceNode(bufferSource);

    audioGraph :=
      audioGraph^
      |> addNode((key, unwrapGain(gain)))
      |> addEdge((key, output, 0, 0))
      |> addEdge((key, "sink", 0, 0))
      |> updateConnections;
  },
  render: self => <div />,
};
