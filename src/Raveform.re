open Audio;
open AudioGraph;
open Wavelet;

type state = {
  analyser: ref(option(analyser)),
  timeData: ref(option(Js.Typed_array.Uint8Array.t)),
  freqData: ref(option(Js.Typed_array.Uint8Array.t)),
  imageData: ref(option(ImageRe.t)),
  animCallbackRef: ref(option((Dom.element, float) => unit)),
};

let animCallback =
    (
      {ReasonReact.state},
      ~canvasWidth,
      ~canvasHeight,
      ~clearAlpha=None,
      ~edgeAlign=false,
      ~writeAlpha=1.0,
      ~desiredFps,
      ~waveletAlpha=1.0,
      canvas,
      t,
    ) =>
  if (mod_float(t, 1000.0 /. desiredFps) >= 33.0) {
    ();
  } else {
    open Webapi.Canvas;
    open Canvas2dRe;
    let ctx = CanvasElement.getContext2d(canvas);
    let width = float_of_int(canvasWidth);
    let height = float_of_int(canvasHeight);
    let edgeThreshold = 5;
    let risingEdge = ref(0);

    switch (
      state.analyser^,
      state.timeData^,
      state.freqData^,
      state.imageData^,
    ) {
    | (Some(analyser), Some(timeData), Some(freqData), Some(imageData)) =>
      switch (clearAlpha) {
      | Some(clearAlpha) =>
        globalCompositeOperation(ctx, Composite.sourceOver);
        setFillStyle(ctx, String, {j|rgba(0,0,0,$(clearAlpha))|j});
      | None => ()
      };
      fillRect(ctx, ~x=0.0, ~y=0.0, ~w=width, ~h=height);
      getByteTimeDomainData(analyser, timeData);
      getByteFrequencyData(analyser, freqData);
      let n = Js.Typed_array.Uint8Array.length(timeData);
      let freqData = castUint8(freqData);
      let timeData = castUint8(timeData);
      let timeDataFloat =
        Array.map(x => float_of_int(x) /. 128.0 -. 1.0, timeData);
      let waveletData = haar(timeDataFloat);
      let sliceHeight = height /. float_of_int(n);
      let sliceWidth = width /. float_of_int(n);

      globalAlpha(ctx, writeAlpha);
      globalCompositeOperation(ctx, Composite.lighter);

      setFillStyle(ctx, String, "red");
      for (i in 0 to n - 1) {
        let rawV = float_of_int(freqData[i]) /. 256.0;
        let v = log1p(rawV) /. log(2.0);
        let rectWidth = v *. width;
        fillRect(
          ctx,
          ~x=0.0,
          ~y=float_of_int(i) *. sliceHeight,
          ~w=rectWidth,
          ~h=sliceHeight,
        );
      };

      let logHeight = log(height) /. log(2.0);
      let levelsFloat = log(float_of_int(n)) /. log(2.0);
      let levels = int_of_float(levelsFloat);
      for (i in 0 to levels - 1) {
        let power = 2.0 ** float_of_int(i);
        let levelOffset = int_of_float(power);
        let levelWidth = width /. power;
        let levelHeight = height /. levelsFloat;
        let y = levelHeight *. float_of_int(i);
        for (j in 0 to levelOffset - 1) {
          let x = float_of_int(j) *. levelWidth;
          let v = waveletCoefToUint8(waveletData[levelOffset + j]);
          setFillStyle(
            ctx,
            String,
            "rgba(0,0,"
            ++ Js.Int.toString(v)
            ++ ","
            ++ Js.Float.toString(waveletAlpha)
            ++ ")",
          );
          fillRect(ctx, ~x, ~y, ~w=levelWidth, ~h=levelHeight);
        };
      };

      if (edgeAlign) {
        while (risingEdge^ < n && timeData[risingEdge^] - 128 > 0) {
          risingEdge := risingEdge^ + 1;
        };

        if (risingEdge^ === n) {
          risingEdge := 0;
        };

        while (risingEdge^ < n && timeData[risingEdge^] - 128 < edgeThreshold) {
          risingEdge := risingEdge^ + 1;
        };

        if (risingEdge^ === n) {
          risingEdge := 0;
        };
      };

      setStrokeStyle(ctx, String, "green");
      beginPath(ctx);
      for (i in risingEdge^ to risingEdge^ + n - 1) {
        let wrappedI = i >= n ? i - n : i;
        let v = float_of_int(timeData[wrappedI]) /. 256.0;
        let x = float_of_int(i - risingEdge^) *. sliceWidth;
        let y = height -. v *. height;
        lineTo(ctx, ~x, ~y);
      };
      stroke(ctx);

    | (Some(analyser), None, None, None) =>
      state.timeData :=
        Some(
          Js.Typed_array.Uint8Array.fromLength(
            frequencyBinCountGet(analyser),
          ),
        );
      state.freqData :=
        Some(
          Js.Typed_array.Uint8Array.fromLength(
            frequencyBinCountGet(analyser),
          ),
        );

      state.imageData := Some(createImageDataCoords(ctx, 256.0, 256.0));
    | _ => ()
    };
  };

let component = ReasonReact.reducerComponent(__MODULE__);

let make =
    (
      ~audioCtx,
      ~audioGraph,
      ~width,
      ~height,
      ~nodeKey="defaultAnalyser",
      _children,
    ) => {
  ...component,
  initialState: () => {
    analyser: ref(None),
    timeData: ref(None),
    freqData: ref(None),
    imageData: ref(None),
    animCallbackRef: ref(None),
  },
  reducer: ((), _state) => ReasonReact.NoUpdate,
  didMount: self => {
    let analyser = makeAnalyser(~fftSize=4096, audioCtx);
    self.state.analyser := Some(analyser);
    audioGraph :=
      audioGraph^
      |> addNode((nodeKey, unwrapAnalyser(analyser)))
      |> addEdge((nodeKey, "sink", 0, 0))
      |> updateConnections;

    self.state.animCallbackRef :=
      Some(
        animCallback(
          self,
          ~canvasWidth=width,
          ~canvasHeight=height,
          ~clearAlpha=Some(1.0),
          ~writeAlpha=1.0,
          ~desiredFps=60.0,
          ~edgeAlign=false,
        ),
      );
  },
  render: self =>
    <RafCanvas width height animCallbackRef=self.state.animCallbackRef />,
};
