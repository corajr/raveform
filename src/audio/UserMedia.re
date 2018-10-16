type mediaStream;

[@bs.deriving abstract]
type constraints = {
  audio: bool,
  video: bool,
};

let _getStream: constraints => Js.Nullable.t(Js.Promise.t(mediaStream)) = [%bs.raw
  constraints => {|
     if (navigator.mediaDevices.getUserMedia) {
       console.log('getUserMedia supported.');
       return navigator.mediaDevices.getUserMedia(constraints);
     } else {
       console.log('getUserMedia not supported on your browser!');
       return null;
     }
     |}
];

let getAudioVisualStream: unit => option(Js.Promise.t(mediaStream)) =
  () =>
    Js.Nullable.toOption(_getStream(constraints(~audio=true, ~video=true)));

let getAudioStream: unit => option(Js.Promise.t(mediaStream)) =
  () =>
    Js.Nullable.toOption(
      _getStream(constraints(~audio=true, ~video=false)),
    );

let getVideoStream: unit => option(Js.Promise.t(mediaStream)) =
  () =>
    Js.Nullable.toOption(
      _getStream(constraints(~audio=false, ~video=true)),
    );
