let root2 = sqrt(2.0);
let oneOverRoot2 = 1.0 /. root2;

let eltsWithStepAndOffset =
    (step: int, offset: int, input: array('a))
    : array('a) => {
  let n = Array.length(input);
  Array.init(n / step, i => input[i * step + offset]);
};

let evens = eltsWithStepAndOffset(2, 0);
let odds = eltsWithStepAndOffset(2, 1);

let zipWith = (f, a, b) => Array.mapi((i, x) => f(x, b[i]), a);

let threshold = (input: array(float), epsilon: float) : array(float) =>
  Array.map(x => abs_float(x) < epsilon ? 0.0 : x, input);

let rec haar = (input: array(float)) : array(float) => {
  let n = Array.length(input);
  if (n > 1) {
    let even = evens(input);
    let odd = odds(input);
    let sum = zipWith((a, b) => (a +. b) *. oneOverRoot2, even, odd);
    let diff = zipWith((a, b) => (a -. b) *. oneOverRoot2, even, odd);
    Array.append(haar(sum), diff);
  } else {
    input;
  };
};

let rec inverseHaar = (input: array(float)) : array(float) => {
  let n = Array.length(input);
  if (n > 1) {
    let offset = n / 2;
    let sum =
      Array.map(x => x *. root2, inverseHaar(Array.sub(input, 0, offset)));
    let output = Array.make(n, 0.0);
    for (i in 0 to offset - 1) {
      let diff = input[i + offset] *. root2;
      output[i * 2] = (sum[i] +. diff) /. 2.0;
      output[i * 2 + 1] = (sum[i] -. diff) /. 2.0;
    };
    output;
  } else {
    input;
  };
};

/*
   def fwht(a):
   """In-place Fast Walsh-Hadamard Transform of array a"""
   h = 1
   while h < len(a):
   for i in range(0, len(a), h * 2):
   for j in range(i, i + h):
   x = a[j]
   y = a[j+h]
   a[j] = x + y
   a[j+h] = x - y
   h *= 2

 */

let inplaceFWHT: array(int) => array(int) =
  a => {
    let h = ref(1);
    let n = Array.length(a);
    while (h^ < n) {
      for (i in 0 to n / (h^ * 2) - 1) {
        let i = i * (h^ * 2);
        for (j in i to i + h^ - 1) {
          let x = a[j];
          let y = a[j + h^];
          a[j] = x + y;
          a[j + h^] = x - y;
        };
      };
      h := h^ * 2;
    };
    a;
  };

let log2OfWaveletEpsilon = (-8.0);
let waveletEpsilon = 2.0 ** log2OfWaveletEpsilon;

let signalFloatToUint8: float => int = v => int_of_float(v *. 128.0) + 127;

let waveletCoefToUint8: float => int =
  v => {
    let absV = abs_float(v);

    let signal =
      if (absV < waveletEpsilon) {
        0.0;
      } else {
        let logAbsV = log(absV) /. log(2.0);
        let scaledLogAbsV =
          (logAbsV -. log2OfWaveletEpsilon)
          /. (2.0 *. abs_float(log2OfWaveletEpsilon));
        copysign(scaledLogAbsV, v);
      };
    signalFloatToUint8(signal);
  };
