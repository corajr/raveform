open Jest;
open Wavelet;

let neg1overRoot2 = (-1.0) /. sqrt(2.0);

let l1dist = (xs, ys) =>
  Array.fold_left((+.), 0.0, zipWith((a, b) => abs_float(a -. b), xs, ys));

test("haar", _ =>
  Expect.(
    expect(
      l1dist(
        haar([|1.0, 2.0, 3.0, 4.0|]),
        [|5.0, (-2.0), neg1overRoot2, neg1overRoot2|],
      ),
    )
    |> toBeLessThan(0.01)
  )
);

test("inverseHaar", _ =>
  Expect.(
    expect(
      l1dist(
        [|1.0, 2.0, 3.0, 4.0|],
        inverseHaar(haar([|1.0, 2.0, 3.0, 4.0|])),
      ),
    )
    |> toBeLessThan(0.01)
  )
);
