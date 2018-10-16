module type AdtSelect = {
  type t;

  let default: t;

  let options: array(t);

  let toString: t => string;
  let fromString: string => t;
};

module Make = (Adt: AdtSelect) => {
  include Adt;

  let component = ReasonReact.statelessComponent("OscillatorTypeSelect");

  let make = (~onChange, ~currentType=Adt.default, _children) => {
    ...component,
    render: self =>
      <select
        value=(Adt.toString(currentType))
        onChange=(
          evt => {
            let s = ReactEvent.Form.target(evt)##value;
            let v = Adt.fromString(s);
            onChange(v);
          }
        )>
        (
          Adt.options
          |> Array.map(x => {
               let s = Adt.toString(x);
               <option key=s value=s> (ReasonReact.string(s)) </option>;
             })
          |> ReasonReact.array
        )
      </select>,
  };
};
