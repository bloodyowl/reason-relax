open Belt;

let explode = (queryString: string) =>
  (
    queryString->Js.String2.slice(~from=0, ~to_=1) == "?"
      ? queryString->Js.String2.sliceToEnd(~from=1) : queryString
  )
  ->Js.String2.split("&")
  ->Array.reduce(
      [],
      (acc, item) => {
        let array = item->Js.String2.split("=");
        switch (array[0], array[1]) {
        | (Some(key), Some(value)) => [
            (
              Js.Global.decodeURIComponent(key),
              Js.Global.decodeURIComponent(value),
            ),
            ...acc,
          ]
        | _ => acc
        };
      },
    )
  ->Js.Dict.fromList;

let set = (qs, key, value) => {
  let qs = qs->Js.Dict.entries->Js.Dict.fromArray;
  qs->Js.Dict.set(key, value);
  qs;
};

let remove = (qs, key) => {
  qs
  ->Js.Dict.entries
  ->Array.keep(((item, _)) => item !== key)
  ->Js.Dict.fromArray;
};

let fromArray = Js.Dict.fromArray;

let implode = map =>
  map
  ->Js.Dict.entries
  ->SortArray.stableSortBy(((a, _), (b, _)) => b > a ? (-1) : 1)
  ->Array.reduceReverse([||], (acc, (key, value)) =>
      Array.concat(
        [|
          Js.Global.encodeURIComponent(key)
          ++ "="
          ++ Js.Global.encodeURIComponent(value),
        |],
        acc,
      )
    )
  ->Js.Array2.joinWith("&");
