module Xhr = {
  type xhr;
  [@bs.new] external make: unit => xhr = "XMLHttpRequest";
  [@bs.send] external open_: (xhr, string, string, bool) => unit = "open";
  [@bs.send] external send: (xhr, Js.Null.t(string)) => unit = "send";
  [@bs.send]
  external setRequestHeader: (xhr, string, string) => unit =
    "setRequestHeader";
  [@bs.send] external abort: xhr => unit = "abort";
  [@bs.set] external setResponseType: (xhr, string) => unit = "responseType";
  [@bs.set]
  external onChange: (xhr, unit => unit) => unit = "onreadystatechange";
  [@bs.get] external readyState: xhr => int = "readyState";
  [@bs.get] external status: xhr => int = "status";
  [@bs.get] external responseText: xhr => string = "responseText";
  [@bs.set]
  external setWithCredentials: (xhr, bool) => unit = "withCredentials";
};

[@bs.deriving jsConverter]
type httpMethod = [ | `GET | `POST | `PATCH | `DELETE];

type error = {
  status: int,
  text: string,
};

let make =
    (
      ~url,
      ~method=`GET,
      ~body=?,
      ~contentType=?,
      ~accept=?,
      ~responseType=?,
      ~withCredentials=false,
      (),
    ) => {
  Future.make(resolve => {
    let xhr = Xhr.make();
    let method = httpMethodToJs(method);
    xhr->Xhr.open_(method, url, true);
    switch (responseType) {
    | Some(responseType) => xhr->Xhr.setResponseType(responseType)
    | None => ()
    };
    switch (accept) {
    | Some(accept) => xhr->Xhr.setRequestHeader("Accept", accept)
    | None => ()
    };
    switch (contentType) {
    | Some(contentType) =>
      xhr->Xhr.setRequestHeader("Content-Type", contentType)
    | None => ()
    };
    xhr->Xhr.setWithCredentials(withCredentials);
    Xhr.onChange(xhr, () =>
      if (Xhr.readyState(xhr) === 4) {
        switch (Xhr.status(xhr)) {
        | status when status >= 200 && status < 300 =>
          resolve(Ok(Xhr.responseText(xhr)))
        | status => resolve(Error({status, text: Xhr.responseText(xhr)}))
        };
      }
    );
    xhr->Xhr.send(body->Js.Null.fromOption);
    Some(() => xhr->Xhr.abort);
  });
};
