var node = document.querySelector(".elm-container");

//
// Config
var config = JSON.parse(
  document.querySelector(".config").innerHTML
);

//
// Setup Elm app
var app = Elm.Main.embed(
  node,
  { pathToRoot: config.pathToRoot }
);

//
// Ports
app.ports.deflate.subscribe(function(string) {
  app.ports.deflateResult.send(tryAndCatch(deflate, string));
});

app.ports.inflate.subscribe(function(string) {
  app.ports.inflateResult.send(tryAndCatch(inflate, string));
});

app.ports.setDocumentTitle.subscribe(function(string) {
  document.title = string;
});

//
// Functions
function deflate(string) {
  var result = pako.deflate(string, { to: "string" });
  var result_ = btoa(result);
  return encodeURIComponent(result_);
}

function inflate(string) {
  var result = decodeURIComponent(string);
  var result_ = atob(result);
  return pako.inflate(result_, { to: "string" });
}

function tryAndCatch(fn, string) {
  try {
    return fn(string);
  } catch (err) {
    console.error(err);
    return null;
  }
}
