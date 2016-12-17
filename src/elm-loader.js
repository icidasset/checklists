var node = document.querySelector(".elm-container");
var authTokenKey = "authToken";

// Config
var config = JSON.parse(
  document.querySelector(".config").innerHTML
);

// Setup Elm app
var app = Elm.Main.embed(
  node,
  { pathToRoot: config.pathToRoot }
);

// Ports
app.ports.deflate.subscribe(function(string) {
  var result = pako.deflate(string, { to: "string" });
  var result_ = btoa(result);
  var result__ = encodeURIComponent(result_);
  app.ports.deflateResult.send(result__);
});

app.ports.inflate.subscribe(function(string) {
  var result = decodeURIComponent(string);
  var result_ = atob(result);
  var result__ = inflate(result_);
  app.ports.inflateResult.send(result__);
});

function inflate(string) {
  try { return pako.inflate(string, { to: "string" }); }
  catch (err) { console.error(err); return null; }
}
