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
