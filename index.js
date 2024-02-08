import { Elm } from "./src/Main.elm";
import "./favicon.ico"

const storedState = localStorage.getItem("map");

// init elm app
const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: storedState
});


// storeTile listener
app.ports.storeMap.subscribe(function (map) {
    // console.log("store", map)
    localStorage.setItem("map", JSON.stringify(map));
});