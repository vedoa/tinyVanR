const { div, button, input, span, hr, h3 } = van.tags;

// Reactive State map for server-pushed updates
const outputs = van.state({});

const getOutputState = (id) => {
    if (!outputs.val[id]) {
        outputs.val = { ...outputs.val, [id]: van.state("") };
    }
    return outputs.val[id];
};

// WebSocket Management
const ws = new WebSocket(`ws://${window.location.host}/ws`);

const sendInput = (id, type, val) => {
    if (ws.readyState === WebSocket.OPEN) {
        ws.send(`IN\t${id}\t${type}\t${val}`);
    }
};

ws.onmessage = (e) => {
    const [cmd, id, type, val] = e.data.split("\t");
    if (cmd === "OUT") {
        getOutputState(id).val = val;
    } else if (cmd === "ERR") {
        console.error("tinyVanR Server Error:", val);
    }
};

// Tiny-lite UI Components
const NumericInput = (id) => input({
    type: "number",
    oninput: (e) => sendInput(id, "num", e.target.value)
});

const ActionButton = (id, label) => button({
    onclick: () => sendInput(id, "click", "1")
}, label);

const TextOutput = (id) => span(getOutputState(id));

// Mount the UI
van.add(document.getElementById("root"), [
    h3("tinyVanR Framework"),
    div("Input Value: ", NumericInput("num_in")),
    div("Calculated (x2): ", TextOutput("num_out")),
    hr(),
    ActionButton("btn_click", "Trigger Server Update"),
    div("Server Response: ", TextOutput("btn_res"))
]);