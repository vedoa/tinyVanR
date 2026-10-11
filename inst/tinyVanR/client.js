// Reactive State map for server-pushed updates (Text, HTML, or Props)
const outputs = van.state({});
const getOutputState = (id) => {
    if (!outputs.val[id]) {
        outputs.val = { ...outputs.val, [id]: van.state({ text: "", props: {} }) };
    }
    return outputs.val[id];
};

const setElementProp = (id, propName, value) => {
    const element = document.getElementById(id);
    if (!element) return;

    if (value === undefined || value === null) {
        element.removeAttribute(propName);
    } else if (propName === "class") {
        element.className = String(value);
    } else if (propName === "style") {
        element.style.cssText = String(value);
    } else if (propName in element) {
        element[propName] = value;
    } else {
        element.setAttribute(propName, String(value));
    }
};
// WebSocket Management
const wsProtocol = window.location.protocol === "https:" ? "wss:" : "ws:";
// run_app() binds to IPv4 loopback by default; avoid localhost resolving to ::1.
const wsHost = window.location.hostname === "localhost"
    ? `127.0.0.1${window.location.port ? `:${window.location.port}` : ""}`
    : window.location.host;
const ws = new WebSocket(`${wsProtocol}//${wsHost}/ws`);
const sendInput = (id, type, val) => {
    if (ws.readyState === WebSocket.OPEN) {
        ws.send(`IN\t${id}\t${type}\t${val}`);
    }
};

ws.onmessage = (e) => {
    const parts = e.data.split("\t");
    const cmd = parts[0];

    if (cmd === "UI") {
        // Parse and render the R-defined UI
        const uiData = eval(`(${parts[1]})`);
        van.add(document.getElementById("root"), renderNode(uiData));
    } else if (cmd === "OUT") {
        const [_, id, type, val] = parts;
        const state = getOutputState(id);
        
        if (!state) {
            console.warn(`Attempted to update unknown element ID: ${id}`);
            return;
        }

        // Handle Boolean strings from R
        let parsedVal = val;
        const upperVal = String(val).toUpperCase();
        if (upperVal === "TRUE" || upperVal === "T") parsedVal = true;
        if (upperVal === "FALSE" || upperVal === "F") parsedVal = false;

        if (type === "text") state.val = { ...state.val, text: parsedVal };
        else if (type.startsWith("prop:")) {
            const propName = type.split(":")[1];
            state.val = { ...state.val, props: { ...state.val.props, [propName]: parsedVal } };
            setElementProp(id, propName, parsedVal);
        }
    } else if (cmd === "ERR") {
        console.error("tinyVanR Server Error:", val);
    }
};

// Generic Managed Component Wrapper
const Bind = (id, tagName, tagFunc, props = {}, ...children) => {
    const state = getOutputState(id);

    // Determine value type and event listener based on tag and props
    const isButton = tagName === "button";
    const isSelect = tagName === "select";
    const isCheckable = props.type === "checkbox" || props.type === "radio";

    // Map HTML behavior to our internal protocol types
    const eventType = isButton ? "onclick" : (isSelect || isCheckable ? "onchange" : "oninput");
    const valType = isButton ? "click" : (isCheckable ? "bool" :
        (props.type === "number" || props.type === "range" ? "num" : "str"));

    const handler = (e) => {
        const val = isButton ? "1" : (isCheckable ? e.target.checked : e.target.value);
        sendInput(id, valType, val);
        if (typeof props[eventType] === "function") props[eventType](e);
    };

    const extendedProps = {
        ...props,
        id,
        [eventType]: handler
    };

    // Bind only attributes with an initial or server-provided value. Binding an
    // absent `src` as undefined makes browsers request the literal `/undefined`.
    const reactiveAttrs = ["disabled", "style", "class", "min", "max", "value", "checked", "placeholder", "src", "href"];

    reactiveAttrs.forEach(attr => {
        const hasInitialValue = props[attr] !== undefined;
        const hasServerValue = Object.prototype.hasOwnProperty.call(state.val.props, attr);

        if (hasInitialValue || hasServerValue) {
            extendedProps[attr] = () => {
                const val = state.val.props[attr] ?? props[attr];
                // Returning undefined tells VanJS to omit the attribute entirely
                return (val === undefined || val === null) ? undefined : val;
            };
        }
    });

    return tagFunc(extendedProps, children, van.derive(() => state.val.text || ""));
};

// Recursive Renderer for any VanJS tag
const renderNode = (node) => {
    if (typeof node === 'string') return node;
    if (Array.isArray(node)) return node.map(renderNode);

    const tagFunc = van.tags[node.tag];
    let children = node.children ? renderNode(node.children) : []; // Ensure children is an array
    
    // Ensure children is always an array for the spread operator
    if (!Array.isArray(children)) {
        children = [children];
    }

    if (node.id) return Bind(node.id, node.tag, tagFunc, node.props, ...children);
    return tagFunc(node.props, ...children);
};
