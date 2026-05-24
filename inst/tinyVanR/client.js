// Reactive State map for server-pushed updates (Text, HTML, or Props)
const outputs = van.state({});
const getOutputState = (id) => {
    if (!outputs.val[id]) {
        outputs.val = { ...outputs.val, [id]: van.state({ text: "", props: {} }) };
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
    const parts = e.data.split("\t");
    const cmd = parts[0];

    if (cmd === "UI") {
        // Parse and render the R-defined UI
        const uiData = eval(`(${parts[1]})`);
        van.add(document.getElementById("root"), renderNode(uiData));
    } else if (cmd === "OUT") {
        const [_, id, type, val] = parts;
        const state = getOutputState(id);
        
        // Handle Boolean strings from R
        let parsedVal = val;
        if (val === "TRUE") parsedVal = true;
        if (val === "FALSE") parsedVal = false;

        if (type === "text") state.val = { ...state.val, text: parsedVal };
        else if (type.startsWith("prop:")) {
            const propName = type.split(":")[1];
            state.val = { ...state.val, props: { ...state.val.props, [propName]: parsedVal } };
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
        if (typeof props[eventType] === "function") propseventType;
    };

    const extendedProps = {
        ...props,
        [eventType]: handler
    };

    // Reactive properties from server
    const reactiveAttrs = ["disabled", "style", "className", "min", "max", "value"];
    reactiveAttrs.forEach(attr => {
        const propKey = attr === "className" ? "class" : attr;
        extendedProps[attr] = () => {
            const val = state.val.props[propKey] ?? props[attr];
            return val === undefined ? null : val;
        };
    });

    return tagFunc(extendedProps, children, van.derive(() => state.val.text || ""));
};

// Recursive Renderer for any VanJS tag
const renderNode = (node) => {
    if (typeof node === 'string') return node;
    if (Array.isArray(node)) return node.map(renderNode);
    
    const tagFunc = van.tags[node.tag];
    const children = node.children ? renderNode(node.children) : [];
    
    if (node.id) return Bind(node.id, node.tag, tagFunc, node.props, ...children);
    return tagFunc(node.props, ...children);
};
