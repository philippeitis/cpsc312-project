function urlParam(key, value) {
    return `${encodeURIComponent(key)}=${encodeURIComponent(value)}`;
}

function renderFunction(json) {
    // Renders the function's name and documentation, as well as buttons
    // to delete and save said function.
    const msg = document.getElementById("function_msg");
    msg.innerHTML = "<h2>" + json["msg"] + "</h2>";
    let output = "";
    for (const fn of json["functions"]) {
        output += `<div data-uuid="${fn.uuid}"\>`;
        // function header
        output += `<table><thead><tr><td><h3>${fn.name}</h3></td><td>`;
        output += `<td>&nbsp; &nbsp; <input type="checkbox" style="width: 16px; height: 16px" data-uuid="${fn.uuid}"> </td>`;
        output += `<td>&nbsp; &nbsp; <input type="image" src="trash.svg" style="width:16px;height:16px;" onclick=onClickDelete("${fn.uuid}")></td>`;
        output += "</td></tr></thead></table>";
        // function paragraph
        output += `<p>${fn.docs}</p>`;
        output += "</div>";
    }
    document.getElementById("functions").innerHTML = output;
    msg.scrollIntoView();
}

function mainFormToURL() {
    // Grabs the items from the main form, and correctly formats them for
    // use server-side.
    const form = document
        .getElementById("search_form");

    const oldFormData = new FormData(form);
    const formData = [];

    for (const pair of oldFormData.entries()) {
        if (pair[0] === "inputs" || pair[0] === "outputs") {
            for (const item of pair[1].split(",")) {
                if (!item.trim()) {
                    continue;
                }
                formData.push(urlParam(pair[0], item.trim()));
            }
        } else {
            if (!pair[1]) {
                continue;
            }
            formData.push(urlParam(pair[0], pair[1]));
        }
    }

    return form.action + "?" + formData.join("&");
}

function searchAndDisplay() {
    // When form is submitted, handles process of fetching all information
    // and displaying it.
    const url = mainFormToURL();
    const xmlhttp = new XMLHttpRequest();
    xmlhttp.onreadystatechange = function() {
        if (this.readyState === 4) {
            const json = JSON.parse(this.responseText);
            if (this.status === 200) {
                renderFunction(json);
            }
            if (this.status === 404) {
                const msg = document.getElementById("function_msg");
                msg.innerHTML = "<h2 style='color:red'>" + json["msg"] + "</h2>";
                document.getElementById("functions").innerHTML = "";
                msg.scrollIntoView();

            }
        }
    };

    xmlhttp.open("GET", url, true);
    xmlhttp.send();
}

function removeUuidFromScreen(uuid) {
    // Deletes all instances of a particular UUID
    const elements = document.querySelectorAll(`div[data-uuid="${uuid}"]`);
    Array.prototype.forEach.call(elements, function(node) {
        node.remove();
    });
}

function onClickDelete(uuid) {
    // Remove the provided UUID from the screen and from the server. If the
    // uuid belongs to a specialized function, its parent will be deleted,
    // and all children on the page will be removed.
    removeUuidFromScreen(uuid);
    const xmlhttp = new XMLHttpRequest();
    const url = "/func?uuid=" + uuid;
    xmlhttp.onreadystatechange = function() {
        if (this.readyState === 4) {
            const json = JSON.parse(this.responseText);
            if (this.status === 200) {
                json["uuids"].forEach(removeUuidFromScreen);
            } else if (this.status === 405) {
                onClickDelete(json["parent_uuid"]);
            }
        }
    };

    xmlhttp.open("DELETE", url, true);
    xmlhttp.send();
}

function clearResults() {
    // Clears all areas where results are displayed.
    document.getElementById("function_msg").innerHTML = "";
    document.getElementById("functions").innerHTML = "";
}

function saveResults() {
    // Saves the user's results to a secondary div, without adding duplicate copies of any particular
    // result.
    const functions = document.getElementById("functions");
    const selectedFunctions = functions.querySelectorAll('input[type=checkbox]');
    const savedFunctions = document.getElementById("saved_functions");
    if (!savedFunctions.innerHTML) {
        savedFunctions.innerHTML = "<h2>Saved Functions</h2>";
    }

    Array.from(selectedFunctions)
        .filter((el) => el.checked)
        .map((el) => `div[data-uuid="${el.getAttribute("data-uuid")}"]`)
        .filter((selector) => savedFunctions.querySelectorAll(selector).length === 0)
        .forEach((selector) => {
            const child = document.querySelector(selector)
                .cloneNode(true);
            child.querySelectorAll('input[type=checkbox]').forEach(
                (el) => el.remove()
            );
            savedFunctions.appendChild(child);
        });
}

function toJsonAndPush() {
    // Extracts the items from the Function Add form, formats them as JSON,
    // and sends a post request to the server.
    const form = document.getElementById("add_function_form");
    const formData = new FormData(form);
    const dict = { generics: [] };
    for (const pair of formData.entries()) {
        if (pair[0] === "inputs" || pair[0] === "outputs") {
            dict[pair[0]] = pair[1].split(",").map((s) => s.trim()).filter((s) => s.length);
        } else {
            dict[pair[0]] = pair[1];
        }
    }

    const xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST", "/func",true);
    xmlhttp.setRequestHeader('Content-Type', 'application/json; charset=UTF-8');
    xmlhttp.send(JSON.stringify(dict));
    form.reset();
}