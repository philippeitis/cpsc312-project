function initPage() {
    // Initialize form
    const params = (new URL(window.location.href)).searchParams;
    params.forEach((val, key) => {
      if (key !== "inputs" && key !== "outputs") {
        document.getElementById(key).value = val
      }
    });
    document.getElementById("inputs").value = params.getAll("inputs").join(", ");
    document.getElementById("outputs").value = params.getAll("outputs").join(", ");

    document.getElementById("function_msg").scrollIntoView();
    document.getElementById("search_form").addEventListener("submit", searchAndDisplay)
}

function urlParam(key, value) {
    return `${encodeURIComponent(key)}=${encodeURIComponent(value)}`;
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

function searchAndDisplay(event) {
    // When form is submitted, handles process of fetching all information
    // and displaying it.
    const form = document.getElementById("search_form");
    event.preventDefault();
    event.stopPropagation();

    window.location.replace(mainFormToURL());
    return false;
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